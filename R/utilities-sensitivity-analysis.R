#' @title runSensitivity
#' @description Determine whether to run SA for individual or population.  If for individual,  pass simulation to individualSensitivityAnalysis.
#' If SA is for population, loop thru population file, extract parameters for each individual, and pass them to individualSensitivityAnalysis.
#' @param simFilePath path to simulation file
#' @param variableParameterPaths paths to parameters to vary in sensitivity analysis
#' @param popFilePath path to the population data file
#' @param individualId ID of individual in population data file for whom to perform sensitivity analysis
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return SA results for individual or population
#' @export
#' @import ospsuite
runSensitivity <- function(simFilePath,
                           variableParameterPaths = NULL,
                           popFilePath = NULL,
                           individualId = NULL,
                           variationRange,
                           numberOfCores = 1,
                           resultsFileFolder,
                           resultsFileName = "sensitivityAnalysisResults",
                           logFolder = getwd()) {
  sim <- ospsuite::loadSimulation(simFilePath)

  allVariableParameterPaths <- ospsuite::potentialVariableParameterPathsFor(simulation = sim)

  # If no parameters to vary specified, vary all parameters valid for sensitivity analysis
  if (is.null(variableParameterPaths)) {
    variableParameterPaths <- allVariableParameterPaths
  } else {
    # if a variableParameterPaths input is provided, ensure that all
    # its elements exist within allVariableParameterPaths.  If not, give an error.
    validateIsIncluded(variableParameterPaths, allVariableParameterPaths)
  }
  totalNumberParameters <- length(variableParameterPaths)
  # In case there are more cores specified in numberOfCores than
  # there are parameters, ensure at least one parameter per spawned core
  numberOfCores <- min(numberOfCores, totalNumberParameters)
  if (totalNumberParameters == 0) {
    logErrorThenStop(messages$errorNoParametersForSensitivityAnalysis(), logFolderPath = logFolder)
  }

  # If numberOfCores > 1 then spawn cores for later use.
  # Otherwise sensitivity analysis will be run on master core only.
  if (numberOfCores > 1) {
    Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)
    Rmpi::mpi.remote.exec(library("ospsuite"))
    Rmpi::mpi.remote.exec(library("ospsuite.reportingengine"))
    Rmpi::mpi.bcast.Robj2slave(obj = simFilePath)
    Rmpi::mpi.bcast.Robj2slave(obj = resultsFileFolder)
    Rmpi::mpi.bcast.Robj2slave(obj = variationRange)
    # Load simulation on each core
    Rmpi::mpi.remote.exec(sim <- loadSimulation(simFilePath))
  }

  # If there is a population file and individualId then for each individual perform SA
  # If there is a population file and no individualId then do SA for entire population
  # If there is no population file and individualId then do SA for mean model
  # If there is no population file and no individualId then do SA for mean model.
  if (!is.null(popFilePath)) { # Determine if SA is to be done for a single individual or more
    popObject <- loadPopulation(popFilePath)
    individualSeq <- individualId %||% seq(1, popObject$count)
    allResultsFileNames <- NULL
    for (ind in individualSeq) {
      logWorkflow(
        message = paste("Starting sensitivity analysis for individual", ind),
        pathFolder = logFolder
      )
      resFile <- individualSensitivityAnalysis(
        simFilePath = simFilePath,
        variableParameterPaths = variableParameterPaths,
        individualParameters = popObject$getParameterValuesForIndividual(individualId = ind),
        variationRange = variationRange,
        numberOfCores = numberOfCores,
        resultsFileFolder = resultsFileFolder,
        resultsFileName = getIndividualSAResultsFileName(ind, resultsFileName)
      )
      allResultsFileNames <- c(allResultsFileNames, resFile)
    }
  }
  else {
    allResultsFileNames <- individualSensitivityAnalysis(
      simFilePath = simFilePath,
      variableParameterPaths = variableParameterPaths,
      individualParameters = NULL,
      variationRange = variationRange,
      numberOfCores = numberOfCores,
      resultsFileFolder = resultsFileFolder,
      resultsFileName = resultsFileName
    )
  }

  # If numberOfCores > 1 then close cores spawned earlier.
  if (numberOfCores > 1) {
    Rmpi::mpi.close.Rslaves()
  }
  return(allResultsFileNames)
}


#' @title individualSensitivityAnalysis
#' @description Run SA for an individual, possibly after modifying the simulation using individualParameters.
#' Determine whether to run SA for on single core or in parallel.
#' If on single core, pass simulation to analyzeCoreSensitivity.
#' If in parallel, pass simulation to runParallelSensitivityAnalysis.
#' @param simFilePath path to simulation file
#' @param variableParameterPaths paths to parameters to vary in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained from a
#' population object's getParameterValuesForIndividual() function.
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return allResultsFileNames, the paths to CSV files containing results of sensitivity analysis
#' @import ospsuite
individualSensitivityAnalysis <- function(simFilePath,
                                          variableParameterPaths = NULL,
                                          individualParameters,
                                          variationRange,
                                          numberOfCores = 1,
                                          resultsFileFolder = resultsFileFolder,
                                          resultsFileName = resultsFileName,
                                          logFolder = getwd()) {


  # Determine if SA is to be done on a single core or more
  if (numberOfCores > 1) {
    allResultsFileNames <- runParallelSensitivityAnalysis(
      simFilePath = simFilePath,
      variableParameterPaths = variableParameterPaths,
      individualParameters = individualParameters,
      variationRange = variationRange,
      numberOfCores = numberOfCores,
      resultsFileFolder = resultsFileFolder,
      resultsFileName = resultsFileName,
      logFolder = logFolder
    )
  } else {
    # No parallelization
    # Load simulation to determine number of parameters valid for sensitivity analysis
    sim <- loadSimulation(simFilePath)
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    allResultsFileNames <- file.path(resultsFileFolder, paste0(resultsFileName, ".csv"))
    analyzeCoreSensitivity(
      simulation = sim,
      variableParameterPaths = variableParameterPaths,
      variationRange = variationRange,
      resultsFilePath = allResultsFileNames
    )
  }
  return(allResultsFileNames)
}


#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores
#' for a single individual, save results as CSV.
#' @param variableParameterPaths paths to parameters to vary in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained
#' from a population object's getParameterValuesForIndividual() function.
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return Simulation results for population
#' @import ospsuite
runParallelSensitivityAnalysis <- function(simFilePath,
                                           variableParameterPaths,
                                           individualParameters,
                                           variationRange,
                                           numberOfCores,
                                           resultsFileFolder,
                                           resultsFileName,
                                           logFolder = getwd()) {
  totalNumberParameters <- length(variableParameterPaths)

  # Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  # Create a vector, of length totalNumberParameters, consisting of a repeating
  # sequence of integers from 1 to numberOfCores
  seqVec <- (1 + ((1:totalNumberParameters) %% numberOfCores))

  # Sort seqVec to obtain an concatenated array of repeated integers,
  # with the repeated integers ranging from from 1 to numberOfCores.
  # These are the core numbers to which each parameter will be assigned.
  sortVec <- sort(seqVec)

  # Split the parameters of the model according to sortVec
  listSplitParameters <- split(x = variableParameterPaths, sortVec)
  tempLogFileNamePrefix <- file.path(logFolder, "logDebug-core-sensitivity-analysis")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores))

  # Generate a listcontaining names of SA CSV result files that will be output by each core
  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores, folderName = resultsFileFolder, fileName = resultsFileName)
  logWorkflow(message = "Starting sending of parameters to cores", pathFolder = logFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = listSplitParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNamePrefix)
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)
  logWorkflow(message = "Sending of parameters to cores completed", pathFolder = logFolder)

  # Update simulation with individual parameters
  logWorkflow(message = "Updating individual parameters on cores.", pathFolder = logFolder)
  Rmpi::mpi.remote.exec(updateSimulationIndividualParameters(simulation = sim, individualParameters))

  logWorkflow(message = "Starting analyzeCoreSensitivity function.", pathFolder = logFolder)
  Rmpi::mpi.remote.exec(analyzeCoreSensitivity(
    simulation = sim,
    variableParameterPaths = listSplitParameters[[mpi.comm.rank()]],
    variationRange = variationRange,
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    numberOfCoresToUse = 1, # Number of local cores, set to 1 when parallelizing.
    debugLogFileName = paste0(tempLogFileNamePrefix, mpi.comm.rank()),
    nodeName = paste("Core", mpi.comm.rank())
  ))
  for (core in seq(1, numberOfCores)) {
    logWorkflow(message = readLines(tempLogFileNames[core]), pathFolder = logFolder)
    file.remove(tempLogFileNames[core])
  }

  allSAResults <- importSensitivityAnalysisResultsFromCSV(simulation = loadSimulation(simFilePath), filePaths = allResultsFileNames)
  combinedFilePath <- file.path(resultsFileFolder, paste0(resultsFileName, ".csv"))
  exportSensitivityAnalysisResultsToCSV(results = allSAResults, filePath = combinedFilePath)
  file.remove(allResultsFileNames)
  return(combinedFilePath)
}

#' @title analyzeCoreSensitivity
#' @description Run a sensitivity analysis for a single individual,
#' varying only the set of parameters variableParameterPaths
#' @param simulation simulation class object
#' @param variableParameterPaths paths of parameters to be analyzed
#' @param variationRange variation range for sensitivity analysis
#' @param resultsFilePath Path to file storing results of sensitivity analysis
#' @param numberOfCoresToUse Number of cores to use on local node.  This parameter
#' should be should be set to 1 when parallelizing over many nodes.
#' @return Save sensitivity analysis results as CSV in path given by resultsFilePath.
#' @import ospsuite
#' @export
analyzeCoreSensitivity <- function(simulation,
                                   variableParameterPaths = NULL,
                                   variationRange = 0.1,
                                   resultsFilePath = paste0(getwd(), "sensitivityAnalysisResults.csv"),
                                   numberOfCoresToUse = NULL,
                                   debugLogFileName = file.path(getwd(), defaultFileNames$logDebugFile()),
                                   infoLogFileName = file.path(getwd(), defaultFileNames$logInfoFile()),
                                   errorLogFileName = file.path(getwd(), defaultFileNames$logErrorFile()),
                                   nodeName = NULL) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation, variationRange = variationRange)
  sensitivityAnalysis$addParameterPaths(variableParameterPaths)
  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
    showProgress = FALSE,
    numberOfCoresToUse = numberOfCoresToUse
  )

  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Starting sensitivity analysis for path(s) ", paste(variableParameterPaths, collapse = ", ")), file = debugLogFileName, printConsole = FALSE)
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Sensitivity analysis for current path(s) completed"), file = debugLogFileName, printConsole = FALSE)
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Starting CSV export of sensitivity analysis results"), file = debugLogFileName, printConsole = FALSE)
  exportSensitivityAnalysisResultsToCSV(results = sensitivityAnalysisResults, resultsFilePath)
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "CSV export of sensitivity analysis results completed"), file = debugLogFileName, printConsole = FALSE)
}



#' @title getPKResultsDataFrame
#' @description Read PK parameter results into a dataframe and set QuantityPath,Parameter and Unit columns as factors
#' @param pkParameterResultsFilePath Path to PK parameter results CSV file
#' @return pkResultsDataFrame, a dataframe storing the contents of the CSV file with path pkParameterResultsFilePath
#' @import ospsuite
getPKResultsDataFrame <- function(pkParameterResultsFilePath, pkParameterSelection) {
  pkResultsDataFrame <- read.csv(pkParameterResultsFilePath, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(pkParameterSelection)) {
    pkResultsDataFrame <- pkResultsDataFrame[pkResultsDataFrame$Parameter %in% pkParameterSelection, ]
  }
  colnames(pkResultsDataFrame) <- c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")
  pkResultsDataFrame$QuantityPath <- as.factor(pkResultsDataFrame$QuantityPath)
  pkResultsDataFrame$Parameter <- as.factor(pkResultsDataFrame$Parameter)
  pkResultsDataFrame$Unit <- as.factor(pkResultsDataFrame$Unit)
  return(pkResultsDataFrame)
}




#' @title getQuantileIndividualIds
#' @description Find IDs of individuals whose PK analysis results closest toquantiles given
#' by vector of quantiles quantileVec
#' @param pkAnalysisResultsDataframe Dataframe storing the PK analysis results for multiple
#' individuals for a single PK parameter and single output path
#' @return ids, IDs of individuals whose PK analysis results closest to quantiles given by vector of quantiles quantileVec
getQuantileIndividualIds <- function(pkAnalysisResultsDataframe, quantileVec) {
  rowNums <- NULL
  for (i in 1:length(quantileVec)) {
    rowNums[i] <- which.min(abs(pkAnalysisResultsDataframe$Value - quantile(pkAnalysisResultsDataframe$Value, quantileVec[i], na.rm = TRUE)))
  }
  ids <- as.numeric(pkAnalysisResultsDataframe$IndividualId[rowNums])
  values <- pkAnalysisResultsDataframe$Value[rowNums]
  units <- as.character(pkAnalysisResultsDataframe$Unit[rowNums])
  quantileResults <- list(ids = ids, values = values, units = units)
  return(quantileResults)
}

#' @title runPopulationSensitivityAnalysis
#' @param simFilePath path to simulation file
#' @param popDataFilePath path to population file
#' @param pkParameterResultsFilePath path to pk parameter results CSV
#' @param resultsFileFolder folder where results of sensitivity analysis will be saved
#' @param popSAResultsIndexFile Names of CSV file containing index of sensitivity analysis CSV files
#' @param variationRange variation range for sensitivity analysis
#' @param resultsFileName root name of sensitivity analysis results CSV files
#' @param quantileVec vector of quantiles in (0,1).  For each output and pk parameter,
#' there will be a distribution of pk parameter values for the population.
#' The individuals yielding pk parameters closest to these quantiles will be selected for sensitivity analysis.
#' @param numberOfCores the number of cores to be used for parallelization of the sensitivity analysis.
#' Default is 1 core (no parallelization).
#' @export
runPopulationSensitivityAnalysis <- function(simFilePath,
                                             variableParameterPaths = NULL,
                                             popDataFilePath,
                                             pkParameterResultsFilePath,
                                             pkParameterSelection = NULL,
                                             resultsFileFolder,
                                             resultsFileName,
                                             popSAResultsIndexFile = "sensitivityAnalysesResultsIndexFile",
                                             variationRange,
                                             quantileVec,
                                             numberOfCores,
                                             logFolder = getwd()) {
  sensitivityAnalysesResultsIndexFileDF <- getSAFileIndex(pkParameterResultsFilePath, pkParameterSelection, quantileVec, resultsFileFolder, resultsFileName, popSAResultsIndexFile)
  ids <- unique(sensitivityAnalysesResultsIndexFileDF$IndividualId)
  allResultsFileNames <- runSensitivity(simFilePath,
    variableParameterPaths = variableParameterPaths,
    popFilePath = popDataFilePath,
    individualId = ids,
    variationRange = variationRange,
    numberOfCores = numberOfCores,
    resultsFileFolder = resultsFileFolder,
    resultsFileName = resultsFileName,
    logFolder = logFolder
  )
  return(allResultsFileNames)
}


#' @title getSAFileIndex
#' @description Function to build and write to CSV a dataframe that stores all
#' sensitivity analysis result files that will be output by a population sensitivity analysis.
#' @param pkParameterResultsFilePath path to pk parameter results CSV file
#' @param quantileVec quantiles of distributions of pk parameter results for a population.
#' The individual in the population that yields a pk parameter closest to the quantile is selected for sensitivity analysis.
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param popSAResultsIndexFile name of CSV file that will store index that identifies name of
#' sensitivity analysis results file for each sensitivity analysis run
getSAFileIndex <- function(pkParameterResultsFilePath,
                           pkParameterSelection,
                           quantileVec,
                           resultsFileFolder,
                           resultsFileName,
                           popSAResultsIndexFile) {
  allPKResultsDataframe <- getPKResultsDataFrame(pkParameterResultsFilePath, pkParameterSelection)
  outputs <- levels(allPKResultsDataframe$QuantityPath)
  pkParameters <- levels(allPKResultsDataframe$Parameter)
  outputColumn <- NULL
  pkParameterColumn <- NULL
  individualIdColumn <- NULL
  valuesColumn <- NULL
  unitsColumn <- NULL
  quantileColumn <- NULL
  for (output in outputs) {
    for (pkParameter in pkParameters) {
      singleOuputSinglePKDataframe <- allPKResultsDataframe[ allPKResultsDataframe["QuantityPath"] == output & allPKResultsDataframe["Parameter"] == pkParameter, ]
      quantileResults <- getQuantileIndividualIds(singleOuputSinglePKDataframe, quantileVec)
      for (i in 1:length(quantileResults$ids)) {
        outputColumn <- c(outputColumn, output)
        pkParameterColumn <- c(pkParameterColumn, pkParameter)
        individualIdColumn <- c(individualIdColumn, quantileResults$ids[i])
        valuesColumn <- c(valuesColumn, quantileResults$values[i])
        unitsColumn <- c(unitsColumn, quantileResults$units[i])
        quantileColumn <- c(quantileColumn, quantileVec[i])
      }
    }
  }
  filenamesColumn <- paste0(sapply(X = individualIdColumn, FUN = getIndividualSAResultsFileName, resultsFileName), ".csv")
  sensitivityAnalysesResultsIndexFileDF <- data.frame("Outputs" = outputColumn, "pkParameters" = pkParameterColumn, "Quantile" = quantileColumn, "Value" = valuesColumn, "Unit" = unitsColumn, "IndividualId" = individualIdColumn, "Filename" = filenamesColumn)
  write.csv(x = sensitivityAnalysesResultsIndexFileDF, file = file.path(resultsFileFolder, paste0(popSAResultsIndexFile, ".csv")))
  return(sensitivityAnalysesResultsIndexFileDF)
}


#' @title getIndividualSAResultsFileName
#' @description Function to build name of inidividual SA results file
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param individualId id of individual
getIndividualSAResultsFileName <- function(individualId, resultsFileName) {
  return(paste(resultsFileName, "IndividualId", individualId, sep = "-"))
}

#' @title defaultVariationRange
#' @description default parameter variation range for sensitivity analysis
#' @export
defaultVariationRange <- 0.1


#' @title defaultSensitivityAnalysisNumberOfCores
#' @description default numberOfCores for sensitivity analysis
#' @export
defaultSensitivityAnalysisNumberOfCores <- 1


#' @title defaultQuantileVec
#' @description default quantiles for population sensitivity analysis
#' @export
defaultQuantileVec <- c(0.05, 0.5, 0.95)
