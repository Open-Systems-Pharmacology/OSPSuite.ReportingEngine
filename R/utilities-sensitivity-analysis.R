#' @title analyzeSensitivity
#' @description Determine whether to run SA for individual or population.  If for individual,  pass simulation to individualSensitivityAnalysis.
#' If SA is for population, loop thru population file, extract parameters for each individual, and pass them to individualSensitivityAnalysis.
#' @param simFilePath path to simulation file
#' @param parametersToPerturb paths to parameters to perturb in sensitivity analysis
#' @param popFilePath path to the population data file
#' @param individualID ID of individual in population data file for whom to perform sensitivity analysis
#' @param numberOfCores number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return SA results for individual or population
#' @export
#' @import ospsuite
analyzeSensitivity <- function(simFilePath,
                               parametersToPerturb = NULL,
                               popFilePath = NULL,
                               individualID = NULL,
                               numberOfCores = 1,
                               resultsFileFolder = "getwd()",
                               resultsFileName = "sensitivityAnalysisResults") {


  # If there is a population file and individualID then for each individual perform SA
  # If there is a population file and no individualID then do SA for entire population
  # If there is no population file and individualID then do SA for mean model
  # If there is no population file and no individualID then do SA for mean model.



  # Determine if SA is to be done for a single individual or more
  if (!is.null(popFilePath)) {
    popObject <- loadPopulation(popFilePath)
    individualSeq <- individualID %||% seq(1, popObject$count)
    allResultsFileNames <- NULL
    for (ind in individualSeq) {
      resFile <- individualSensitivityAnalysis(
        simFilePath = simFilePath,
        parametersToPerturb = parametersToPerturb,
        individualParameters = popObject$getParameterValuesForIndividual(individualId = ind),
        numberOfCores = numberOfCores,
        resultsFileFolder = resultsFileFolder,
        resultsFileName = getIndividualSAResultsFileName(ind,resultsFileName)
      )
      allResultsFileNames <- c(allResultsFileNames, resFile)
    }
  }
  else {
    allResultsFileNames <- individualSensitivityAnalysis(
      simFilePath = simFilePath,
      parametersToPerturb = parametersToPerturb,
      individualParameters = NULL,
      numberOfCores = numberOfCores,
      resultsFileFolder = resultsFileFolder,
      resultsFileName = resultsFileName
    )
  }
  return(allResultsFileNames)
}


#' @title individualSensitivityAnalysis
#' @description Run SA for an individual, possibly after modifying the simulation using individualParameters.  Determine whether to run SA for on single core or in parallel.
#' If on single core, pass simulation to analyzeCoreSensitivity.  If in parallel, pass simulation to runParallelSensitivityAnalysis.
#' @param simFilePath path to simulation file
#' @param parametersToPerturb paths to parameters to perturb in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained from a population object's getParameterValuesForIndividual() function.
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return allResultsFileNames, the paths to CSV files containing results of sensitivity analysis
#' @export
#' @import ospsuite
individualSensitivityAnalysis <- function(simFilePath,
                                          parametersToPerturb = NULL,
                                          individualParameters,
                                          numberOfCores = 1,
                                          resultsFileFolder = resultsFileFolder,
                                          resultsFileName = resultsFileName) {
  # Load simulation to determine number of perturbation parameters
  sim <- loadSimulation(simFilePath)

  # If no perturbation parameters specified, perturb all parameters
  if (is.null(parametersToPerturb)) {
    parametersToPerturb <- ospsuite::potentialVariableParameterPathsFor(simulation = sim)
  }
  totalNumberParameters <- length(parametersToPerturb)

  # In case there are more cores specified in numberOfCores than there are parameters, ensure at least one parameter per spawned core
  numberOfCores <- min(numberOfCores, totalNumberParameters)
  if (totalNumberParameters == 0) {
    stop("No variable parameters found for sensitivity analysis.")
  }

  # Determine if SA is to be done on a single core or more
  if (numberOfCores > 1) {
    allResultsFileNames <- runParallelSensitivityAnalysis(
      simFilePath,
      parametersToPerturb,
      individualParameters,
      numberOfCores,
      resultsFileFolder,
      resultsFileName
    )
  } else {
    # No parallelization
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    allResultsFileNames <- file.path(resultsFileFolder, paste0(resultsFileName, ".csv"))
    analyzeCoreSensitivity(
      simulation = sim,
      parametersToPerturb = parametersToPerturb,
      totalSensitivityThreshold = 1,
      resultsFilePath = allResultsFileNames
    )
  }
  return(allResultsFileNames)
}


#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores for a single individual, save results as CSV.
#' @param parametersToPerturb paths to parameters to perturb in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained from a population object's getParameterValuesForIndividual() function.
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return Simulation results for population
#' @import ospsuite
runParallelSensitivityAnalysis <- function(simFilePath,
                                           parametersToPerturb,
                                           individualParameters,
                                           numberOfCores,
                                           resultsFileFolder,
                                           resultsFileName) {
  totalNumberParameters <- length(parametersToPerturb)

  # Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  seqVec <- (1 + ((1:totalNumberParameters) %% numberOfCores)) # Create a vector, of length totalNumberParameters, consisting of a repeating sequence of integers from 1 to numberOfCores
  sortVec <- sort(seqVec) # Sort seqVec to obtain an concatenated array of repeated integers, with the repeated integers ranging from from 1 to numberOfCores.  These are the core numbers to which each parameter will be assigned.
  listSplitParameters <- split(x = parametersToPerturb, sortVec) # Split the parameters of the model according to sortVec
  # listSplitParameters <- split(x = parametersToPerturb, sort(1+( (1:length(totalNumberParameters)) %% numberOfCores))  )
  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)
  Rmpi::mpi.bcast.cmd(library("ospsuite"))
  Rmpi::mpi.bcast.cmd(library("ospsuite.reportingengine"))
  Rmpi::mpi.bcast.Robj2slave(obj = simFilePath)
  Rmpi::mpi.bcast.Robj2slave(obj = listSplitParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = resultsFileFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)

  # Generate a listcontaining names of SA CSV result files that will be output by each core
  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores, folderName = resultsFileFolder, fileName = resultsFileName)


  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  # Load simulation on each core
  Rmpi::mpi.bcast.cmd(sim <- loadSimulation(simFilePath))

  # Update simulation with individual parameters
  Rmpi::mpi.bcast.cmd(updateSimulationIndividualParameters(simulation = sim, individualParameters))



  Rmpi::mpi.remote.exec(analyzeCoreSensitivity(
    simulation = sim,
    parametersToPerturb = listSplitParameters[[mpi.comm.rank()]],
    totalSensitivityThreshold = 1,
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    numberOfCoresToUse = 1 # Number of local cores, set to 1 when parallelizing.
  ))
  Rmpi::mpi.close.Rslaves()

  allSAResults <- importSensitivityAnalysisResultsFromCSV(simulation = loadSimulation(simFilePath), filePaths = allResultsFileNames)
  combinedFilePath <- file.path(resultsFileFolder, paste0(resultsFileName, ".csv"))
  exportSensitivityAnalysisResultsToCSV(results = allSAResults, filePath = combinedFilePath)
  file.remove(allResultsFileNames)
  return(combinedFilePath)
}

#' @title analyzeCoreSensitivity
#' @description Run a sensitivity analysis for a single individual, perturbing only the set of parameters parametersToPerturb
#' @param simulation simulation class object
#' @param parametersToPerturb paths of parameters to be analyzed
#' @param totalSensitivityThreshold numeric value between 0 and 1.
#' Close to 0, only the most sensitive output paths are returned.
#' Close to 1, almost all the output paths are returned.
#' @param resultsFilePath Path to file storing results of sensitivity analysis
#' @param numberOfCoresToUse Number of cores to use on local node.  This parameter should be should be set to 1 when parallelizing over many nodes.
#' @return Save sensitivity analysis results as CSV in path given by resultsFilePath.
#' @import ospsuite
#' @export
analyzeCoreSensitivity <- function(simulation,
                                   parametersToPerturb = NULL,
                                   totalSensitivityThreshold = 1,
                                   resultsFilePath = paste0(getwd(), "sensitivityAnalysisResults.csv"),
                                   numberOfCoresToUse = NULL) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation)
  sensitivityAnalysis$addParameterPaths(parametersToPerturb)



  if (is.null(numberOfCoresToUse)) {
    sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)
  }
  else {
    sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
      showProgress = FALSE,
      numberOfCoresToUse = numberOfCoresToUse
      #The numberOfCoresToUse input in the SensitivityAnalysisRunOptions initializer should not be set to NULL.
    )
  }
  print("Running sensitivity analysis...")
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  print("...done")
  exportSensitivityAnalysisResultsToCSV(results = sensitivityAnalysisResults, resultsFilePath)
}



#' @title getPKResultsDataFrame
#' @description Read PK parameter results into a dataframe and set QuantityPath,Parameter and Unit columns as factors
#' @param pkParameterResultsFilePath Path to PK parameter results CSV file
#' @return pkResultsDataFrame, a dataframe storing the contents of the CSV file with path pkParameterResultsFilePath
#' @import ospsuite
getPKResultsDataFrame <- function(pkParameterResultsFilePath) {
  pkResultsDataFrame <- read.csv(pkParameterResultsFilePath, encoding = "UTF-8", check.names = FALSE)
  colnames(pkResultsDataFrame) <- c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")
  pkResultsDataFrame$QuantityPath <- as.factor(pkResultsDataFrame$QuantityPath)
  pkResultsDataFrame$Parameter <- as.factor(pkResultsDataFrame$Parameter)
  pkResultsDataFrame$Unit <- as.factor(pkResultsDataFrame$Unit)
  return(pkResultsDataFrame)
}




#' @title getQuantileIndividualIds
#' @description Find IDs of individuals whose PK analysis results closest to quantiles given by vector of quantiles quantileVec
#' @param pkAnalysisResultsDataframe Dataframe storing the PK analysis results for multiple individuals for a single PK parameter and single output path
#' @return ids, IDs of individuals whose PK analysis results closest to quantiles given by vector of quantiles quantileVec
getQuantileIndividualIds <- function(pkAnalysisResultsDataframe, quantileVec = c(0.05, 0.5, 0.95)) {
  rowNums <- NULL
  for (n in 1:length(quantileVec)) {
    rowNums[n] <- which.min(abs(pkAnalysisResultsDataframe$Value - quantile(pkAnalysisResultsDataframe$Value, quantileVec[n])))
  }
  ids <- as.numeric(pkAnalysisResultsDataframe$IndividualId[rowNums])
  values <- pkAnalysisResultsDataframe$Value[rowNums]
  units <- as.character(pkAnalysisResultsDataframe$Unit[rowNums])
  quantileResults <- list(ids = ids,values=values,units=units)
  return(quantileResults)
}

#' @title populationSensitivityAnalysis
#' @param simFilePath path to simulation file
#' @param popDataFilePath path to population file
#' @param pkParameterResultsFilePath path to pk parameter results CSV
#' @param resultsFileFolder folder where results of sensitivity analysis will be saved
#' @param popSAResultsIndexFile Names of CSV file containing index of sensitivity analysis CSV files
#' @param resultsFileName root name of sensitivity analysis results CSV files
#' @param quantileVec vector of quantiles in (0,1).  For each output and pk parameter, there will be a distribution of pk parameter values for the population.
#' The individuals yielding pk parameters closest to these quantiles will be selected for sensitivity analysis.  Default is (0.05,0.5,0.95)
#' @param numberOfCores the number of cores to be used for parallelization of the sensitivity analysis.  Default is 1 core (no parallelization).
#' @export
populationSensitivityAnalysis <- function(simFilePath,
                                          popDataFilePath,
                                          pkParameterResultsFilePath,
                                          resultsFileFolder,
                                          resultsFileName,
                                          popSAResultsIndexFile = "sensitivityAnalysesResultsIndexFile",
                                          quantileVec = c(0.05, 0.5, 0.95),
                                          numberOfCores = 1) {
  sensitivityAnalysesResultsIndexFileDF <- getSAFileIndex(pkParameterResultsFilePath, quantileVec, resultsFileFolder, resultsFileName, popSAResultsIndexFile)
  ids <- unique(sensitivityAnalysesResultsIndexFileDF$IndividualId)

  analyzeSensitivity(simFilePath,
    parametersToPerturb = NULL,
    popFilePath = popDataFilePath,
    individualID = ids,
    numberOfCores = numberOfCores,
    resultsFileFolder = resultsFileFolder,
    resultsFileName = resultsFileName
  )
}


#' @title getSAFileIndex
#' @description Function to build and write to CSV a dataframe that stores all sensitivity analysis result files that will be output by a population sensitivity analysis.
#' @param pkParameterResultsFilePath path to pk parameter results CSV file
#' @param quantileVec quantiles of distributions of pk parameter results for a population.  The individual in the population that yields a pk parameter closest to the quantile is selected for sensitivity analysis.
#' @param resultsFileFolder path to population sensitivity analysis results CSV files
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param popSAResultsIndexFile name of CSV file that will store index that identifies name of sensitivity analysis results file for each sensitivity analysis run

getSAFileIndex <- function(pkParameterResultsFilePath,
                           quantileVec,
                           resultsFileFolder,
                           resultsFileName,
                           popSAResultsIndexFile) {

  allPKResultsDataframe <- getPKResultsDataFrame(pkParameterResultsFilePath)
  outputs <- levels(allPKResultsDataframe$QuantityPath)
  pkParameters <- levels(allPKResultsDataframe$Parameter)
  # pkParameters<- pkParameters[1:3]#### ONLY TO TEST SMALL PK PARAMETER SET - REMOVE THIS LINE EVENTUALLY!!!!!!!!!!!!!!!!!!!!


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
      for (n in 1:length(quantileResults$ids)) {
        outputColumn <- c(outputColumn, output)
        pkParameterColumn <- c(pkParameterColumn, pkParameter)
        individualIdColumn <- c(individualIdColumn, quantileResults$ids[n])
        valuesColumn <- c(valuesColumn, quantileResults$values[n])
        unitsColumn <- c(unitsColumn, quantileResults$units[n])
        quantileColumn <- c(quantileColumn, quantileVec[n])
      }
    }
  }

  filenamesColumn <-  paste0(sapply(X = individualIdColumn,FUN = getIndividualSAResultsFileName,resultsFileName),".csv")
  sensitivityAnalysesResultsIndexFileDF <- data.frame("Outputs" = outputColumn, "pkParameters" = pkParameterColumn, "Quantile" = quantileColumn, "Value" = valuesColumn, "Unit" = unitsColumn,  "IndividualId" = individualIdColumn, "Filename" = filenamesColumn)
  write.csv(x = sensitivityAnalysesResultsIndexFileDF, file = file.path(resultsFileFolder, paste0(popSAResultsIndexFile, ".csv")))
  return(sensitivityAnalysesResultsIndexFileDF)
}


#' @title getIndividualSAResultsFileName
#' @description Function to build name of inidividual SA results file
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param individualId id of individual
getIndividualSAResultsFileName <- function(individualId,resultsFileName){
  return(paste(resultsFileName,"IndividualId",individualId,sep="-"))
}
