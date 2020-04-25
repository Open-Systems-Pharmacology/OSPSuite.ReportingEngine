#' @title runSensitivity
#' @description Determine whether to run SA for individual or population.  If for individual,  pass simulation to individualSensitivityAnalysis.
#' If SA is for population, loop thru population file, extract parameters for each individual, and pass them to individualSensitivityAnalysis.
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the population sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @param individualId ID of individual in population data file for whom to perform sensitivity analysis
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return SA results for individual or population
#' @export
#' @import ospsuite
runSensitivity <- function(structureSet,
                           settings,
                           individualId = NULL,
                           logFolder = getwd(),
                           resultsFileName = NULL) {
  variableParameterPaths <- settings$variableParameterPaths
  popFilePath <- structureSet$simulationSet$populationFile
  variationRange <- settings$variationRange
  numberOfCores <- settings$numberOfCores
  showProgress <- settings$showProgress

  sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

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

    # Pass to cores the inputs that will not change from one individual to another
    Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
    Rmpi::mpi.bcast.Robj2slave(obj = variationRange)
    Rmpi::mpi.bcast.Robj2slave(obj = showProgress)
    # Load simulation on each core
    Rmpi::mpi.remote.exec(sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet))
  }

  # If there is a population file and individualId then for each individual perform SA
  # If there is a population file and no individualId then do SA for entire population
  # If there is no population file and individualId then do SA for mean model
  # If there is no population file and no individualId then do SA for mean model.
  if (!is.null(popFilePath)) { # Determine if SA is to be done for a single individual or more
    popObject <- loadPopulation(popFilePath)
    resultsFileName <- resultsFileName %||% "sensitivityAnalysisResults"
    individualSeq <- individualId %||% seq(1, popObject$count)
    individualSensitivityAnalysisResults <- list()
    for (ind in individualSeq) {
      logWorkflow(
        message = paste("Starting sensitivity analysis for individual", ind),
        pathFolder = logFolder
      )

      individualSensitivityAnalysisResults[[getIndividualSAResultsFileName(ind, resultsFileName)]] <- individualSensitivityAnalysis(
        structureSet = structureSet,
        variableParameterPaths = variableParameterPaths,
        individualParameters = popObject$getParameterValuesForIndividual(individualId = ind),
        variationRange = variationRange,
        numberOfCores = numberOfCores,
        showProgress = showProgress,
        logFolder = logFolder
      )
    }
  }
  else {
    individualSensitivityAnalysisResults <- individualSensitivityAnalysis(
      structureSet = structureSet,
      variableParameterPaths = variableParameterPaths,
      individualParameters = NULL,
      variationRange = variationRange,
      numberOfCores = numberOfCores,
      showProgress = showProgress,
      logFolder = logFolder
    )
  }

  # If numberOfCores > 1 then close cores spawned earlier.
  if (numberOfCores > 1) {
    Rmpi::mpi.close.Rslaves()
  }
  return(individualSensitivityAnalysisResults)
}


#' @title individualSensitivityAnalysis
#' @description Run SA for an individual, possibly after modifying the simulation using individualParameters.
#' Determine whether to run SA for on single core or in parallel.
#' If on single core, pass simulation to analyzeCoreSensitivity.
#' If in parallel, pass simulation to runParallelSensitivityAnalysis.
#' @param structureSet `SimulationStructure` R6 class object
#' @param variableParameterPaths paths to parameters to vary in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained from a
#' population object's getParameterValuesForIndividual() function.
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @return SA results for an individual
#' @import ospsuite
individualSensitivityAnalysis <- function(structureSet,
                                          variableParameterPaths = NULL,
                                          individualParameters,
                                          variationRange,
                                          numberOfCores = 1,
                                          showProgress = FALSE,
                                          logFolder = getwd()) {


  # Determine if SA is to be done on a single core or more
  if (numberOfCores > 1) {
    individualSensitivityAnalysisResults <- runParallelSensitivityAnalysis(
      structureSet = structureSet,
      variableParameterPaths = variableParameterPaths,
      individualParameters = individualParameters,
      numberOfCores = numberOfCores,
      logFolder = logFolder
    )
  } else {
    # No parallelization
    # Load simulation to determine number of parameters valid for sensitivity analysis
    sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    individualSensitivityAnalysisResults <- analyzeCoreSensitivity(
      simulation = sim,
      variableParameterPaths = variableParameterPaths,
      variationRange = variationRange,
      showProgress = showProgress
    )
  }
  return(individualSensitivityAnalysisResults)
}


#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores
#' for a single individual, save results as CSV.
#' @param structureSet `SimulationStructure` R6 class object
#' @param variableParameterPaths paths to parameters to vary in sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained
#' from a population object's getParameterValuesForIndividual() function.
#' @param numberOfCores is the number of cores over which to parallelize the sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @return SA results for population
#' @import ospsuite
runParallelSensitivityAnalysis <- function(structureSet,
                                           variableParameterPaths,
                                           individualParameters,
                                           numberOfCores,
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
  allResultsFileNames <- generateResultFileNames(
    numberOfCores = numberOfCores,
    folderName = getwd(),
    fileName = "tempSAResultsCore"
  )
  logWorkflow(message = "Starting sending of parameters to cores", pathFolder = logFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = listSplitParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNamePrefix)
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)
  logWorkflow(message = "Sending of parameters to cores completed", pathFolder = logFolder)

  # Update simulation with individual parameters
  logWorkflow(message = "Updating individual parameters on cores.", pathFolder = logFolder)
  Rmpi::mpi.remote.exec(updateSimulationIndividualParameters(simulation = sim, individualParameters))

  logWorkflow(message = "Starting sensitivity analysis on cores.", pathFolder = logFolder)
  Rmpi::mpi.remote.exec(partialIndividualSensitivityAnalysisResults <- analyzeCoreSensitivity(
    simulation = sim,
    variableParameterPaths = listSplitParameters[[mpi.comm.rank()]],
    variationRange = variationRange,
    numberOfCores = 1, # Number of local cores, set to 1 when parallelizing.
    debugLogFileName = paste0(tempLogFileNamePrefix, mpi.comm.rank()),
    nodeName = paste("Core", mpi.comm.rank()),
    showProgress = showProgress
  ))
  Rmpi::mpi.remote.exec(exportSensitivityAnalysisResultsToCSV(
    results = partialIndividualSensitivityAnalysisResults,
    filePath = allResultsFileNames[mpi.comm.rank()]
  ))

  for (core in seq(1, numberOfCores)) {
    logWorkflow(message = readLines(tempLogFileNames[core]), pathFolder = logFolder)
    file.remove(tempLogFileNames[core])
  }

  allSAResults <- importSensitivityAnalysisResultsFromCSV(simulation = loadSimulationWithUpdatedPaths(structureSet$simulationSet), filePaths = allResultsFileNames)
  file.remove(allResultsFileNames)
  return(allSAResults)
}

#' @title analyzeCoreSensitivity
#' @description Run a sensitivity analysis for a single individual,
#' varying only the set of parameters variableParameterPaths
#' @param simulation simulation class object
#' @param variableParameterPaths paths of parameters to be analyzed
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores Number of cores to use on local node.  This parameter
#' should be should be set to 1 when parallelizing over many nodes.
#' @param debugLogFileName path to debug log file
#' @param infoLogFileName path to info log file
#' @param errorLogFileName path to error log file
#' @param nodeName identifier for node used in parallel computation of sensitivity
#' @return sensitivity analysis results
#' @import ospsuite
#' @export
analyzeCoreSensitivity <- function(simulation,
                                   variableParameterPaths = NULL,
                                   variationRange = 0.1, # resultsFilePath = paste0(getwd(), "sensitivityAnalysisResults.csv"),
                                   numberOfCores = NULL,
                                   debugLogFileName = file.path(getwd(), defaultFileNames$logDebugFile()),
                                   infoLogFileName = file.path(getwd(), defaultFileNames$logInfoFile()),
                                   errorLogFileName = file.path(getwd(), defaultFileNames$logErrorFile()),
                                   nodeName = NULL,
                                   showProgress = FALSE) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation, variationRange = variationRange)
  sensitivityAnalysis$addParameterPaths(variableParameterPaths)
  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
    showProgress = showProgress,
    numberOfCores = numberOfCores
  )

  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Starting sensitivity analysis for path(s) ", paste(variableParameterPaths, collapse = ", ")), file = debugLogFileName, printConsole = FALSE)
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Sensitivity analysis for current path(s) completed"), file = debugLogFileName, printConsole = FALSE)
  return(sensitivityAnalysisResults)
}



#' @title getPKResultsDataFrame
#' @description Read PK parameter results into a dataframe and set QuantityPath,Parameter and Unit columns as factors
#' @param pkParameterResultsFilePath Path to PK parameter results CSV file
#' @param pkParameterSelection PK parameters used for selection of individuals for whom to calculate sensitivity
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
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the population sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @export
runPopulationSensitivityAnalysis <- function(structureSet, settings, logFolder = getwd()) {
  resultsFileName <- trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(structureSet$simulationSet$simulationSetName), extension = "csv")
  popSAResultsIndexFile <- trimFileName(structureSet$popSensitivityAnalysisResultsIndexFile)

  sensitivityAnalysesResultsIndexFileDF <- getSAFileIndex(
    pkParameterResultsFilePath = structureSet$pkAnalysisResultsFileNames,
    pkParameterSelection = settings$pkParameterSelection,
    quantileVec = settings$quantileVec, # resultsFileFolder = getwd(),
    resultsFileName = resultsFileName
  )

  ids <- unique(sensitivityAnalysesResultsIndexFileDF$IndividualId)

  popSensitivityResultsDF <- runSensitivity(
    structureSet = structureSet,
    settings = settings,
    individualId = ids,
    logFolder = logFolder,
    resultsFileName = resultsFileName
  )

  return(list(
    "indexDataFrame" = sensitivityAnalysesResultsIndexFileDF,
    "indexFileName" = popSAResultsIndexFile,
    "populationSensitivityResults" = popSensitivityResultsDF
  ))
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
                           resultsFileName) {
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
  filenamesColumn <- sapply(X = individualIdColumn, FUN = getIndividualSAResultsFileName, resultsFileName)
  # filenamesColumn <- paste0(sapply(X = individualIdColumn, FUN = getIndividualSAResultsFileName, resultsFileName), ".csv")
  sensitivityAnalysesResultsIndexFileDF <- data.frame("Outputs" = outputColumn, "pkParameters" = pkParameterColumn, "Quantile" = quantileColumn, "Value" = valuesColumn, "Unit" = unitsColumn, "IndividualId" = individualIdColumn, "Filename" = filenamesColumn)
  # write.csv(x = sensitivityAnalysesResultsIndexFileDF, file = file.path(resultsFileFolder, paste0(popSAResultsIndexFile, ".csv")))
  return(sensitivityAnalysesResultsIndexFileDF)
}


#' @title getIndividualSAResultsFileName
#' @description Function to build name of inidividual SA results file
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param individualId id of individual
getIndividualSAResultsFileName <- function(individualId, resultsFileName) {
  return(paste0(resultsFileName, "IndividualId-", individualId, ".csv"))
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



#' @title plotMeanSensitivity
#' @description Plot sensitivity analysis results for mean models
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @return list of plots and tables
#' @export
#' @import ospsuite
plotMeanSensitivity <- function(structureSet,
                                logFolder = getwd(),
                                settings = NULL) {
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  saResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
    simulation = simulation,
    structureSet$sensitivityAnalysisResultsFileNames
  )

  # TO DO: workout integration of selection of output paths and PK parameters in settings
  allOutputPaths <- structureSet$simulationSet$pathID %||% sapply(simulation$outputSelections$allOutputs, function(output) {
    output$path
  })
  pkParameters <- saResults$allPKParameterNames

  sensitivityPlots <- list()
  for (outputPathIndex in seq_along(allOutputPaths)) {
    for (pkParameter in pkParameters) {
      pkSensitivities <- saResults$allPKParameterSensitivitiesFor(
        pkParameterName = pkParameter,
        outputPath = allOutputPaths[outputPathIndex]
      )

      # Translate into a data.frame for plot
      sensitivityData <- data.frame(
        parameter = sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$parameterPath
        }),
        value = sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$value
        })
      )

      # Create the tornado plot with output path - PK parameter as its name
      sensitivityPlots[[paste0(pkParameter, "-", outputPathIndex)]] <- plotTornado(
        data = sensitivityData,
        plotConfiguration = NULL
      )
    }
  }

  return(list(plots = sensitivityPlots))
}




#' @title plotTornado
#' @description Plot sensitivity results in a tornado plot
#' @param data data.frame
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotTornado <- function(data,
                        plotConfiguration = NULL) {

  # Ensure that the plot bars are ordered by sensitivity values
  data$parameter <- reorder(data$parameter, data$value)

  tornadoPlot <- tlf::initializePlot(plotConfiguration)
  tornadoPlot <- tornadoPlot + ggplot2::geom_col(
    data = data,
    mapping = ggplot2::aes_string(
      x = "parameter",
      y = "value",
      fill = "parameter",
      color = "parameter"
    ),
    alpha = 0.8,
    size = 1,
    show.legend = FALSE,
    position = "dodge"
  ) +
    ggplot2::coord_flip() + ggplot2::xlab(NULL) + ggplot2::ylab("Sensitivity") + ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::scale_y_continuous(limits = c(-1.05 * max(abs(data$value)), 1.05 * max(abs(data$value)))) +
    ggplot2::scale_fill_brewer(palette = "Spectral", aesthetics = c("color", "fill")) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = 1,
      size = 1,
      linetype = "longdash"
    )

  return(tornadoPlot)
}
