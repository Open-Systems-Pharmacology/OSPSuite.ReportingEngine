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
    loadLibraryOnCores(libraryName = "ospsuite.reportingengine", logFolder = logFolder)
    loadLibraryOnCores(libraryName = "ospsuite", logFolder = logFolder)
    Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
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
#' If on single core, pass simulation to analyzeSensitivity.
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
      variationRange = variationRange,
      showProgress = showProgress,
      individualParameters = individualParameters,
      numberOfCores = numberOfCores,
      logFolder = logFolder
    )
  } else {
    # No parallelization
    # Load simulation to determine number of parameters valid for sensitivity analysis
    sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    individualSensitivityAnalysisResults <- analyzeSensitivity(
      simulation = sim,
      variableParameterPaths = variableParameterPaths,
      variationRange = variationRange,
      showProgress = showProgress,
      logFolder = logFolder
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
                                           variationRange,
                                           showProgress,
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
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores), ".txt")

  # Generate a listcontaining names of SA CSV result files that will be output by each core
  allResultsFileNames <- generateResultFileNames(
    numberOfCores = numberOfCores,
    folderName = structureSet$workflowFolder,
    fileName = "tempSAResultsCore"
  )

  partialIndividualSensitivityAnalysisResults <- NULL

  # Load simulation on each core
  loadSimulationOnCores(structureSet = structureSet, logFolder = logFolder)

  logWorkflow(message = "Starting sending of parameters to cores", pathFolder = logFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = partialIndividualSensitivityAnalysisResults)
  Rmpi::mpi.bcast.Robj2slave(obj = variationRange)
  Rmpi::mpi.bcast.Robj2slave(obj = showProgress)
  Rmpi::mpi.bcast.Robj2slave(obj = listSplitParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNames)
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)
  logWorkflow(message = "Sending of parameters to cores completed", pathFolder = logFolder)

  # Update simulation with individual parameters
  logWorkflow(message = "Updating individual parameters on cores.", pathFolder = logFolder)
  updateIndividualParametersOnCores(individualParameters = individualParameters, logFolder = logFolder)

  logWorkflow(message = "Starting sensitivity analysis on cores.", pathFolder = logFolder)
  Rmpi::mpi.remote.exec(partialIndividualSensitivityAnalysisResults <- analyzeCoreSensitivity(
    simulation = sim,
    variableParameterPaths = listSplitParameters[[mpi.comm.rank()]],
    variationRange = variationRange,
    numberOfCores = 1, # Number of local cores, set to 1 when parallelizing.
    debugLogFileName = tempLogFileNames[mpi.comm.rank()],
    nodeName = paste("Core", mpi.comm.rank()),
    showProgress = showProgress
  ))

  # Verify sensitivity analyses ran successfully
  sensitivityRunSuccess <- Rmpi::mpi.remote.exec(!is.null(partialIndividualSensitivityAnalysisResults))
  verifySensitivityAnalysisRunSuccessful(sensitivityRunSuccess, logFolder = logFolder)

  # Write core logs to workflow logs
  for (core in seq(1, numberOfCores)) {
    logWorkflow(message = readLines(tempLogFileNames[core]), pathFolder = logFolder)
    file.remove(tempLogFileNames[core])
  }

  # Remove any previous temporary results files
  Rmpi::mpi.remote.exec(if (file.exists(allResultsFileNames[mpi.comm.rank()])) {
    file.remove(allResultsFileNames[mpi.comm.rank()])
  })
  anyPreviousPartialResultsRemoved <- Rmpi::mpi.remote.exec(!file.exists(allResultsFileNames[mpi.comm.rank()]))
  verifyAnyPreviousFilesRemoved(anyPreviousPartialResultsRemoved, logFolder = logFolder)

  # Export temporary results files to CSV
  Rmpi::mpi.remote.exec(ospsuite::exportSensitivityAnalysisResultsToCSV(
    results = partialIndividualSensitivityAnalysisResults,
    filePath = allResultsFileNames[mpi.comm.rank()]
  ))
  partialResultsExported <- Rmpi::mpi.remote.exec(file.exists(allResultsFileNames[mpi.comm.rank()]))
  verifyPartialResultsExported(partialResultsExported, logFolder = logFolder)

  # Merge temporary results files
  allSAResults <- importSensitivityAnalysisResultsFromCSV(simulation = loadSimulationWithUpdatedPaths(structureSet$simulationSet), filePaths = allResultsFileNames)
  file.remove(allResultsFileNames)
  return(allSAResults)
}


#' @title analyzeSensitivity
#' @description Run a sensitivity analysis for a single individual,
#' varying only the set of parameters variableParameterPaths
#' @param simulation simulation class object
#' @param variableParameterPaths paths of parameters to be analyzed
#' @param variationRange variation range for sensitivity analysis
#' @param numberOfCores Number of cores to use on local node.  This parameter
#' should be should be set to 1 when parallelizing over many nodes.
#' @param logFolder folder where the logs are saved
#' @param showProgress option to print progress of simulation to console
#' @return sensitivity analysis results
#' @import ospsuite
#' @export
analyzeSensitivity <- function(simulation,
                               variableParameterPaths = NULL,
                               variationRange = 0.1,
                               numberOfCores = NULL,
                               logFolder = getwd(),
                               showProgress = FALSE) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation, variationRange = variationRange)
  sensitivityAnalysis$addParameterPaths(variableParameterPaths)
  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
    showProgress = showProgress,
    numberOfCores = numberOfCores
  )

  logWorkflow(message = paste0("Starting sensitivity analysis for path(s) ", paste(variableParameterPaths, collapse = ", ")), pathFolder = logFolder)
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  logWorkflow(message = "Sensitivity analysis for current path(s) completed", pathFolder = logFolder)
  return(sensitivityAnalysisResults)
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
#' @param showProgress option to print progress of simulation to console
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








#' @title getQuantileIndividualIds
#' @description Find IDs of individuals whose PK analysis results closest toquantiles given
#' by vector of quantiles quantileVec
#' @param pkAnalysisResultsDataframe Dataframe storing the PK analysis results for multiple
#' individuals for a single PK parameter and single output path
#' @param quantileVec vector of quantiles in the pk results distribution.  Ids for individuals with pk parameter values at these quantiles will be returned.
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
    structureSet = structureSet,
    settings = settings,
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


#' @title getPKResultsDataFrame
#' @description Read PK parameter results into a dataframe and set QuantityPath,Parameter and Unit columns as factors
#' @param structureSet `SimulationStructure` R6 class object
#' @return pkResultsDataFrame, a dataframe storing the contents of the CSV file with path pkParameterResultsFilePath
#' @import ospsuite
getPKResultsDataFrame <- function(structureSet) {
  pkResultsDataFrame <- read.csv(structureSet$pkAnalysisResultsFileNames,
    encoding = "UTF-8",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # get a list pkParameterNamesEachOutput where field names are the paths to the output andthe names of the pkParameters for each output in the current simulation set
  pkParameterNamesEachOutput <- list()

  for (n in seq_along(structureSet$simulationSet$outputs)) {
    op <- structureSet$simulationSet$outputs[[n]]
    if (!is.null(op$pkParameters)) {
      # case where pkParameters are specified for each output path
      pkParameterNamesEachOutput[[op$path]] <- unname(sapply(
        op$pkParameters,
        function(y) {
          y$pkParameter
        }
      ))
    } else {
      # case where no pkParameters are specified for an output and SA performed for all pk parameters by default
      pkResultsDataFrameThisOutput <- pkResultsDataFrame[ pkResultsDataFrame$QuantityPath == op$path, ]
      pkParameterNamesEachOutput[[op$path]] <- unique(pkResultsDataFrameThisOutput$Parameter)
    }
  }

  colnames(pkResultsDataFrame) <- c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")

  # extract rows of pkResultsDataFrame corresponding to simulation set outputs and pkParameters
  filteredPkResultsList <- list()
  for (n in seq_along(pkParameterNamesEachOutput)) {
    op <- names(pkParameterNamesEachOutput)[n]
    filteredPkResultsList[[n]] <- pkResultsDataFrame[ (pkResultsDataFrame$QuantityPath %in% op) & (pkResultsDataFrame$Parameter %in% pkParameterNamesEachOutput[[op]]), ]
  }
  filteredPkResultsDf <- do.call("rbind.data.frame", filteredPkResultsList)


  filteredPkResultsDf$QuantityPath <- as.factor(filteredPkResultsDf$QuantityPath)
  filteredPkResultsDf$Parameter <- as.factor(filteredPkResultsDf$Parameter)
  filteredPkResultsDf$Unit <- as.factor(filteredPkResultsDf$Unit)
  return(filteredPkResultsDf)
}


#' @title getSAFileIndex
#' @description Function to build and write to CSV a dataframe that stores all
#' sensitivity analysis result files that will be output by a population sensitivity analysis.
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the population sensitivity analysis
#' @param resultsFileName root name of population sensitivity analysis results CSV files
getSAFileIndex <- function(structureSet = structureSet,
                           settings = settings,
                           resultsFileName = resultsFileName) {
  quantileVec <- settings$quantileVec

  allPKResultsDataframe <- getPKResultsDataFrame(structureSet)

  outputs <- levels(allPKResultsDataframe$QuantityPath)
  outputColumn <- NULL
  pkParameterColumn <- NULL
  individualIdColumn <- NULL
  valuesColumn <- NULL
  unitsColumn <- NULL
  quantileColumn <- NULL
  for (output in outputs) {
    pkParameters <- unique(allPKResultsDataframe$Parameter[allPKResultsDataframe$QuantityPath == output])
    for (pkParameter in pkParameters) {
      singleOuputSinglePKDataframe <- allPKResultsDataframe[ allPKResultsDataframe["QuantityPath"] == output & allPKResultsDataframe["Parameter"] == pkParameter, ]
      quantileResults <- getQuantileIndividualIds(singleOuputSinglePKDataframe, quantileVec)
      for (i in seq_along(quantileResults$ids)) {
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
  sensitivityAnalysesResultsIndexFileDF <- data.frame("Outputs" = outputColumn, "pkParameters" = pkParameterColumn, "Quantile" = quantileColumn, "Value" = valuesColumn, "Unit" = unitsColumn, "IndividualId" = individualIdColumn, "Filename" = filenamesColumn)

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
  sensitivityPlots <- list()
  sensitivityCaptions <- list()

  for (output in structureSet$simulationSet$outputs) {
    pathLabel <- lastPathElement(output$path)
    for (pkParameter in output$pkParameters) {
      parameterLabel <- lastPathElement(pkParameter$pkParameter)

      pkSensitivities <- saResults$allPKParameterSensitivitiesFor(
        pkParameterName = pkParameter$pkParameter,
        outputPath = output$path
      )

      sensitivityData <- data.frame(
        parameter = as.character(sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$parameterPath
        })),
        value = as.numeric(sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$value
        })),
        stringsAsFactors = FALSE
      )
      sensitivityData$parameter <- renderSaParameterDisplayName(sensitivityData$parameter)

      sensitivityPlots[[paste0(parameterLabel, "-", pathLabel)]] <- plotTornado(
        data = sensitivityData,
        plotConfiguration = settings$plotConfiguration
      )
      sensitivityCaptions[[paste0(parameterLabel, "-", pathLabel)]] <- paste0("Most sensitive parameters for ", pkParameter$displayName %||% pkParameter$pkParameter, " of ", output$displayName, ".")
    }
  }
  return(list(
    plots = sensitivityPlots,
    captions = sensitivityCaptions
  ))
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
    ggplot2::coord_flip() + ggplot2::xlab(NULL) + ggplot2::ylab("Sensitivity") +
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

#' @title plotPopulationSensitivity
#' @description Retrieve list of plots of population sensitivity analyses across all populations
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the population sensitivity plot
#' @param logFolder folder where the logs are saved
#' @return a structured list of plots for each possible combination of pathID output-pkParameter that is found in sensitivity results index file
#' @export
#' @import ospsuite
plotPopulationSensitivity <- function(structureSets,
                                      logFolder = NULL,
                                      settings,
                                      workflowType = NULL,
                                      xParameters = NULL,
                                      yParameters = NULL) {
  dFList <- list()
  i <- 0

  saResultIndexFiles <- list()
  simulationList <- list()

  for (structureSet in structureSets) {
    sensitivityResultsFolder <- file.path(structureSet$workflowFolder, structureSet$sensitivityAnalysisResultsFolder)
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    indexDf <- read.csv(file = structureSet$popSensitivityAnalysisResultsIndexFile)
    outputPaths <- sapply(structureSet$simulationSet$outputs, function(x) {
      x$path
    })
    populationName <- structureSet$simulationSet$simulationSetName


    saResultIndexFiles[[populationName]] <- structureSet$popSensitivityAnalysisResultsIndexFile
    simulationList[[populationName]] <- simulation

    for (op in outputPaths) {
      # opIndexDf is sub-dataframe of indexDf that has only the outputs op in the Outputs column
      opIndexDf <- indexDf[indexDf$Outputs == op, ]

      # pkParameters are all the entries in the pkParameters column of  opIndexDf
      pkParameters <- unique(opIndexDf$pkParameters)

      for (pk in pkParameters) {
        i <- i + 1

        dfForPkAndOp <- getPopSensDfForPkAndOutput(
          simulation = simulation,
          sensitivityResultsFolder = sensitivityResultsFolder,
          indexDf = indexDf,
          output = op,
          pkParameter = pk,
          totalSensitivityThreshold = settings$totalSensitivityThreshold
        )
        populationNameCol <- rep(populationName, nrow(dfForPkAndOp))
        dFList[[i]] <- cbind(dfForPkAndOp, data.frame("Population" = populationNameCol))
      }
    }
  }

  # allPopsDf is a dataframe that holds the results of all sensitivity analyses for all populations
  allPopsDf <- do.call("rbind", dFList)


  # add any missing sensitivity results omitted when applying threshold in getPopSensDfForPkAndOutput

  # get set of unique combinations of outputs and pkParameters.  A plot is built for each combination.
  uniqueQuantitiesAndPKParameters <- unique(allPopsDf[, c("QuantityPath", "PKParameter")])


  for (n in 1:nrow(uniqueQuantitiesAndPKParameters)) {
    op <- uniqueQuantitiesAndPKParameters$QuantityPath[n]
    pk <- uniqueQuantitiesAndPKParameters$PKParameter[n]

    sensitivityThisOpPk <- allPopsDf[ (allPopsDf$QuantityPath %in% op) & (allPopsDf$PKParameter %in% pk), ]

    # get list of all perturbation parameters used in this plot
    allParamsForThisOpPk <- unique(sensitivityThisOpPk$Parameter)

    # get the sensitivity results dataframe for this combination of output and pkParameter
    individualCombinationsThisOpPk <- unique(sensitivityThisOpPk[, c("Quantile", "individualId", "Population")])

    # loop thru each individual in current combination of output and pkParameter
    for (m in 1:nrow(individualCombinationsThisOpPk)) {
      qu <- individualCombinationsThisOpPk$Quantile[m]
      id <- individualCombinationsThisOpPk$individualId[m]
      pop <- individualCombinationsThisOpPk$Population[m]

      # get list of all perturbation parameters for this one individual that are used in the plot for this combination of output and pkParameter
      allParamsForThisIndividual <- unique(sensitivityThisOpPk[ (sensitivityThisOpPk$Quantile %in% qu) & (sensitivityThisOpPk$individualId %in% id) & (sensitivityThisOpPk$Population %in% pop), ]$Parameter)

      # create list of the parameters missing for this individual but otherwise shown in this plot for this combination of output and pkParameter
      missingParameters <- setdiff(allParamsForThisOpPk, allParamsForThisIndividual)

      # loop thru the missing parameters for the current individual
      for (parNumber in seq_along(missingParameters)) {

        # load the index file of SA results for this individual's population to get the name of the individuals's sensitivity result file
        indx <- read.csv(saResultIndexFiles[[pop]])

        # get the name of the individuals's sensitivity result file
        saResFileName <- indx[ (indx$Outputs %in% op) & (indx$pkParameters %in% pk) & (indx$Quantile %in% qu), ]$Filename

        # import SA results for individual
        individualSAResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
          simulation = simulationList[[pop]],
          filePaths = file.path(structureSet$workflowFolder, structureSet$sensitivityAnalysisResultsFolder, saResFileName)
        )

        # get the sensitivity for the missing parameter for this individual and for this combination of output and pkParameter
        missingSensivitity <- individualSAResults$pkParameterSensitivityValueFor(
          pkParameterName = as.character(pk),
          parameterName = missingParameters[parNumber],
          outputPath = as.character(op)
        )

        # create a sensitivity result row for the missing parameter for this individual and this combination of output and pkParameter
        saMissingParameter <- data.frame(
          "QuantityPath" = op,
          "Parameter" = missingParameters[parNumber],
          "PKParameter" = pk,
          "Value" = missingSensivitity,
          "Quantile" = qu,
          "individualId" = id,
          "Population" = pop
        )

        # append to allPopsDf the row containing the missing parameter's sensitivity
        allPopsDf <- rbind(allPopsDf, saMissingParameter)
      }
    }
  }



  plotList <- list()

  for (i in 1:nrow(uniqueQuantitiesAndPKParameters)) {
    pk <- as.character(uniqueQuantitiesAndPKParameters$PKParameter[i])
    op <- as.character(uniqueQuantitiesAndPKParameters$QuantityPath[i])
    sensitivityPlotName <- paste(pk, op, sep = "_")
    sensitivityPlotName <- gsub(pattern = "|", replacement = "-", x = sensitivityPlotName, fixed = TRUE)

    # popDfPkOp is a sorted dataframe containing all the rows in allPopsDf that have the same
    # combination of (QuantityPath,PKParameter) as the current (i'th) row of uniqueQuantitiesAndPKParameters
    unsortedPopDfPkOp <- allPopsDf[allPopsDf[, "QuantityPath"] == uniqueQuantitiesAndPKParameters$QuantityPath[i] & allPopsDf[, "PKParameter"] == uniqueQuantitiesAndPKParameters$PKParameter[i], ]
    popDfPkOp <- unsortedPopDfPkOp[order(-abs(unsortedPopDfPkOp$Value)), ]

    # Set level order of Parameter column to make most sensitive parameter have highest factor level
    popDfPkOp$Parameter <- factor(x = popDfPkOp$Parameter, levels = rev(unique(popDfPkOp$Parameter)))

    # Get vector of settings$maximalParametersPerSensitivityPlot most sensitive parameters
    parameterLevels <- levels(popDfPkOp$Parameter)
    truncParamLevels <- rev(parameterLevels)[1:min(settings$maximalParametersPerSensitivityPlot, length(parameterLevels))]

    plotObject <- getPkParameterPopulationSensitivityPlot(
      data = popDfPkOp[ popDfPkOp$Parameter %in% truncParamLevels, ], # title = paste("Population sensitivity of", pk, "of", op),
      settings = settings
    )
    plotList[["plots"]][[sensitivityPlotName]] <- plotObject
  }


  return(plotList)
}



#' @title getPopSensDfForPkAndOutput
#' @description Retrieve dataframe of ranked and filtered population sensitivity results for a given PK parameter and model output pathID
#' @param simulation loaded from the used in the simulationFile path in the simulation set
#' @param sensitivityResultsFolder location of sensitivity analysis results
#' @param indexDf dataframe with contents of population sensitivity results index file
#' @param output pathID of output for which to obtain the population sensitivity results
#' @param pkParameter name of PK parameter for which to obtain the population sensitivity results
#' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
#' @return sortedFilteredIndividualsDfForPKParameter dataframe of population-wide sensitivity results for pkParameter and output
#' @import ospsuite
getPopSensDfForPkAndOutput <- function(simulation,
                                       sensitivityResultsFolder,
                                       indexDf,
                                       output,
                                       pkParameter,
                                       totalSensitivityThreshold) {
  pkOutputIndexDf <- getPkOutputIndexDf(indexDf, pkParameter, output)
  individualsDfForPKParameter <- NULL

  # Verify that there are SA results for this output and pkParameter combination
  if (nrow(pkOutputIndexDf) > 0) {
    dfList <- list()

    # Loop through the quantiles for this output and pkParameter combination
    for (n in 1:nrow(pkOutputIndexDf)) {

      # Current quantile
      quantile <- pkOutputIndexDf$Quantile[n]


      # Individual corresponding to current quantile
      individualId <- pkOutputIndexDf$IndividualId[n]

      # SA results file corresponding to individual correspinding to current quantile
      saResultsFileName <- pkOutputIndexDf$Filename[n]

      # import SA results for individual corresponding to current quantile
      individualSAResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
        simulation = simulation,
        filePaths = file.path(sensitivityResultsFolder, saResultsFileName)
      )

      # filter out low sensitivity parameters for current individual and current output and pkParameter using totalSensitivityThreshold
      filteredIndividualSAResults <- individualSAResults$allPKParameterSensitivitiesFor(
        pkParameterName = pkParameter,
        outputPath = output,
        totalSensitivityThreshold = totalSensitivityThreshold
      )

      # Verify that there exist SA results within the threshold given by totalSensitivityThreshold for this individual, this output and this pkParameter
      if (length(filteredIndividualSAResults) > 0) {

        # Build a list of sensitivities to parameters containing each sensitivity result falling within the threshold given by totalSensitivityThreshold
        listOfFilteredIndividualSAResults <- lapply(filteredIndividualSAResults, function(x) {
          list(
            "QuantityPath" = x$outputPath,
            "Parameter" = x$parameterName,
            "PKParameter" = x$pkParameterName,
            "Value" = x$value
          )
        })

        # Merge list into a dataframe
        individualDf <- do.call(rbind.data.frame, listOfFilteredIndividualSAResults)

        # Append dataframe for current individual, current output and current pkParameter to the list dFlist
        dfList[[n]] <- cbind(individualDf, data.frame("Quantile" = rep(quantile, nrow(individualDf)), "individualId" = rep(individualId, nrow(individualDf))))
      }
    }

    # Merge dFlist into one dataframe for all individuals for current output and current pkParameter
    individualsDfForPKParameter <- do.call(rbind.data.frame, dfList)
  }

  return(individualsDfForPKParameter)
}




#' @title getPkParameterPopulationSensitivityPlot
#' @description build sensitvity plot object for a population for one output and pk parameter
#' @param data dataframe of sensitivity analysis results
#' @param settings used to set plot configuration
#' @return plt sensitivity plot based on results in input data
#' @import ggplot2
#' @import tlf
getPkParameterPopulationSensitivityPlot <- function(data, settings) {
  data[["Quantile"]] <- as.factor(data[["Quantile"]])

  shapeAes <- NULL
  if ("Population" %in% colnames(data)) {
    shapeAes <- "Population"
  }

  plt <- ggplot2::ggplot() + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(x = "Parameter", y = "Value", color = "Quantile", shape = shapeAes),
    size = 2,
    position = ggplot2::position_dodge(width = 0.5)
  ) + ggplot2::xlab(NULL) + ggplot2::ylab("Sensitivity") + ggplot2::labs(
    color = "Individual quantile"
  )

  plt <- plt + ggplot2::geom_hline(yintercept = 0, size = 1)
  plt <- plt + ggplot2::coord_flip()

  plt <- plt + ggplot2::theme(
    legend.position = "top",
    legend.box = "vertical",
    text = element_text(size = settings$plotFontSize),
    legend.title = element_text(size = settings$plotFontSize),
    axis.text.x = element_text(size = settings$plotFontSize),
    axis.text.y = element_text(size = settings$plotFontSize),
    legend.spacing.y = unit(-0.1, "cm")
  )

  return(plt)
}


#' @title getPkOutputIndexDf
#' @description Function to filter the population results index file for given pkParameter and output
#' @param indexDf dataframe containing summary of sensitivity results
#' @param output pathID of output for which to obtain the population sensitivity results
#' @param pkParameter name of PK parameter for which to obtain the population sensitivity results
#' @return pkOutputIndexDf dataframe containing index of files containing population sensitivity analysis results conducted for given output and pkParameter
getPkOutputIndexDf <- function(indexDf, pkParameter, output) {
  pkOutputIndexDf <- indexDf[(indexDf$pkParameters == pkParameter) & (indexDf$Outputs == output), ]
  return(pkOutputIndexDf)
}

#' @title getDefaultTotalSensitivityThreshold
#' @description return the default totalSensitivityThreshold to be used in a population sensitivity analysis plot
#' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
#' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
getDefaultTotalSensitivityThreshold <- function(totalSensitivityThreshold = NULL, variableParameterPaths = NULL) {
  if (is.null(totalSensitivityThreshold)) {
    if (is.null(variableParameterPaths)) {
      totalSensitivityThreshold <- 0.9
    } else {
      totalSensitivityThreshold <- 1
    }
  }
  return(totalSensitivityThreshold)
}

#' @title renderSaParameterDisplayName
#' @description Render display names for sensitivity plots keeping elements in `depth` of parameter paths,
#' and removing following keys `Organism`, `Applications`, `ProtocolSchemaItem`, and `Neighborhoods`
#' @param paths vector of paths to render
#' @param depth levels in parameter path
#' @return Updated paths
renderSaParameterDisplayName <- function(paths, depth = 3) {
  updatedPaths <- paths
  # toPathArray and toPathString do not work here as paths were separated by "-" in the SA
  for (pathIndex in seq_along(paths)) {
    pathArray <- as.character(unlist(strsplit(paths[pathIndex], "-")))
    pathArray <- utils::tail(pathArray, depth)
    pathArray <- pathArray[!pathArray %in% c("Organism", "Applications", "ProtocolSchemaItem", "Neighborhoods")]
    updatedPaths[pathIndex] <- paste0(pathArray, collapse = " - ")
  }
  return(updatedPaths)
}
