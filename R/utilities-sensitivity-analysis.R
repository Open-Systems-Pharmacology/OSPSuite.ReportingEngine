#' @title runSensitivity
#' @description Determine whether to run SA for individual or population.  If for individual,  pass simulation to individualSensitivityAnalysis.
#' If SA is for population, loop thru population file, extract parameters for each individual, and pass them to individualSensitivityAnalysis.
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @param individualId ID of individual in population data file for whom to perform sensitivity analysis
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @return SA results for individual or population
#' @import ospsuite
#' @keywords internal
runSensitivity <- function(structureSet,
                           settings,
                           individualId = NULL,
                           logFolder = getwd(),
                           resultsFileName = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  allVariableParameterPaths <- ospsuite::potentialVariableParameterPathsFor(simulation = sim)

  # If no parameters to vary specified, vary all parameters valid for sensitivity analysis
  if (is.null(settings$variableParameterPaths)) {
    settings$variableParameterPaths <- allVariableParameterPaths
  } else {
    # if a variableParameterPaths input is provided, ensure that all
    # its elements exist within allVariableParameterPaths.  If not, give an error.

    validParameterPaths <- intersect(settings$variableParameterPaths, allVariableParameterPaths)

    if (length(validParameterPaths) == 0) {
      logErrorThenStop(messages$errorNoValidParametersForSensitivityAnalysis(structureSet$simulationSet$simulationSetName), logFolderPath = logFolder)
    }

    invalidParameterPaths <- setdiff(settings$variableParameterPaths, validParameterPaths)
    settings$variableParameterPaths <- validParameterPaths

    if (length(invalidParameterPaths) != 0) {
      logWorkflow(
        message = messages$warningIgnoringInvalidParametersForSensitivityAnalysis(invalidParameterPaths, structureSet$simulationSet$simulationSetName),
        pathFolder = logFolder
      )
    }
  }
  totalNumberParameters <- length(settings$variableParameterPaths)
  # In case there are more cores specified in numberOfCores than
  # there are parameters, ensure at least one parameter per spawned core
  settings$numberOfCores <- min(settings$numberOfCores, totalNumberParameters)
  if (totalNumberParameters == 0) {
    logErrorThenStop(messages$errorNoParametersForSensitivityAnalysis(), logFolderPath = logFolder)
  }

  # If numberOfCores > 1 then spawn cores for later use.
  # Otherwise sensitivity analysis will be run on master core only.
  if (settings$numberOfCores > 1) {
    Rmpi::mpi.spawn.Rslaves(nslaves = settings$numberOfCores)
    loadLibraryOnCores(libraryName = "ospsuite.reportingengine", logFolder = logFolder)
    loadLibraryOnCores(libraryName = "ospsuite", logFolder = logFolder)
    Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  }

  # If there is a population file and individualId then for each individual perform SA
  # If there is a population file and no individualId then do SA for entire population
  # If there is no population file and individualId then do SA for mean model
  # If there is no population file and no individualId then do SA for mean model.
  if (!is.null(structureSet$simulationSet$populationFile)) { # Determine if SA is to be done for a single individual or more
    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$populationFile)
    popObject <- loadPopulation(structureSet$simulationSet$populationFile)
    resultsFileName <- resultsFileName %||% "sensitivityAnalysisResults"
    individualSeq <- individualId %||% seq(0, popObject$count - 1)
    individualSensitivityAnalysisResults <- list()
    for (ind in individualSeq) {
      logWorkflow(
        message = paste("Starting sensitivity analysis for individual", ind),
        pathFolder = logFolder
      )

      individualSensitivityAnalysisResults[[getIndividualSAResultsFileName(ind, resultsFileName)]] <- individualSensitivityAnalysis(
        structureSet = structureSet,
        settings = settings,
        logFolder = logFolder,
        individualParameters = popObject$getParameterValuesForIndividual(individualId = ind)
      )
    }
  }
  else {
    individualSensitivityAnalysisResults <- individualSensitivityAnalysis(
      structureSet = structureSet,
      settings = settings,
      logFolder = logFolder,
      individualParameters = NULL
    )
  }

  # If numberOfCores > 1 then close cores spawned earlier.
  if (settings$numberOfCores > 1) {
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
#' @param settings list of settings for the sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained from a
#' population object's getParameterValuesForIndividual() function.
#' @param logFolder folder where the logs are saved
#' @return SA results for an individual
#' @import ospsuite
#' @keywords internal
individualSensitivityAnalysis <- function(structureSet,
                                          settings,
                                          individualParameters,
                                          logFolder = getwd()) {


  # Determine if SA is to be done on a single core or more
  if (settings$numberOfCores > 1) {
    individualSensitivityAnalysisResults <- runParallelSensitivityAnalysis(
      structureSet = structureSet,
      settings = settings,
      individualParameters = individualParameters,
      logFolder = logFolder
    )
  } else {
    # No parallelization

    # Get allowable number of cores
    settings$allowedCores <- getAllowedCores()

    # Load simulation to determine number of parameters valid for sensitivity analysis
    sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    individualSensitivityAnalysisResults <- analyzeSensitivity(
      simulation = sim,
      settings = settings,
      logFolder = logFolder
    )
  }
  return(individualSensitivityAnalysisResults)
}


#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores
#' for a single individual, save results as CSV.
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the sensitivity analysis
#' @param individualParameters is an object storing an individual's parameters, obtained
#' from a population object's `getParameterValuesForIndividual()`` function.
#' @param logFolder folder where the logs are saved
#' @return SA results for population
#' @import ospsuite
#' @keywords internal
runParallelSensitivityAnalysis <- function(structureSet,
                                           settings = settings,
                                           individualParameters,
                                           logFolder = getwd()) {
  totalNumberParameters <- length(settings$variableParameterPaths)

  variationRange <- settings$variationRange
  showProgress <- settings$showProgress

  # Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  # Create a vector, of length totalNumberParameters, consisting of a repeating
  # sequence of integers from 1 to numberOfCores
  seqVec <- (1 + ((1:totalNumberParameters) %% settings$numberOfCores))

  # Sort seqVec to obtain an concatenated array of repeated integers,
  # with the repeated integers ranging from from 1 to numberOfCores.
  # These are the core numbers to which each parameter will be assigned.
  sortVec <- sort(seqVec)

  # Split the parameters of the model according to sortVec
  listSplitParameters <- split(x = settings$variableParameterPaths, sortVec)
  tempLogFileNamePrefix <- file.path(logFolder, "logDebug-core-sensitivity-analysis")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, settings$numberOfCores), ".txt")

  # Generate a listcontaining names of SA CSV result files that will be output by each core
  allResultsFileNames <- generateResultFileNames(
    numberOfCores = settings$numberOfCores,
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
    variableParameterPaths = listSplitParameters[[Rmpi::mpi.comm.rank()]],
    variationRange = variationRange,
    numberOfCores = 1, # Number of local cores, set to 1 when parallelizing.
    debugLogFileName = tempLogFileNames[Rmpi::mpi.comm.rank()],
    nodeName = paste("Core", Rmpi::mpi.comm.rank()),
    showProgress = showProgress
  ))

  # Verify sensitivity analyses ran successfully
  sensitivityRunSuccess <- Rmpi::mpi.remote.exec(!is.null(partialIndividualSensitivityAnalysisResults))
  verifySensitivityAnalysisRunSuccessful(sensitivityRunSuccess, logFolder = logFolder)

  # Write core logs to workflow logs
  for (core in seq(1, settings$numberOfCores)) {
    logWorkflow(message = readLines(tempLogFileNames[core]), pathFolder = logFolder)
    file.remove(tempLogFileNames[core])
  }

  # Remove any previous temporary results files
  Rmpi::mpi.remote.exec(if (file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()])) {
    file.remove(allResultsFileNames[Rmpi::mpi.comm.rank()])
  })
  anyPreviousPartialResultsRemoved <- Rmpi::mpi.remote.exec(!file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()]))
  verifyAnyPreviousFilesRemoved(anyPreviousPartialResultsRemoved, logFolder = logFolder)

  # Export temporary results files to CSV
  Rmpi::mpi.remote.exec(ospsuite::exportSensitivityAnalysisResultsToCSV(
    results = partialIndividualSensitivityAnalysisResults,
    filePath = allResultsFileNames[Rmpi::mpi.comm.rank()]
  ))
  partialResultsExported <- Rmpi::mpi.remote.exec(file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()]))
  verifyPartialResultsExported(partialResultsExported, settings$numberOfCores, logFolder = logFolder)

  # Merge temporary results files
  allSAResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(simulation = loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE), filePaths = allResultsFileNames)
  file.remove(allResultsFileNames)
  return(allSAResults)
}


#' @title analyzeSensitivity
#' @description Run a sensitivity analysis for a single individual,
#' varying only the set of parameters variableParameterPaths
#' @param simulation simulation class object
#' @param settings list of settings for the sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @return sensitivity analysis results
#' @import ospsuite
#' @export
analyzeSensitivity <- function(simulation,
                               settings = settings,
                               logFolder = getwd()) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation, variationRange = settings$variationRange)
  sensitivityAnalysis$addParameterPaths(settings$variableParameterPaths)

  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
    showProgress = settings$showProgress,
    numberOfCores = settings$allowedCores
  )

  logWorkflow(message = paste0("Starting sensitivity analysis for path(s) ", paste(settings$variableParameterPaths, collapse = ", ")), pathFolder = logFolder)
  sensitivityAnalysisResults <- ospsuite::runSensitivityAnalysis(
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
                                   variationRange = 0.1,
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
#' @keywords internal
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
#' @keywords internal
runPopulationSensitivityAnalysis <- function(structureSet, settings, logFolder = getwd()) {
  resultsFileName <- trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(structureSet$simulationSet$simulationSetName), extension = "csv")
  popSAResultsIndexFile <- trimFileName(structureSet$popSensitivityAnalysisResultsIndexFile)

  sensitivityAnalysesResultsIndexFileDF <- getSAFileIndex(
    structureSet = structureSet,
    settings = settings,
    logFolder = logFolder,
    resultsFileName = resultsFileName
  )

  ids <- unique(sensitivityAnalysesResultsIndexFileDF$IndividualId)
  if (is.null(ids)) {
    return(NULL)
  }

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
#' @keywords internal
getPKResultsDataFrame <- function(structureSet) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  re.tStoreFileMetadata(access = "read", filePath = structureSet$pkAnalysisResultsFileNames)
  pkResults <- ospsuite::importPKAnalysesFromCSV(
    filePath = structureSet$pkAnalysisResultsFileNames,
    simulation = loadSimulationWithUpdatedPaths(simulationSet = structureSet$simulationSet, loadFromCache = TRUE)
  )

  pkResultsDataFrame <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses = pkResults)

  filteredPkResultsDf <- NULL

  for (op in structureSet$simulationSet$outputs) {
    # skip cases where pkParameters are not specified for the output
    if (is.null(op$pkParameters)) next

    pkParametersThisOutput <- unname(sapply(
      op$pkParameters,
      function(y) {
        y$pkParameter
      }
    ))

    filteredPkResultsDf <- rbind.data.frame(pkResultsDataFrame[(pkResultsDataFrame$QuantityPath %in% op$path) & (pkResultsDataFrame$Parameter %in% pkParametersThisOutput), ], filteredPkResultsDf)
  }

  return(filteredPkResultsDf)
}


#' @title getSAFileIndex
#' @description Function to build and write to CSV a dataframe that stores all
#' sensitivity analysis result files that will be output by a population sensitivity analysis.
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the population sensitivity analysis
#' @param logFolder folder where the logs are saved
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @keywords internal
getSAFileIndex <- function(structureSet,
                           settings,
                           logFolder,
                           resultsFileName) {
  quantileVec <- settings$quantileVec

  allPKResultsDataframe <- getPKResultsDataFrame(structureSet)
  sensitivityAnalysesResultsIndexFileDF <- NULL
  outputs <- unique(allPKResultsDataframe$QuantityPath)

  for (output in outputs) {
    pkParameters <- unique(allPKResultsDataframe$Parameter[allPKResultsDataframe$QuantityPath == output])
    for (pkParameter in pkParameters) {
      singleOuputSinglePKDataframe <- allPKResultsDataframe[(allPKResultsDataframe["QuantityPath"] == output) & (allPKResultsDataframe["Parameter"] == pkParameter), ]
      quantileResults <- getQuantileIndividualIds(singleOuputSinglePKDataframe, quantileVec)

      if (!isOfLength(quantileResults$ids, length(quantileVec))) {
        logWorkflow(
          message = messages$warningNoFinitePKParametersForSomeIndividuals(pkParameter, output, structureSet$simulationSet$simulationSetName),
          logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error),
          pathFolder = logFolder
        )
        next
      }
      saResultsByOutput <- data.frame(
        "Output" = output,
        "pkParameter" = pkParameter,
        "Quantile" = quantileVec,
        "Value" = quantileResults$values,
        "Unit" = quantileResults$units,
        "IndividualId" = quantileResults$ids,
        "Filename" = sapply(X = quantileResults$ids, FUN = getIndividualSAResultsFileName, resultsFileName)
      )
      sensitivityAnalysesResultsIndexFileDF <- rbind.data.frame(sensitivityAnalysesResultsIndexFileDF, saResultsByOutput)
    }
  }

  return(sensitivityAnalysesResultsIndexFileDF)
}


#' @title getIndividualSAResultsFileName
#' @description Function to build name of inidividual SA results file
#' @param resultsFileName root name of population sensitivity analysis results CSV files
#' @param individualId id of individual
#' @keywords internal
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
#' @import ospsuite
#' @import tlf
#' @keywords internal
plotMeanSensitivity <- function(structureSet,
                                logFolder = getwd(),
                                settings) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$sensitivityAnalysisResultsFileNames)
  saResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
    simulation = simulation,
    structureSet$sensitivityAnalysisResultsFileNames
  )
  sensitivityPlots <- list()
  sensitivityCaptions <- list()

  # It is possible to add options to SensitivityPlotSettings
  # That the mapping could account for (e.g. how to sort the sensitivities)
  sensitivityMapping <- tlf::TornadoDataMapping$new(
    x = "value",
    y = "parameter",
    fill = "parameter",
    color = "parameter"
  )
  # Plot Configuration overwritten by SensitivityPlotSettings object: settings
  sensitivityPlotConfiguration <- settings$plotConfiguration %||% tlf::TornadoPlotConfiguration$new()
  sensitivityPlotConfiguration$labels$xlabel$font$size <- settings$xAxisFontSize %||% sensitivityPlotConfiguration$labels$xlabel$font$size
  sensitivityPlotConfiguration$labels$ylabel$font$size <- settings$yAxisFontSize %||% sensitivityPlotConfiguration$labels$ylabel$font$size
  sensitivityPlotConfiguration$xAxis$font$size <- settings$xAxisFontSize %||% sensitivityPlotConfiguration$xAxis$font$size
  sensitivityPlotConfiguration$yAxis$font$size <- settings$yAxisFontSize %||% sensitivityPlotConfiguration$yAxis$font$size
  sensitivityPlotConfiguration$labels$xlabel$text <- settings$xLabel %||% sensitivityPlotConfiguration$labels$xlabel$text
  sensitivityPlotConfiguration$labels$ylabel$text <- settings$yLabel %||% sensitivityPlotConfiguration$labels$ylabel$text
  sensitivityPlotConfiguration$colorPalette <- settings$colorPalette %||% sensitivityPlotConfiguration$colorPalette

  for (output in structureSet$simulationSet$outputs) {
    validateIsIncluded(output$path, saResults$allQuantityPaths)
    pathLabel <- lastPathElement(output$path)
    for (pkParameter in output$pkParameters) {
      if (!isIncluded(pkParameter$pkParameter, saResults$allPKParameterNames)) {
        logWorkflow(
          message = messages$errorNotIncluded(pkParameter$pkParameter, saResults$allPKParameterNames),
          logTypes = c(LogTypes$Debug, LogTypes$Error),
          pathFolder = logFolder
        )
        next
      }
      parameterLabel <- lastPathElement(pkParameter$pkParameter)
      plotID <- paste0(parameterLabel, "-", pathLabel)

      pkSensitivities <- saResults$allPKParameterSensitivitiesFor(
        pkParameterName = pkParameter$pkParameter,
        outputPath = output$path,
        totalSensitivityThreshold = settings$totalSensitivityThreshold
      )

      indx1 <- 1 + settings$maximalParametersPerSensitivityPlot
      indx2 <- max(indx1, length(pkSensitivities))
      pkSensitivities <- pkSensitivities[-c(indx1:indx2)]

      sensitivityData <- data.frame(
        parameter = as.character(sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$parameterName
        })),
        value = as.numeric(sapply(pkSensitivities, function(pkSensitivity) {
          pkSensitivity$value
        })),
        stringsAsFactors = FALSE
      )
      # Add line breaks for display based on allowed size if parameters are too long
      sensitivityData$parameter <- getDisplaySensitivityParameters(sensitivityData$parameter,
        settings$maxLinesPerParameter,
        settings$maxWidthPerParameter,
        logFolder = logFolder
      )

      sensitivityPlot <- tlf::plotTornado(
        data = sensitivityData,
        dataMapping = sensitivityMapping,
        plotConfiguration = sensitivityPlotConfiguration
      )
      # Remove legend which is redundant from y axis
      sensitivityPlot <- tlf::setLegendPosition(sensitivityPlot, position = tlf::LegendPositions$none)
      sensitivityPlots[[plotID]] <- sensitivityPlot

      pkParameterCaption <- pkParameter$displayName %||% pkParameter$pkParameter
      sensitivityCaptions[[plotID]] <- captions$plotSensitivity$mean(pkParameterCaption, output$displayName)
    }
  }
  return(list(
    plots = sensitivityPlots,
    captions = sensitivityCaptions
  ))
}

#' @title lookupPKParameterDisplayName
#' @param output an Output object
#' @param pkParameter a string with the name of the pkParameter from the population sensitivity results index file
#' @return the display name of the input pk parameter or the string pkParameter itself if no display name found
#' @keywords internal
lookupPKParameterDisplayName <- function(output, pkParameter) {
  for (pk in output$pkParameters) {
    if (pkParameter == pk$pkParameter) {
      return(pk$displayName)
    }
  }
  return(pkParameter)
}

#' @title plotPopulationSensitivity
#' @description Retrieve list of plots of population sensitivity analyses across all populations
#' @param structureSets list of `SimulationStructure` objects
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the population sensitivity plot
#' @param workflowType Element from `PopulationWorkflowTypes`
#' @param xParameters selected parameters to be plotted in x axis
#' @param yParameters selected parameters to be plotted in y axis
#' @return a structured list of plots for each possible combination of pathID output-pkParameter that is found in sensitivity results index file
#' @import ospsuite
#' @keywords internal
plotPopulationSensitivity <- function(structureSets,
                                      logFolder = NULL,
                                      settings,
                                      workflowType = PopulationWorkflowTypes$parallelComparison,
                                      xParameters = NULL,
                                      yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")

  allPopsDf <- NULL
  saResultIndexFiles <- list()
  simulationList <- list()
  simulationSetDescriptor <- structureSets[[1]]$simulationSetDescriptor

  for (structureSet in structureSets) {
    sensitivityResultsFolder <- file.path(structureSet$workflowFolder, structureSet$sensitivityAnalysisResultsFolder)

    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

    re.tStoreFileMetadata(access = "read", filePath = structureSet$popSensitivityAnalysisResultsIndexFile)
    if (!(file.exists(structureSet$popSensitivityAnalysisResultsIndexFile))) next
    indexDf <- read.csv(file = structureSet$popSensitivityAnalysisResultsIndexFile)

    outputPaths <- sapply(structureSet$simulationSet$outputs, function(x) {
      x$path
    })
    populationName <- structureSet$simulationSet$simulationSetName

    saResultIndexFiles[[populationName]] <- structureSet$popSensitivityAnalysisResultsIndexFile
    simulationList[[populationName]] <- simulation

    for (output in structureSet$simulationSet$outputs) {
      outputPath <- output$path
      outputDisplayName <- output$displayName

      # opIndexDf is sub-dataframe of indexDf that has only the output paths outputPath in the Outputs column
      opIndexDf <- indexDf[indexDf$Output == outputPath, ]

      # pkParameters are all the entries in the pkParameters column of  opIndexDf
      pkParameters <- unique(opIndexDf$pkParameter)

      for (pkParameter in output$pkParameters) {
        pk <- pkParameter$pkParameter
        dfForPkAndOp <- getPopSensDfForPkAndOutput(
          simulation = simulation,
          sensitivityResultsFolder = sensitivityResultsFolder,
          indexDf = indexDf,
          output = outputPath,
          pkParameter = pk,
          totalSensitivityThreshold = settings$totalSensitivityThreshold,
          logFolder = logFolder
        )

        if (is.null(dfForPkAndOp)) {
          logWorkflow(
            message = messages$warningPopulationSensitivityPlotsNotAvailableForPKParameterOutputSimulationSet(
              pkParameter = pkParameter$pkParameter,
              output = outputPath,
              simulationSetName = structureSet$simulationSet$simulationSetName
            ),
            logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error),
            pathFolder = logFolder
          )
          next
        }

        pkDisplayName <- lookupPKParameterDisplayName(output = output, pkParameter = pk)

        populationNameCol <- rep(populationName, nrow(dfForPkAndOp))
        outputDisplayNameCol <- rep(outputDisplayName, nrow(dfForPkAndOp))
        pkDisplayNameCol <- rep(pkDisplayName, nrow(dfForPkAndOp))

        allPopsDf <- rbind.data.frame(
          allPopsDf,
          cbind(
            dfForPkAndOp,
            data.frame("Population" = populationNameCol),
            data.frame("OutputDisplayName" = outputDisplayNameCol),
            data.frame("PKDisplayName" = pkDisplayNameCol)
          )
        )
      }
    }
  }

  if (isOfLength(allPopsDf, 0)) {
    logWorkflow(
      message = messages$warningPopulationSensitivityPlotsNotAvailable(),
      logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error),
      pathFolder = logFolder
    )
    return()
  }

  # allPopsDf is a dataframe that holds the results of all sensitivity analyses for all populations
  # add any missing sensitivity results omitted when applying threshold in getPopSensDfForPkAndOutput
  # get set of unique combinations of outputs and pkParameters.  A plot is built for each combination.
  uniqueQuantitiesAndPKParameters <- unique(allPopsDf[, c("QuantityPath", "PKParameter")])

  for (n in 1:nrow(uniqueQuantitiesAndPKParameters)) {
    outputPath <- uniqueQuantitiesAndPKParameters$QuantityPath[n]
    pk <- uniqueQuantitiesAndPKParameters$PKParameter[n]

    sensitivityThisOpPk <- allPopsDf[(allPopsDf$QuantityPath %in% outputPath) & (allPopsDf$PKParameter %in% pk), ]

    # get list of all perturbation parameters used in this plot
    allParamsForThisOpPk <- unique(sensitivityThisOpPk$Parameter)

    # get the sensitivity results dataframe for this combination of output and pkParameter
    individualCombinationsThisOpPk <- unique(sensitivityThisOpPk[, c("Quantile", "IndividualId", "Population", "OutputDisplayName", "PKDisplayName")])

    # loop thru each individual in current combination of output and pkParameter
    for (m in 1:nrow(individualCombinationsThisOpPk)) {
      qu <- individualCombinationsThisOpPk$Quantile[m]
      id <- individualCombinationsThisOpPk$IndividualId[m]
      pop <- individualCombinationsThisOpPk$Population[m]
      opDN <- individualCombinationsThisOpPk$OutputDisplayName[m]
      pkDN <- individualCombinationsThisOpPk$PKDisplayName[m]

      # get list of all perturbation parameters for this one individual that are used in the plot for this combination of output and pkParameter
      allParamsForThisIndividual <- unique(sensitivityThisOpPk[(sensitivityThisOpPk$Quantile %in% qu) & (sensitivityThisOpPk$IndividualId %in% id) & (sensitivityThisOpPk$Population %in% pop), ]$Parameter)

      # create list of the parameters missing for this individual but otherwise shown in this plot for this combination of output and pkParameter
      missingParameters <- setdiff(allParamsForThisOpPk, allParamsForThisIndividual)

      # loop thru the missing parameters for the current individual
      for (parNumber in seq_along(missingParameters)) {

        # load the index file of SA results for this individual's population to get the name of the individual's sensitivity result file
        indx <- read.csv(saResultIndexFiles[[pop]])

        # get the name of the individual's sensitivity result file
        saResFileName <- indx[(indx$Output %in% outputPath) & (indx$pkParameter %in% pk) & (indx$Quantile %in% qu), ]$Filename

        # import SA results for individual
        individualSAResultsFilePath <- file.path(structureSet$workflowFolder, structureSet$sensitivityAnalysisResultsFolder, saResFileName)
        re.tStoreFileMetadata(access = "read", filePath = individualSAResultsFilePath)
        individualSAResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
          simulation = simulationList[[pop]],
          filePaths = individualSAResultsFilePath
        )

        # get the sensitivity for the missing parameter for this individual and for this combination of output and pkParameter
        missingSensivitity <- individualSAResults$pkParameterSensitivityValueFor(
          pkParameterName = as.character(pk),
          parameterName = missingParameters[parNumber],
          outputPath = as.character(outputPath)
        )

        if (!is.na(missingSensivitity)) {
          # create a sensitivity result row for the missing parameter for this individual and this combination of output and pkParameter
          saMissingParameter <- data.frame(
            "QuantityPath" = outputPath,
            "Parameter" = missingParameters[parNumber],
            "PKParameter" = pk,
            "Value" = missingSensivitity,
            "Quantile" = qu,
            "IndividualId" = id,
            "Population" = pop,
            "OutputDisplayName" = opDN,
            "PKDisplayName" = pkDN
          )

          # append to allPopsDf the row containing the missing parameter's sensitivity
          allPopsDf <- rbind(allPopsDf, saMissingParameter)
        }
      }
    }
  }

  #----- Population sensitivity plots
  sensitivityPlots <- list()
  sensitivityCaptions <- list()

  # Add line breaks for display based on allowed size if parameters are too long
  allPopsDf$Parameter <- getDisplaySensitivityParameters(as.character(allPopsDf$Parameter),
    settings$maxLinesPerParameter,
    settings$maxWidthPerParameter,
    logFolder = logFolder
  )

  # Translate quantile numeric values into sorted percentile names
  # to ensure binning by Percentiles is appropriate
  allPopsDf$Percentile <- factor(as.character(100 * allPopsDf$Quantile),
    levels = sort(unique(100 * allPopsDf$Quantile))
  )

  # It is possible to add options to SensitivityPlotSettings
  # That the mapping could account for (e.g. how to sort the sensitivities)
  sensitivityMapping <- tlf::TornadoDataMapping$new(
    x = "Value",
    y = "Parameter",
    color = "Percentile",
    shape = "Population"
  )

  # Plot Configuration overwritten by SensitivityPlotSettings object: settings
  # Bar option to FALSE makes a tornado plot with dots instead
  sensitivityPlotConfiguration <- settings$plotConfiguration %||% tlf::TornadoPlotConfiguration$new(bar = FALSE)
  sensitivityPlotConfiguration$labels$xlabel$font$size <- settings$xAxisFontSize %||% sensitivityPlotConfiguration$labels$xlabel$font$size
  sensitivityPlotConfiguration$labels$ylabel$font$size <- settings$yAxisFontSize %||% sensitivityPlotConfiguration$labels$ylabel$font$size
  sensitivityPlotConfiguration$xAxis$font$size <- settings$xAxisFontSize %||% sensitivityPlotConfiguration$xAxis$font$size
  sensitivityPlotConfiguration$yAxis$font$size <- settings$yAxisFontSize %||% sensitivityPlotConfiguration$yAxis$font$size
  sensitivityPlotConfiguration$labels$xlabel$text <- settings$xLabel %||% sensitivityPlotConfiguration$labels$xlabel$text
  sensitivityPlotConfiguration$labels$ylabel$text <- settings$yLabel %||% sensitivityPlotConfiguration$labels$ylabel$text
  sensitivityPlotConfiguration$colorPalette <- settings$colorPalette %||% sensitivityPlotConfiguration$colorPalette

  # Create plots and captions per unique output path and PK parameter
  for (outputIndex in 1:nrow(uniqueQuantitiesAndPKParameters)) {
    selectedPath <- uniqueQuantitiesAndPKParameters$QuantityPath[outputIndex]
    selectedPKParameter <- uniqueQuantitiesAndPKParameters$PKParameter[outputIndex]
    plotID <- paste(as.character(selectedPKParameter), as.character(selectedPath), sep = "_")
    plotID <- gsub(pattern = "|", replacement = "-", x = plotID, fixed = TRUE)

    outputRows <- allPopsDf$QuantityPath == selectedPath & allPopsDf$PKParameter == selectedPKParameter
    outputSensitivityData <- allPopsDf[outputRows, ]

    # Because of option `maximalParametersPerSensitivityPlot` available from settings
    # Parameters must first be ranked and selected according to their sensitivity
    # This must be done using levels because sensitivities are split between simulation sets and percentiles
    outputSensitivityData <- outputSensitivityData[order(-abs(outputSensitivityData$Value)), ]
    outputSensitivityData$Parameter <- factor(x = outputSensitivityData$Parameter, levels = rev(unique(outputSensitivityData$Parameter)))

    # Select most sensitive parameters
    allAvailableParameters <- levels(outputSensitivityData$Parameter)
    selectedParameters <- rev(allAvailableParameters)[1:min(settings$maximalParametersPerSensitivityPlot, length(allAvailableParameters))]
    selectedSensitivityData <- outputSensitivityData[outputSensitivityData$Parameter %in% selectedParameters, ]

    tornadoPlot <- tlf::plotTornado(
      data = selectedSensitivityData,
      dataMapping = sensitivityMapping,
      plotConfiguration = sensitivityPlotConfiguration
    )
    # Legends currently left aligned. This option may be changed on later version and use reEnv instead
    tornadoPlot <- tlf::setLegendPosition(tornadoPlot, position = tlf::LegendPositions$outsideTopLeft)
    # Legend titles are not currently handled by tlf, ggplot2 is needed to print descriptor and quantiles
    tornadoPlot <- tornadoPlot +
      ggplot2::theme(legend.title = ggplot2::element_text()) +
      ggplot2::labs(color = "Individual Percentile", shape = translateDescriptor(simulationSetDescriptor))

    sensitivityPlots[[plotID]] <- tornadoPlot

    sensitivityCaptions[[plotID]] <- captions$plotSensitivity$population(
      parameterName = selectedSensitivityData$PKDisplayName[1],
      pathName = selectedSensitivityData$OutputDisplayName[1],
      quantiles = levels(selectedSensitivityData$Percentile),
      simulationSetName = unique(selectedSensitivityData$Population),
      descriptor = simulationSetDescriptor
    )
  }
  return(list(
    plots = sensitivityPlots,
    captions = sensitivityCaptions
  ))
}

#' @title getPopSensDfForPkAndOutput
#' @description Retrieve dataframe of ranked and filtered population sensitivity results for a given PK parameter and model output pathID
#' @param simulation loaded from the used in the simulationFile path in the simulation set
#' @param sensitivityResultsFolder location of sensitivity analysis results
#' @param indexDf dataframe with contents of population sensitivity results index file
#' @param output pathID of output for which to obtain the population sensitivity results
#' @param pkParameter name of PK parameter for which to obtain the population sensitivity results
#' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
#' @param logFolder folder where the logs are saved
#' @return sortedFilteredIndividualsDfForPKParameter dataframe of population-wide sensitivity results for pkParameter and output
#' @import ospsuite
#' @keywords internal
getPopSensDfForPkAndOutput <- function(simulation,
                                       sensitivityResultsFolder,
                                       indexDf,
                                       output,
                                       pkParameter,
                                       totalSensitivityThreshold,
                                       logFolder) {
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

      individualSAResultsFilePath <- file.path(sensitivityResultsFolder, saResultsFileName)
      re.tStoreFileMetadata(access = "read", filePath = individualSAResultsFilePath)
      individualSAResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
        simulation = simulation,
        filePaths = individualSAResultsFilePath
      )

      # verify that pkParameter is included in sensitivity results for current individual.  If not, skip to next individual
      if (!isIncluded(pkParameter, individualSAResults$allPKParameterNames)) {
        logWorkflow(
          message = paste("For individualID", individualId, ":", messages$errorNotIncluded(pkParameter, individualSAResults$allPKParameterNames)),
          logTypes = c(LogTypes$Debug, LogTypes$Error),
          pathFolder = logFolder
        )
        next
      }

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
        dfList[[n]] <- cbind(individualDf, data.frame("Quantile" = rep(quantile, nrow(individualDf)), "IndividualId" = rep(individualId, nrow(individualDf))))
      }
    }

    # Merge dFlist into one dataframe for all individuals for current output and current pkParameter
    individualsDfForPKParameter <- do.call(rbind.data.frame, dfList)
  }

  return(individualsDfForPKParameter)
}

#' @title getPkOutputIndexDf
#' @description Function to filter the population results index file for given pkParameter and output
#' @param indexDf dataframe containing summary of sensitivity results
#' @param output pathID of output for which to obtain the population sensitivity results
#' @param pkParameter name of PK parameter for which to obtain the population sensitivity results
#' @return pkOutputIndexDf dataframe containing index of files containing population sensitivity analysis results conducted for given output and pkParameter
#' @keywords internal
getPkOutputIndexDf <- function(indexDf, pkParameter, output) {
  pkOutputIndexDf <- indexDf[(indexDf$pkParameter == pkParameter) & (indexDf$Output == output), ]
  return(pkOutputIndexDf)
}

#' @title getDefaultTotalSensitivityThreshold
#' @description return the default totalSensitivityThreshold to be used in a population sensitivity analysis plot
#' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
#' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
#' @keywords internal
getDefaultTotalSensitivityThreshold <- function(totalSensitivityThreshold = NULL, variableParameterPaths = NULL) {
  totalSensitivityThreshold <- totalSensitivityThreshold %||% ifnotnull(variableParameterPaths, 1, 0.9)
  return(totalSensitivityThreshold)
}

#' @title getDisplaySensitivityParameters
#' @description Displayed parameter names for sensitivity breaking lines when too long
#' @param parameterNames vector parameters displayed in the sensitivity plots
#' @param maxLinesPerParameter maxLinesPerParameter maximum number of lines allowed per displayed parameters
#' @param maxWidthPerParameter maximum number of characters allowed per lines of displayed parameters
#' @param logFolder folder where the logs are saved
#' @return Displayed parameter names for sensitivity breaking lines when too long
#' @export
getDisplaySensitivityParameters <- function(parameterNames, maxLinesPerParameter, maxWidthPerParameter, logFolder = getwd()) {
  # Get total parameters lengths
  parametersTotalLengths <- nchar(parameterNames)

  # Check parameters to split
  parametersToSplit <- parametersTotalLengths > maxWidthPerParameter
  if (sum(parametersToSplit) == 0) {
    return(parameterNames)
  }

  logWorkflow(
    message = paste0(
      "Parameters '", paste0(parameterNames[parametersToSplit], collapse = "', '"),
      "' were split for display due to number of characters higher than ", maxWidthPerParameter
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  # Check how many line break required
  numberOfSplits <- floor(parametersTotalLengths / maxWidthPerParameter)
  numberOfLines <- numberOfSplits + 1

  # Flag parameters reaching maximum allowed number of lines
  if (sum(numberOfLines > maxLinesPerParameter) > 0) {
    logWorkflow(
      message = paste0(
        "Maximum allowed number of lines (", maxLinesPerParameter,
        ") reached for parameters '", paste0(parameterNames[numberOfLines > maxLinesPerParameter], collapse = "', '")
      ),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  # Splits cannot create more lines than max lines
  numberOfSplits[numberOfLines > maxLinesPerParameter] <- maxLinesPerParameter - 1
  numberOfLines <- numberOfSplits + 1

  # displayParameterNames <- parameterNames
  for (parameterIndex in seq_along(parameterNames)) {
    if (numberOfSplits[parameterIndex] == 0) {
      next
    }
    dashSplits <- as.numeric(gregexpr(pattern = "-", parameterNames[parameterIndex])[[1]])
    spaceSplits <- as.numeric(gregexpr(pattern = " ", parameterNames[parameterIndex])[[1]])
    possibleSplits <- sort(c(dashSplits[dashSplits > 0], spaceSplits[spaceSplits > 0]))

    # Optimal splits are at equal width
    splitWidth <- parametersTotalLengths[parameterIndex] / numberOfLines[parameterIndex]

    # Select breaks from possible splits and optimal splits
    actualSplits <- getSplitValues(possibleSplits, splitWidth, numberOfSplits[parameterIndex])
    splitFirst <- c(1, actualSplits + 1)
    splitLast <- c(actualSplits, parametersTotalLengths[parameterIndex])

    # Split the display parameter at selected values and add line breaks
    parameterNames[parameterIndex] <- paste0(substring(parameterNames[parameterIndex], first = splitFirst, last = splitLast),
      collapse = "\n"
    )
  }
  return(parameterNames)
}

getSplitValues <- function(possibleSplits, splitWidth, numberOfSplits) {
  # Optimal splits are at equal width
  optimalSplits <- floor(cumsum(rep(splitWidth, numberOfSplits)))
  for (splitIndex in seq_along(optimalSplits)) {
    if (isOfLength(possibleSplits, 0)) {
      return(optimalSplits)
    }
    positionDifference <- min(abs(possibleSplits - optimalSplits[splitIndex]))
    # If closest possible split too far, use optimal split
    if (positionDifference > splitWidth) {
      next
    }

    # If available use the available split and remove it from other loops
    closestAvailableSplitIndex <- which.min(abs(possibleSplits - optimalSplits[splitIndex]))
    optimalSplits[splitIndex] <- possibleSplits[closestAvailableSplitIndex]
    possibleSplits <- possibleSplits[-closestAvailableSplitIndex]
  }
  return(optimalSplits)
}
