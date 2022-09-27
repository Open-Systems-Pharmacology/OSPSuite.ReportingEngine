#' @title plotMeanGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list with `plots`, `tables` and `residuals` objects to be saved
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ggplot2
#' @import ospsuite.utils
#' @keywords internal
plotMeanGoodnessOfFit <- function(structureSet, settings = NULL) {
  validateIsOfType(structureSet, "SimulationStructure")

  initializeExpression <- parse(text = paste0(
    c("observedData", "simulatedData", "lloqData", "residualsData", "residualsMetaData"),
    " <- NULL"
  ))
  eval(initializeExpression)

  goodnessOfFitResults <- list()
  goodnessOfFitResiduals <- list()

  # Load observed and simulated data
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResult <- ospsuite::importResultsFromCSV(simulation, structureSet$simulationResultFileNames)

  observedResult <- loadObservedDataFromSimulationSet(structureSet$simulationSet)

  outputSelections <- structureSet$simulationSet$outputs
  if (!is.null(settings$outputSelections)) {
    availableOutputs <- sapply(structureSet$simulationSet$outputs, function(output) {
      output$path
    })
    selectedOutputs <- availableOutputs %in% settings$outputSelections
    outputSelections <- structureSet$simulationSet$outputs[selectedOutputs]
  }
  outputSimulatedMetaData <- list()
  for (output in outputSelections) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getSimulatedResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet)
    outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
    lloqData <- rbind.data.frame(lloqData, outputObservedResults$lloq)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileData <- rbind.data.frame(observedData, lloqData, simulatedData)
  timeProfileMapping <- list(x = "Time", y = "Concentration", group = "Legend")

  resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, "time_profile_data")

  goodnessOfFitResults[[resultID]] <- saveTaskResults(
    id = resultID,
    table = timeProfileData,
    includeTable = FALSE
  )

  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  # Get list of total, first application, and last application ranges
  # Note: currently simulationSet$applicationRanges includes only logicals
  # timeOffset could also be included into the process
  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet)

  # For multiple applications, include text results corresponding to application sub section
  hasMultipleApplications <- sum(sapply(timeRanges, function(timeRange) {
    timeRange$keep
  })) > 1

  # If one or no application, field 'keep' for time range other than total is FALSE
  for (timeRange in timeRanges) {
    if (!timeRange$keep) {
      next
    }
    if (hasMultipleApplications) {
      resultID <- defaultFileNames$resultID(
        length(goodnessOfFitResults) + 1,
        "sub_section_title",
        timeRange$name
      )

      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        textChunk = getTimeRangeCaption(timeRange$name, "goodness-of-fit", structureSet$simulationSet$simulationSetName),
        includeTextChunk = TRUE
      )
    }

    timeProfilePlotResults <- getTimeProfilePlotResults(
      "mean",
      timeRange$values,
      simulatedData,
      observedData,
      lloqData,
      metaDataFrame,
      timeProfileMapping,
      structureSet,
      settings
    )

    for (plotID in names(timeProfilePlotResults$plots)) {
      resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, plotID, timeRange$name)
      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = timeProfilePlotResults$plots[[plotID]],
        plotCaption = timeProfilePlotResults$captions[[plotID]]
      )
    }

    if (isEmpty(residualsData)) {
      next
    }

    residualsPlotResults <- getResidualsPlotResults(
      timeRange$values,
      residualsData,
      metaDataFrame,
      structureSet,
      settings
    )

    for (plotID in names(residualsPlotResults$plots)) {
      resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, plotID, timeRange$name)
      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = residualsPlotResults$plots[[plotID]],
        plotCaption = residualsPlotResults$captions[[plotID]]
      )
    }

    goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
    residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
  }

  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  return(list(
    results = goodnessOfFitResults,
    residuals = allResiduals
  ))
}

#' @title getSimulatedResultsFromOutput
#' @description
#' Get simulated data from an Output object
#' @param simulationPathResults list with simulated data included
#' @param output An `Output` object
#' @param simulationQuantity Dimension/quantity for unit conversion of dependent variable
#' @param molWeight Molar weight for unit conversion of dependent variable
#' @param structureSet `SimulationStructure` object
#' @return list of data and metaData
#' @import ospsuite.utils
#' @keywords internal
getSimulatedResultsFromOutput <- function(simulationPathResults, output, simulationQuantity, molWeight, structureSet) {
  simulationSet <- structureSet$simulationSet
  outputConcentration <- simulationPathResults$data[, output$path]
  # Output object is updated: displayUnit cannot be empty anymore
  output$displayUnit <- output$displayUnit %||% simulationQuantity$displayUnit
  
  outputSimulatedData <- data.frame(
    "Time" = ospsuite::toUnit(
      "Time", 
      simulationPathResults$data[, "Time"], 
      simulationSet$timeUnit
      ),
    "Concentration" = ospsuite::toUnit(
      simulationQuantity,
      simulationPathResults$data[, output$path],
      output$displayUnit,
      molWeight = molWeight
    ),
    "Legend" = captions$plotGoF$meanLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      pathName = output$displayName
    ),
    "Legend" = captions$plotGoF$resLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      pathName = output$displayName
    ),
    "Path" = output$path
  )

  outputSimulatedMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit
    ),
    "Path" = output$path
  )
  return(list(
    data = outputSimulatedData,
    metaData = outputSimulatedMetaData
  ))
}

#' @title getResiduals
#' @description This function may be reshape to be more generic later on
#' Currently, the input variable data is a data.frame with "Time", "Concentration" and "Legend"
#' The function get the simulated data with the time the closest to the observed data times
#' @param observedData data.frame of time profile observed data
#' @param simulatedData data.frame of time profile simulated data
#' @param residualScale Scale for calculation of residuals as included in enum `ResidualScales`
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
getResiduals <- function(observedData,
                         simulatedData,
                         residualScale = ResidualScales$Logarithmic) {
  if (isOfLength(observedData, 0)) {
    return()
  }
  # Time matrix to match observed time with closest simulation time
  # This method assumes that there simulated data are dense enough to capture observed data
  obsTimeMatrix <- matrix(observedData[, "Time"], nrow(simulatedData), nrow(observedData), byrow = TRUE)
  simTimeMatrix <- matrix(simulatedData[, "Time"], nrow(simulatedData), nrow(observedData))

  timeMatchedData <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))

  residualValues <- calculateResiduals(
    simulatedData = simulatedData[timeMatchedData, "Concentration"],
    observedData = observedData[, "Concentration"],
    residualScale = residualScale
  )

  residualValues <- rep(NA, nrow(observedData))
  if (isIncluded(residualScale, ResidualScales$Logarithmic)) {
    residualValues <- log(observedData[, "Concentration"]) - log(simulatedData[timeMatchedData, "Concentration"])
  }
  if (isIncluded(residualScale, ResidualScales$Linear)) {
    residualValues <- (observedData[, "Concentration"] - simulatedData[timeMatchedData, "Concentration"])
  }

  residualsData <- data.frame(
    "Time" = observedData[, "Time"],
    "Observed" = observedData[, "Concentration"],
    "Simulated" = simulatedData[timeMatchedData, "Concentration"],
    "Residuals" = residualValues,
    "Legend" = simulatedData[timeMatchedData, "ResidualsLegend"],
    "Path" = observedData[, "Path"]
  )
  return(residualsData)
}

#' @title plotPopulationGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list with `plots`, `tables` and `residuals` objects to be saved
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ggplot2
#' @import ospsuite.utils
#' @keywords internal
plotPopulationGoodnessOfFit <- function(structureSet, settings = NULL) {
  validateIsOfType(structureSet, "SimulationStructure")

  initializeExpression <- parse(text = paste0(
    c("observedData", "simulatedData", "lloqData", "residualsData", "residualsMetaData"),
    " <- NULL"
  ))
  eval(initializeExpression)
  goodnessOfFitResults <- list()
  goodnessOfFitResiduals <- list()

  # Load observed and simulated data
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResult <- ospsuite::importResultsFromCSV(simulation, structureSet$simulationResultFileNames)

  observedResult <- loadObservedDataFromSimulationSet(structureSet$simulationSet)

  outputSimulatedMetaData <- list()
  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getPopulationResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet, settings)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet)

    outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
    lloqData <- rbind.data.frame(lloqData, outputObservedResults$lloq)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  resultID <- defaultFileNames$resultID(
    length(goodnessOfFitResults) + 1,
    "time_profile_simulated_data"
  )
  goodnessOfFitResults[[resultID]] <- saveTaskResults(
    id = resultID,
    table = simulatedData,
    includeTable = FALSE
  )

  # Save goodness of fit data for reference to be compared with other simulation sets
  referenceData <- list(
    simulatedData = simulatedData,
    observedData = observedData,
    lloqData = lloqData,
    residualsData = residualsData,
    timeOffset = structureSet$simulationSet$timeOffset
  )

  timeProfileMapping <- list(x = "Time", y = "Concentration", ymin = "ymin", ymax = "ymax", group = "Legend")
  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet)

  # For multiple applications, include text results corresponding to application sub section
  hasMultipleApplications <- sum(sapply(timeRanges, function(timeRange) {
    timeRange$keep
  })) > 1

  for (timeRange in timeRanges) {
    if (!timeRange$keep) {
      next
    }
    if (hasMultipleApplications) {
      resultID <- defaultFileNames$resultID(
        length(goodnessOfFitResults) + 1,
        "sub_section_title",
        removeForbiddenLetters(structureSet$simulationSet$simulationName),
        timeRange$name
      )

      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        textChunk = getTimeRangeCaption(timeRange$name, "goodness-of-fit", structureSet$simulationSet$simulationSetName),
        includeTextChunk = TRUE
      )
    }

    timeProfilePlotResults <- getTimeProfilePlotResults(
      "population",
      timeRange$values,
      simulatedData,
      observedData,
      lloqData,
      metaDataFrame,
      timeProfileMapping,
      structureSet,
      settings
    )

    for (plotID in names(timeProfilePlotResults$plots)) {
      resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, plotID, timeRange$name)
      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = timeProfilePlotResults$plots[[plotID]],
        plotCaption = timeProfilePlotResults$captions[[plotID]]
      )
    }

    if (isEmpty(residualsData)) {
      next
    }

    residualsPlotResults <- getResidualsPlotResults(
      timeRange$values,
      residualsData,
      metaDataFrame,
      structureSet,
      settings
    )

    for (plotID in names(residualsPlotResults$plots)) {
      resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, plotID, timeRange$name)
      goodnessOfFitResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = residualsPlotResults$plots[[plotID]],
        plotCaption = residualsPlotResults$captions[[plotID]]
      )
    }

    goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
    residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
  }

  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  return(list(
    results = goodnessOfFitResults,
    referenceData = referenceData,
    residuals = allResiduals
  ))
}

#' @title getPopulationResultsFromOutput
#' @description
#' Get simulated population data from an Output object
#' @param simulationPathResults list with simulated data included
#' @param output An `Output` object
#' @param simulationQuantity Dimension/quantity for unit conversion of dependent variable
#' @param molWeight Molar weight for unit conversion of dependent variable
#' @param structureSet `SimulationStructure` object
#' @param settings TaskSetting object
#' @return list of data and metaData
#' @import ospsuite.utils
#' @keywords internal
getPopulationResultsFromOutput <- function(simulationPathResults, output, simulationQuantity, molWeight, structureSet, settings = NULL) {
  simulationSet <- structureSet$simulationSet
  timeProfileStatistics <- settings$getStatistics()
  aggregateNames <- c("y", "ymin", "ymax")
  aggregateFunctions <- sapply(
    timeProfileStatistics[aggregateNames],
    function(functionName) {
      match.fun(functionName)
    }
  )

  # Get the aggregated results: mean, median and range along time bins
  aggregateSummary <- tlf::AggregationSummary$new(
    data = simulationPathResults$data,
    metaData = simulationPathResults$metaData,
    xColumnNames = "Time",
    yColumnNames = output$path,
    aggregationFunctionsVector = aggregateFunctions,
    aggregationFunctionNames = aggregateNames
  )

  # Conversion to user-defined units
  # Output object is updated: displayUnit cannot be empty anymore
  output$displayUnit <- output$displayUnit %||% simulationQuantity$displayUnit
  # Expressions are used to prevent copy/paste of the code for mean, median and range conversions
  aggregateData <- aggregateSummary$dfHelper
  aggregateData$Time <- ospsuite::toUnit("Time", aggregateData$Time, simulationSet$timeUnit)

  convertExpressions <- parse(text = paste0(
    "aggregateData$", aggregateNames, "<- ",
    "ospsuite::toUnit(simulationQuantity, aggregateData$", aggregateNames, 
    ", output$displayUnit, molWeight = molWeight)"
  ))
  eval(convertExpressions)

  outputSimulatedData <- aggregateData
  outputSimulatedData$Concentration <- outputSimulatedData$y
  outputSimulatedData$Legend <- captions$plotGoF$populationLegend(
    simulationSetName = simulationSet$simulationSetName,
    descriptor = structureSet$simulationSetDescriptor,
    statistics = timeProfileStatistics,
    pathName = output$displayName
  )
  outputSimulatedData$ResidualsLegend <- captions$plotGoF$resLegend(
    simulationSetName = simulationSet$simulationSetName,
    descriptor = structureSet$simulationSetDescriptor,
    pathName = output$displayName
  )
  outputSimulatedData$Path <- output$path

  outputSimulatedMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit %||% simulationQuantity$displayUnit
    ),
    "Path" = output$path
  )
  return(list(
    data = outputSimulatedData,
    metaData = outputSimulatedMetaData
  ))
}

#' @title plotMeanTimeProfile
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param lloqData data.frame of lloq data
#' @param metaData meta data on `data`
#' @param dataMapping A list mapping `x`, `y` and `group` from datasets
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotMeanTimeProfile <- function(simulatedData,
                                observedData = NULL,
                                lloqData = NULL,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  simulatedDataMapping <- tlf::TimeProfileDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    group = dataMapping$group
  )
  observedDataMapping <- tlf::ObservedDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    group = dataMapping$group
  )

  plotConfiguration <- plotConfiguration %||%
    TimeProfilePlotConfiguration$new(
      data = simulatedData,
      metaData = metaData,
      dataMapping = simulatedDataMapping
    )
  plotConfiguration <- updatePlotConfigurationTimeTicks(simulatedData, metaData, simulatedDataMapping, plotConfiguration)

  timeProfilePlot <- tlf::plotTimeProfile(
    data = simulatedData,
    metaData = metaData,
    dataMapping = simulatedDataMapping,
    observedData = rbind.data.frame(observedData, lloqData),
    observedDataMapping = observedDataMapping,
    plotConfiguration = plotConfiguration
  )

  return(timeProfilePlot)
}

#' @title plotMeanTimeProfileLog
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param lloqData data.frame of lloq data
#' @param metaData meta data on `data`
#' @param dataMapping A list mapping `x`, `y` and `group` from datasets
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotMeanTimeProfileLog <- function(simulatedData,
                                   observedData = NULL,
                                   lloqData = NULL,
                                   metaData = NULL,
                                   dataMapping = NULL,
                                   plotConfiguration = NULL) {
  # Remove 0 values from simulated and observed data
  # TODO: use dplyr to refactor the selection
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$y)
  observedData <- removeNegativeValues(observedData, dataMapping$y)
  lloqData <- removeNegativeValues(lloqData, dataMapping$y)
  
  logObservedValues <- NULL
  logLLOQValues <- NULL
  if(!isEmpty(observedData)){
    logObservedValues <- observedData[, dataMapping$y]
  }
  if(!isEmpty(lloqData)){
    logLLOQValues <- lloqData[, dataMapping$y]
  }
  # Get the nice auto scaling of the log data
  yAxisLimits <- autoAxesLimits(c(
    simulatedData[, dataMapping$y],
    simulatedData[, dataMapping$ymin],
    simulatedData[, dataMapping$ymax],
    logObservedValues,
    logLLOQValues
  ),
  scale = "log"
  )
  yAxisTicks <- autoAxesTicksFromLimits(yAxisLimits)

  meanTimeProfile <- plotMeanTimeProfile(
    simulatedData,
    observedData = observedData,
    lloqData = lloqData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = NULL
  )

  meanTimeProfileLog <- tlf::setYAxis(
    meanTimeProfile,
    scale = tlf::Scaling$log,
    limits = yAxisLimits,
    ticks = yAxisTicks
  )
  return(meanTimeProfileLog)
}

#' @title plotPopulationTimeProfile
#' @description Plot time profile for population model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param lloqData data.frame of lloq data
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotPopulationTimeProfile <- function(simulatedData,
                                      observedData = NULL,
                                      lloqData = NULL,
                                      dataMapping = NULL,
                                      metaData = NULL,
                                      plotConfiguration = NULL) {

  simulatedData <- removeMissingValues(simulatedData, dataMapping$y)
  simulatedData <- removeMissingValues(simulatedData, dataMapping$ymin)
  simulatedData <- removeMissingValues(simulatedData, dataMapping$ymax)
  observedData <- removeMissingValues(observedData, dataMapping$y)
  lloqData <- removeMissingValues(lloqData, dataMapping$y)
  
  # metaData needs to be transferred to ymin and ymax
  # so that y label shows dimension [unit] by default
  metaData$x <- metaData$Time
  metaData$ymin <- metaData$Concentration
  metaData$ymax <- metaData$Concentration

  simulatedDataMapping <- tlf::TimeProfileDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    ymin = dataMapping$ymin,
    ymax = dataMapping$ymax,
    group = dataMapping$group
  )
  observedDataMapping <- tlf::ObservedDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    group = dataMapping$group
  )

  plotConfiguration <- plotConfiguration %||%
    TimeProfilePlotConfiguration$new(
      data = simulatedData,
      metaData = metaData,
      dataMapping = simulatedDataMapping
    )
  plotConfiguration <- updatePlotConfigurationTimeTicks(simulatedData, metaData, simulatedDataMapping, plotConfiguration)

  timeProfilePlot <- tlf::plotTimeProfile(
    data = simulatedData,
    metaData = metaData,
    dataMapping = simulatedDataMapping,
    observedData = rbind.data.frame(observedData, lloqData),
    observedDataMapping = observedDataMapping,
    plotConfiguration = plotConfiguration
  )
  
  return(timeProfilePlot)
}

#' @title plotPopulationTimeProfileLog
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param lloqData data.frame of lloq data
#' @param metaData meta data on `data`
#' @param dataMapping A list mapping `x`, `y` and `group` from datasets
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotPopulationTimeProfileLog <- function(simulatedData,
                                         observedData = NULL,
                                         lloqData = NULL,
                                         metaData = NULL,
                                         dataMapping = NULL,
                                         plotConfiguration = NULL) {
  # Remove 0 values from simulated and observed data
  # TODO: use dplyr to refactor the selection
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$y)
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$ymin)
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$ymax)
  observedData <- removeNegativeValues(observedData, dataMapping$y)
  lloqData <- removeNegativeValues(lloqData, dataMapping$y)
  
  logObservedValues <- NULL
  logLLOQValues <- NULL
  if(!isEmpty(observedData)){
    logObservedValues <- observedData[, dataMapping$y]
  }
  if(!isEmpty(lloqData)){
    logLLOQValues <- lloqData[, dataMapping$y]
  }
  # Get the nice auto scaling of the log data
  yAxisLimits <- autoAxesLimits(c(
    simulatedData[, dataMapping$y],
    simulatedData[, dataMapping$ymin],
    simulatedData[, dataMapping$ymax],
    logObservedValues,
    logLLOQValues
  ),
  scale = "log"
  )
  yAxisTicks <- autoAxesTicksFromLimits(yAxisLimits)

  populationTimeProfile <- plotPopulationTimeProfile(
    simulatedData,
    observedData = observedData,
    lloqData = lloqData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = NULL
  )

  populationTimeProfile <- tlf::setYAxis(
    populationTimeProfile,
    scale = tlf::Scaling$log,
    limits = yAxisLimits,
    ticks = yAxisTicks
  )
  return(populationTimeProfile)
}

#' @title getSimulationTimeRanges
#' @description Get time ranges for time profile plots according to applications and user defined settings
#' @param simulation A `Simulation` object
#' @param path Field `path` from `Output` object
#' @param simulationSet A `SimulationSet` or `PopulationSimulationSet` object
#' @return Lists including `values` and `name` of time ranges.
#' Also includes logical field `keep` to define if a specific application range is kept in report.
#' @keywords internal
getSimulationTimeRanges <- function(simulation, path, simulationSet) {
  timeUnit <- simulationSet$timeUnit
  applicationRanges <- simulationSet$applicationRanges
  # Initialize output
  timeRanges <- list(
    total = list(
      name = ApplicationRanges$total,
      keep = applicationRanges[[ApplicationRanges$total]],
      values = NULL
    ),
    firstApplication = list(
      name = ApplicationRanges$firstApplication,
      keep = FALSE,
      values = NULL
    ),
    lastApplication = list(
      name = ApplicationRanges$lastApplication,
      keep = FALSE,
      values = NULL
    )
  )

  # Get applications
  applications <- simulation$allApplicationsFor(path)
  applicationTimes <- 0
  if (!isOfLength(applications, 0)) {
    applicationTimes <- sapply(applications, function(application) {
      application$startTime$value
    })
  }
  # Get all ranges of simulation ranked defined by application intervals
  simulationRanges <- c(applicationTimes, simulation$outputSchema$endTime)
  simulationRanges <- sort(ospsuite::toUnit("Time", simulationRanges, timeUnit))

  # Store number of applications and their ranges
  logDebug(messages$numberOfApplications(length(applications), path, simulation$name))
  logDebug(messages$timeRangesForSimulation(paste0(simulationRanges, collapse = "', '"), simulation$name))

  # Define ranges for output
  # Depending on expected behaviour of settings$applicationRange
  # It would be possible to set these values
  timeRanges$total$values <- c(min(simulationRanges), max(simulationRanges))

  # Flag simulationRanges prior to timeOffset
  timeOffsetFlag <- simulationRanges < simulationSet$timeOffset
  if (any(timeOffsetFlag)) {
    logError(messages$warningApplicationsBeforeTimeOffset(
      sum(timeOffsetFlag),
      paste0(simulationRanges[timeOffsetFlag], collapse = ", "),
      timeUnit,
      simulationSet$timeOffset, simulationSet$simulationSetName
    ))
  }

  # Case of multiple applications, get first and last
  if (!isOfLength(simulationRanges, 2)) {
    # First application becomes first application after timeOffset
    timeRanges$firstApplication$values <- utils::head(simulationRanges[!timeOffsetFlag], 2)
    timeRanges$lastApplication$values <- utils::tail(simulationRanges, 2)
    timeRanges$firstApplication$keep <- applicationRanges[[ApplicationRanges$firstApplication]]
    timeRanges$lastApplication$keep <- applicationRanges[[ApplicationRanges$lastApplication]]
  }

  return(timeRanges)
}

#' @title asTimeAfterDose
#' @description Transform time data as time after dose data
#' @param data A data.frame that includes the variable `Time`
#' @param timeOffset Value shifting time
#' @param maxTime Optional maximum time value before shift
#' @return A data.frame
#' @keywords internal
asTimeAfterDose <- function(data, timeOffset, maxTime = NULL) {
  if (isOfLength(data, 0)) {
    # Return empty data.frame (consitent class with data[dataFilter, ])
    return(data.frame())
  }
  dataFilter <- data$Time >= timeOffset
  if (!is.null(maxTime)) {
    dataFilter <- dataFilter & data$Time <= maxTime
  }
  if (!any(dataFilter)) {
    return(data.frame())
  }
  data$Time <- data$Time - timeOffset
  return(data[dataFilter, ])
}

#' @title getMetaDataFrame
#' @description Get time profile metaData as a data.frame
#' @param listOfMetaData list including `Path` and `Concentration`
#' @return A data.frame with variables `path`, `dimension` and `unit`
#' @keywords internal
getMetaDataFrame <- function(listOfMetaData) {
  metaDataFrame <- data.frame(
    path = as.character(sapply(listOfMetaData, function(metaData) {
      metaData$Path
    })),
    dimension = as.character(sapply(listOfMetaData, function(metaData) {
      metaData$Concentration$dimension
    })),
    unit = as.character(sapply(listOfMetaData, function(metaData) {
      metaData$Concentration$unit
    })),
    stringsAsFactors = FALSE
  )
  # Update dimension name for better caption in plot label
  selectedDimensions <- metaDataFrame$dimension %in% c("Concentration (mass)", "Concentration (molar)")
  metaDataFrame$dimension[selectedDimensions] <- "Concentration"
  return(metaDataFrame)
}

#' @title getTimeProfilePlotResults
#' @description Get plots and their captions for mean or population time profiles
#' @param workflowType `"mean"` or `"population"` workflow
#' @param timeRange array of time values defining range of simulated data
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param lloqData data.frame of observed lloq data
#' @param metaDataFrame metaData represented as a data.frame
#' @param timeProfileMapping list of variables to map in the time profile plots
#' @param structureSet A `SimulationStructure` object
#' @param settings Optional settings for the plots. In particular, includes reference data for population time profile.
#' @return List of `plots` and their `captions`
#' @keywords internal
getTimeProfilePlotResults <- function(workflowType, timeRange, simulatedData, observedData = NULL, lloqData = NULL, metaDataFrame, timeProfileMapping, structureSet, settings = NULL) {
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  # Add reference simulated data, if any, to the existing data
  # rbind.data.frame enforces data.frame type and nrow can be used as is
  # Reset time based on timeOffset or application range
  simulatedData <- rbind.data.frame(
    asTimeAfterDose(
      settings$referenceData$simulatedData,
      min(timeRange) + settings$referenceData$timeOffset,
      max(timeRange) + settings$referenceData$timeOffset
    ),
    asTimeAfterDose(
      simulatedData,
      min(timeRange) + structureSet$simulationSet$timeOffset,
      max(timeRange) + structureSet$simulationSet$timeOffset
    )
  )
  if (nrow(simulatedData) == 0) {
    stop(messages$dataIncludedInTimeRange(
      nrow(simulatedData),
      timeRange + structureSet$simulationSet$timeOffset,
      structureSet$simulationSet$timeUnit, "simulated"
    ))
  }
  observedData <- asTimeAfterDose(
    observedData,
    min(timeRange) + structureSet$simulationSet$timeOffset,
    max(timeRange) + structureSet$simulationSet$timeOffset
  )
  lloqData <- asTimeAfterDose(
    lloqData,
    min(timeRange) + structureSet$simulationSet$timeOffset,
    max(timeRange) + structureSet$simulationSet$timeOffset
  )

  # Add reference observed data only if option is set to true
  # isTRUE is used for mean model workflows that have a NULL field
  if (isTRUE(structureSet$simulationSet$plotReferenceObsData)) {
    observedData <- rbind.data.frame(
      asTimeAfterDose(
        settings$referenceData$observedData,
        min(timeRange) + settings$referenceData$timeOffset,
        max(timeRange) + settings$referenceData$timeOffset
      ),
      observedData
    )
    lloqData <- rbind.data.frame(
      asTimeAfterDose(
        settings$referenceData$lloqData,
        min(timeRange) + settings$referenceData$timeOffset,
        max(timeRange) + settings$referenceData$timeOffset
      ),
      lloqData
    )
  }

  logDebug(messages$dataIncludedInTimeRange(
    nrow(simulatedData),
    timeRange + structureSet$simulationSet$timeOffset,
    structureSet$simulationSet$timeUnit,
    "simulated"
  ))
  logDebug(messages$dataIncludedInTimeRange(
    nrow(observedData),
    timeRange + structureSet$simulationSet$timeOffset,
    structureSet$simulationSet$timeUnit,
    "observed"
  ))
  logDebug(messages$dataIncludedInTimeRange(
    nrow(lloqData),
    timeRange + structureSet$simulationSet$timeOffset,
    structureSet$simulationSet$timeUnit,
    "lloq observed"
  ))

  for (unit in unique(metaDataFrame$unit)) {
    selectedDimension <- utils::head(metaDataFrame$dimension[metaDataFrame$unit %in% unit], 1)
    selectedPaths <- metaDataFrame$path[metaDataFrame$unit %in% unit]
    selectedSimulatedData <- simulatedData[simulatedData$Path %in% selectedPaths, ]
    selectedObservedData <- observedData[observedData$Path %in% selectedPaths, ]
    selectedLloqData <- lloqData[lloqData$Path %in% selectedPaths, ]

    timeProfileMetaData <- list(
      "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit),
      "Concentration" = list(dimension = selectedDimension, unit = unit)
    )

    if (workflowType %in% "mean") {
      timeProfilePlot <- plotMeanTimeProfile(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        lloqData = selectedLloqData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = settings$plotConfigurations[["timeProfile"]]
      )
      timeProfilePlotLog <- plotMeanTimeProfileLog(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        lloqData = selectedLloqData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = settings$plotConfigurations[["timeProfileLog"]]
      )
    }
    if (workflowType %in% "population") {
      timeProfilePlot <- plotPopulationTimeProfile(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        lloqData = selectedLloqData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = settings$plotConfigurations[["timeProfile"]]
      )
      timeProfilePlotLog <- plotPopulationTimeProfileLog(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        lloqData = selectedLloqData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = settings$plotConfigurations[["timeProfileLog"]]
      )
    }

    timeProfilePlotLog <- tlf::setYAxis(plotObject = timeProfilePlot, scale = tlf::Scaling$log)

    goodnessOfFitPlots[[paste0("timeProfile-", selectedDimension)]] <- timeProfilePlot
    goodnessOfFitPlots[[paste0("timeProfileLog-", selectedDimension)]] <- timeProfilePlotLog

    goodnessOfFitCaptions[[paste0("timeProfile-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile", "linear")
    goodnessOfFitCaptions[[paste0("timeProfileLog-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile", "logarithmic")
  }
  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions
  ))
}

#' @title getResidualsPlotResults
#' @description Get plots and their captions for residuals
#' @param timeRange array of time values defining range of simulated data
#' @param residualsData data.frame of residuals data
#' @param metaDataFrame metaData represented as a data.frame
#' @param structureSet A `SimulationStructure` object
#' @param settings Optional settings for the plots. In particular, includes reference data for population time profile.
#' @return List of `plots`, their `captions` and `data` to export
#' @keywords internal
getResidualsPlotResults <- function(timeRange, residualsData, metaDataFrame, structureSet, settings = NULL) {
  residualsMetaData <- NULL
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  # Residuals can contain Inf/NA values which need to be seen in csv output file
  # While removed from the ggplot object
  csvResidualsData <- residualsData
  csvResidualsData[, "Residuals"] <- replaceInfWithNA(csvResidualsData[, "Residuals"])

  # rbind.data.frame enforces data.frame type and nrow can be used as is
  refResidualsData <- csvResidualsData
  # Add reference residuals data only if option is set to true
  # isTRUE is used for mean model workflows that have a NULL field
  if (isTRUE(structureSet$simulationSet$plotReferenceObsData)) {
    refResidualsData <- rbind.data.frame(settings$referenceData$residualsData, csvResidualsData)
  }
  refResidualsData <- removeMissingValues(refResidualsData, "Residuals")
  residualsData <- asTimeAfterDose(refResidualsData, min(timeRange), max(timeRange))

  logDebug(messages$dataIncludedInTimeRange(
    nrow(residualsData),
    timeRange, structureSet$simulationSet$timeUnit,
    "residuals"
  ))

  if (nrow(residualsData) == 0) {
    return(list(
      plots = goodnessOfFitPlots,
      captions = goodnessOfFitCaptions,
      data = csvResidualsData,
      metaData = list()
    ))
  }

  # Get residual scale for legends and captions
  residualsLegend <- "Residuals"
  residualScale <- ""
  residualScales <- sapply(structureSet$simulationSet$outputs, function(output) {
    output$residualScale
  })
  if (all(residualScales %in% ResidualScales$Logarithmic)) {
    residualsLegend <- "Residuals\nlog(Observed)-log(Simulated)"
    residualScale <- ResidualScales$Logarithmic
  }
  if (all(residualScales %in% ResidualScales$Linear)) {
    residualsLegend <- "Residuals\nObserved-Simulated"
    residualScale <- ResidualScales$Linear
  }


  for (unit in unique(metaDataFrame$unit)) {
    selectedDimension <- utils::head(metaDataFrame$dimension[metaDataFrame$unit %in% unit], 1)
    selectedPaths <- metaDataFrame$path[metaDataFrame$unit %in% unit]
    selectedResidualsData <- residualsData[residualsData$Path %in% selectedPaths, ]

    if (nrow(selectedResidualsData) == 0) {
      next
    }

    residualsMetaData <- list(
      "Observed" = list(dimension = "Observed data", unit = unit),
      "Simulated" = list(dimension = "Simulated value", unit = unit),
      "Residuals" = list(unit = "", dimension = residualsLegend)
    )

    obsVsPredPlot <- tlf::plotObsVsPred(
      data = selectedResidualsData,
      metaData = residualsMetaData,
      dataMapping = tlf::ObsVsPredDataMapping$new(
        x = "Observed",
        y = "Simulated",
        group = "Legend"
      ),
      plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
    )

    goodnessOfFitPlots[[paste0("obsVsPred-", selectedDimension)]] <- obsVsPredPlot
    goodnessOfFitCaptions[[paste0("obsVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "linear", settings)

    # TODO: update after tlf is robust enough when 0 is in log plots (tlf issue #369)
    selectedLogData <- selectedResidualsData$Simulated > 0 & selectedResidualsData$Observed > 0
    if (sum(selectedLogData) > 0) {
      obsVsPredRange <- autoAxesLimits(c(
        selectedResidualsData$Simulated[selectedLogData],
        selectedResidualsData$Observed[selectedLogData]
      ),
      scale = "log"
      )
      obsVsPredBreaks <- autoAxesTicksFromLimits(obsVsPredRange)

      obsVsPredPlotLog <- tlf::plotObsVsPred(
        data = selectedResidualsData[selectedLogData, ],
        metaData = residualsMetaData,
        dataMapping = tlf::ObsVsPredDataMapping$new(
          x = "Observed",
          y = "Simulated",
          group = "Legend"
        ),
        plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
      )
      obsVsPredPlotLog <- tlf::setXAxis(
        plotObject = obsVsPredPlotLog,
        scale = tlf::Scaling$log,
        limits = obsVsPredRange,
        ticks = obsVsPredBreaks
      )
      obsVsPredPlotLog <- tlf::setYAxis(
        plotObject = obsVsPredPlotLog,
        scale = tlf::Scaling$log,
        limits = obsVsPredRange,
        ticks = obsVsPredBreaks
      )

      goodnessOfFitPlots[[paste0("obsVsPredLog-", selectedDimension)]] <- obsVsPredPlotLog
      goodnessOfFitCaptions[[paste0("obsVsPredLog-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "logarithmic", settings)
    }

    goodnessOfFitPlots[[paste0("resVsPred-", selectedDimension)]] <- tlf::plotResVsPred(
      data = selectedResidualsData,
      metaData = residualsMetaData,
      dataMapping = tlf::ResVsPredDataMapping$new(
        x = "Simulated",
        y = "Residuals",
        group = "Legend"
      ),
      plotConfiguration = settings$plotConfigurations[["resVsPred"]]
    )
    goodnessOfFitCaptions[[paste0("resVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "resVsPred", residualScale, settings)
  }

  residualsMetaData <- list(
    "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit),
    "Residuals" = list(dimension = residualsLegend, unit = "")
  )

  residualTimeTicks <- getTimeTicksFromUnit(
    residualsMetaData$Time$unit,
    timeValues = residualsData$Time
  )
  goodnessOfFitPlots[["resVsTime"]] <- tlf::plotResVsTime(
    data = residualsData,
    metaData = residualsMetaData,
    dataMapping = tlf::ResVsTimeDataMapping$new(
      x = "Time",
      y = "Residuals",
      group = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["resVsTime"]]
  )
  goodnessOfFitPlots[["resVsTime"]] <- tlf::setXAxis(
    plotObject = goodnessOfFitPlots[["resVsTime"]],
    ticks = residualTimeTicks$ticks,
    ticklabels = residualTimeTicks$ticklabels
  )
  goodnessOfFitCaptions[["resVsTime"]] <- getGoodnessOfFitCaptions(structureSet, "resVsTime", residualScale)

  goodnessOfFitPlots[["resHisto"]] <- tlf::plotHistogram(
    data = residualsData,
    metaData = residualsMetaData,
    dataMapping = tlf::HistogramDataMapping$new(
      x = "Residuals",
      fill = "Legend",
      stack = TRUE,
      distribution = "normal"
    ),
    plotConfiguration = settings$plotConfigurations[["resHisto"]],
    bins = settings$bins %||% reEnv$defaultBins
  )
  goodnessOfFitPlots[["resHisto"]] <- tlf::setPlotLabels(
    goodnessOfFitPlots[["resHisto"]],
    ylabel = reEnv$residualsHistogramLabel
  )
  goodnessOfFitCaptions[["resHisto"]] <- getGoodnessOfFitCaptions(structureSet, "resHisto", residualScale)

  goodnessOfFitPlots[["resQQPlot"]] <- tlf::plotQQ(
    data = residualsData,
    metaData = residualsMetaData,
    dataMapping = tlf::QQDataMapping$new(
      y = "Residuals",
      group = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["resQQPlot"]]
  )
  goodnessOfFitPlots[["resQQPlot"]] <- tlf::setPlotLabels(
    goodnessOfFitPlots[["resQQPlot"]],
    ylabel = reEnv$residualsQQLabel
  )
  goodnessOfFitCaptions[["resQQPlot"]] <- getGoodnessOfFitCaptions(structureSet, "resQQPlot", residualScale)

  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    data = csvResidualsData,
    metaData = residualsMetaData
  ))
}
