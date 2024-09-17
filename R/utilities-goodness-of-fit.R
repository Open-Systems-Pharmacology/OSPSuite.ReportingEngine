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
    c("observedData", "simulatedData", "residualsData", "residualsMetaData"),
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
  outputObservedMetaData <- list()
  for (output in outputSelections) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getSimulatedResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet)
    outputObservedMetaData[[output$path]] <- outputObservedResults$metaData
    outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileMapping <- list(x = "Time", y = "Concentration", group = "Legend")

  resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, "time_profile_data")

  goodnessOfFitResults[[resultID]] <- saveTaskResults(
    id = resultID,
    table = simulatedData,
    includeTable = FALSE
  )
  # Save residuals data as a csv file
  resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, "residuals_data")
  goodnessOfFitResults[[resultID]] <- saveTaskResults(
    id = resultID,
    table = residualsData,
    includeTable = FALSE
  )

  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(c(outputSimulatedMetaData, outputObservedMetaData))

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
  }

  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  return(list(
    results = goodnessOfFitResults,
    residuals = allResiduals,
    metaData = metaDataFrame
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
  # Output object is updated: displayUnit cannot be empty anymore
  output$displayUnit <- output$displayUnit %||% simulationQuantity$displayUnit

  outputSimulatedMetaData <- list(
    Time = list(
      dimension = "Time",
      unit = simulationSet$timeUnit
    ),
    Concentration = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit
    ),
    Path = output$path,
    displayName = output$displayName,
    legend = captions$plotGoF$meanLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      pathName = output$displayName
    ),
    residualsLegend = captions$plotGoF$resLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      pathName = output$displayName
    ),
    residualScale = output$residualScale,
    group = output$groupID,
    color = output$color,
    fill = output$fill
  )

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
    "Legend" = outputSimulatedMetaData$legend,
    "ResidualsLegend" = outputSimulatedMetaData$residualsLegend,
    "Path" = output$path,
    "Group" = output$groupID
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
  if (isEmpty(observedData)) {
    return()
  }
  # Time matrix to match observed time with closest simulation time
  # This method assumes that there simulated data are dense enough to capture observed data
  obsTimeMatrix <- matrix(observedData[, "Time"], nrow(simulatedData), nrow(observedData), byrow = TRUE)
  simTimeMatrix <- matrix(simulatedData[, "Time"], nrow(simulatedData), nrow(observedData))

  timeMatchedData <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))

  # Issue #942, once implemented use ospsuite residuals calculation
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
    "lloq" = observedData[, "lloq"],
    "Simulated" = simulatedData[timeMatchedData, "Concentration"],
    "Residuals" = residualValues,
    "Legend" = simulatedData[timeMatchedData, "ResidualsLegend"],
    "Path" = observedData[, "Path"],
    "Group" = simulatedData[timeMatchedData, "Group"],
    "ResidualScale" = residualScale
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
    c("observedData", "simulatedData", "residualsData", "residualsMetaData"),
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
  outputObservedMetaData <- list()
  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getPopulationResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet, settings)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet)
    outputObservedMetaData[[output$path]] <- outputObservedResults$metaData

    outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
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
  # Save residuals data as a csv file
  resultID <- defaultFileNames$resultID(length(goodnessOfFitResults) + 1, "residuals_data")
  goodnessOfFitResults[[resultID]] <- saveTaskResults(
    id = resultID,
    table = residualsData,
    includeTable = FALSE
  )

  timeProfileMapping <- list(x = "Time", y = "Concentration", ymin = "ymin", ymax = "ymax", group = "Legend")
  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(c(outputSimulatedMetaData, outputObservedMetaData))

  # Save goodness of fit data for reference to be compared with other simulation sets
  referenceData <- list(
    simulatedData = simulatedData,
    observedData = observedData,
    residualsData = residualsData,
    timeOffset = structureSet$simulationSet$timeOffset,
    metaData = metaDataFrame
  )

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
  }

  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  return(list(
    results = goodnessOfFitResults,
    referenceData = referenceData,
    residuals = allResiduals,
    metaData = metaDataFrame
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

  outputSimulatedMetaData <- list(
    Time = list(
      dimension = "Time",
      unit = simulationSet$timeUnit
    ),
    Concentration = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit %||% simulationQuantity$displayUnit
    ),
    Path = output$path,
    displayName = output$displayName,
    legend = captions$plotGoF$populationLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      statistics = timeProfileStatistics,
      pathName = output$displayName
    ),
    residualsLegend = captions$plotGoF$resLegend(
      simulationSetName = simulationSet$simulationSetName,
      descriptor = structureSet$simulationSetDescriptor,
      pathName = output$displayName
    ),
    residualScale = output$residualScale,
    group = output$groupID,
    color = output$color,
    fill = output$fill
  )

  outputSimulatedData <- aggregateData
  outputSimulatedData$Concentration <- outputSimulatedData$y
  outputSimulatedData$Legend <- outputSimulatedMetaData$legend
  outputSimulatedData$ResidualsLegend <- outputSimulatedMetaData$residualsLegend
  outputSimulatedData$Path <- output$path
  outputSimulatedData$Group <- output$groupID

  return(list(
    data = outputSimulatedData,
    metaData = outputSimulatedMetaData
  ))
}

#' @title plotMeanTimeProfile
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param metaData meta data on `data`
#' @param dataMapping A list to be integrated to `tlf` DataMapping objects
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotMeanTimeProfile <- function(simulatedData,
                                observedData = NULL,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  simulatedDataMapping <- tlf::TimeProfileDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    group = dataMapping$group
  )
  observedDataMapping <- ObservedDataMapping$new(
    x = dataMapping$x,
    y = dataMapping$y,
    group = dataMapping$group,
    lloq = "lloq"
  )
  timeProfilePlot <- tlf::plotTimeProfile(
    data = simulatedData,
    metaData = metaData,
    dataMapping = simulatedDataMapping,
    observedData = observedData,
    observedDataMapping = observedDataMapping,
    plotConfiguration = plotConfiguration
  )
  # Check if lloq needs to be added
  lloqRows <- !is.na(observedData$lloq)
  if (!any(lloqRows)) {
    return(timeProfilePlot)
  }
  lloqCaptions <- unique(observedData[lloqRows, dataMapping$group])
  timeProfilePlot <- addLLOQLegend(timeProfilePlot, lloqCaptions)
  return(timeProfilePlot)
}

#' @title plotMeanTimeProfileLog
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param metaData meta data on `data`
#' @param dataMapping A list to be integrated to `tlf` DataMapping objects
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotMeanTimeProfileLog <- function(simulatedData,
                                   observedData = NULL,
                                   metaData = NULL,
                                   dataMapping = NULL,
                                   plotConfiguration = NULL) {
  # Remove 0 values from simulated and observed data because of log scale
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$y)
  observedData <- removeNegativeValues(observedData, dataMapping$y)

  logObservedValues <- NULL
  if (!isEmpty(observedData)) {
    logObservedValues <- c(observedData[, dataMapping$y], observedData$lloq)
  }
  # Get the nice auto scaling of the log data unless user defined
  yAxisLimits <- autoAxesLimits(
    c(simulatedData[, dataMapping$y], logObservedValues),
    scale = "log"
  )
  yAxisTicks <- autoAxesTicksFromLimits(yAxisLimits)
  plotConfiguration$yAxis$scale <- tlf::Scaling$log
  plotConfiguration$yAxis$axisLimits <- plotConfiguration$yAxis$axisLimits %||% yAxisLimits
  plotConfiguration$yAxis$ticks <- yAxisTicks

  meanTimeProfileLog <- plotMeanTimeProfile(
    simulatedData,
    observedData = observedData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  return(meanTimeProfileLog)
}

#' @title plotPopTimeProfile
#' @description Plot time profile for population model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param metaData meta data on `data`
#' @param dataMapping A list to be integrated to `tlf` DataMapping objects
#' @param plotConfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotPopTimeProfile <- function(simulatedData,
                               observedData = NULL,
                               dataMapping = NULL,
                               metaData = NULL,
                               plotConfiguration = NULL) {
  simulatedData <- removeMissingValues(simulatedData, dataMapping$y)
  simulatedData <- removeMissingValues(simulatedData, dataMapping$ymin)
  simulatedData <- removeMissingValues(simulatedData, dataMapping$ymax)
  observedData <- removeMissingValues(observedData, dataMapping$y)

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
    color = dataMapping$group,
    lloq = "lloq"
  )

  timeProfilePlot <- tlf::plotTimeProfile(
    data = simulatedData,
    metaData = metaData,
    dataMapping = simulatedDataMapping,
    observedData = observedData,
    observedDataMapping = observedDataMapping,
    plotConfiguration = plotConfiguration
  )
  # Check if lloq needs to be added
  lloqRows <- !is.na(observedData$lloq)
  if (!any(lloqRows)) {
    return(timeProfilePlot)
  }
  lloqCaptions <- unique(observedData[lloqRows, dataMapping$group])
  timeProfilePlot <- addLLOQLegend(timeProfilePlot, lloqCaptions)
  return(timeProfilePlot)
}

#' @title plotPopTimeProfileLog
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param metaData meta data on `data`
#' @param dataMapping A list mapping `x`, `y` and `group` from datasets
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotPopTimeProfileLog <- function(simulatedData,
                                  observedData = NULL,
                                  metaData = NULL,
                                  dataMapping = NULL,
                                  plotConfiguration = NULL) {
  # Remove 0 values from simulated and observed data because of log plots
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$y)
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$ymin)
  simulatedData <- removeNegativeValues(simulatedData, dataMapping$ymax)
  observedData <- removeNegativeValues(observedData, dataMapping$y)

  logObservedValues <- NULL
  if (!isEmpty(observedData)) {
    logObservedValues <- c(observedData[, dataMapping$y], observedData$lloq)
  }
  # Get the nice auto scaling of the log data unless user defined
  yAxisLimits <- autoAxesLimits(
    c(
      simulatedData[, dataMapping$y],
      simulatedData[, dataMapping$ymin],
      simulatedData[, dataMapping$ymax],
      logObservedValues
    ),
    scale = "log"
  )
  yAxisTicks <- autoAxesTicksFromLimits(yAxisLimits)
  plotConfiguration$yAxis$scale <- tlf::Scaling$log
  plotConfiguration$yAxis$axisLimits <- plotConfiguration$yAxis$axisLimits %||% yAxisLimits
  plotConfiguration$yAxis$ticks <- yAxisTicks

  populationTimeProfileLog <- plotPopTimeProfile(
    simulatedData,
    observedData = observedData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  return(populationTimeProfileLog)
}

#' @title asTimeAfterDose
#' @description Transform time data as time after dose data
#' @param data A data.frame that includes the variable `Time`
#' @param timeOffset Value shifting time
#' @param maxTime Optional maximum time value before shift
#' @return A data.frame
#' @keywords internal
asTimeAfterDose <- function(data, timeOffset, maxTime = NULL) {
  if (isEmpty(data)) {
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
#' @description Get time profile list of metaData as a single data.frame
#' @param listOfMetaData list including `Path` and `Concentration`
#' @return A data.frame with variables `path`, `dimension`, `unit`, `group`, `color`, `fill`
#' @keywords internal
getMetaDataFrame <- function(listOfMetaData) {
  # Create a list of data.frames with properties of interest
  metaDataFrame <- lapply(
    listOfMetaData,
    function(metaData) {
      data.frame(
        path = metaData$Path,
        dimension = metaData$Concentration$dimension,
        unit = metaData$Concentration$unit,
        legend = metaData$legend,
        residualsLegend = metaData$residualsLegend,
        residualScale = metaData$residualScale,
        group = metaData$group %||% NA,
        color = metaData$color,
        fill = metaData$fill
      )
    }
  )
  # Merge and format data.frame
  metaDataFrame <- do.call(rbind.data.frame, metaDataFrame)
  metaDataFrame <- data.frame(metaDataFrame, stringsAsFactors = FALSE)
  # Update dimension name for better caption in plot label
  selectedDimensions <- metaDataFrame$dimension %in% c("Concentration (mass)", "Concentration (molar)")
  metaDataFrame$dimension[selectedDimensions] <- "Concentration"
  return(metaDataFrame)
}

#' @title getTimeProfilePlotResults
#' @description Get plots and their captions for mean or population time profiles
#' @param workflowType `"mean"` or `"population"` workflow
#' @param timeRange array of time values defining range of simulated data.
#' Note that `timeRange` accounts for user defined `timeOffset`
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param metaDataFrame metaData represented as a data.frame
#' @param timeProfileMapping list of variables to map in the time profile plots
#' @param structureSet A `SimulationStructure` object
#' @param settings Optional settings for the plots. In particular, includes reference data for population time profile.
#' @return List of `plots` and their `captions`
#' @keywords internal
getTimeProfilePlotResults <- function(workflowType, timeRange, simulatedData, observedData = NULL, metaDataFrame, timeProfileMapping, structureSet, settings = NULL) {
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  # Add reference simulated data, if any, to the existing data
  # rbind.data.frame enforces data.frame type and nrow can be used as is
  # Reset time based on timeOffset or application range
  simulatedData <- rbind.data.frame(
    asTimeAfterDose(settings$referenceData$simulatedData, min(timeRange), max(timeRange)),
    asTimeAfterDose(simulatedData, min(timeRange), max(timeRange))
  )
  if (nrow(simulatedData) == 0) {
    stop(messages$dataIncludedInTimeRange(
      nrow(simulatedData),
      timeRange,
      structureSet$simulationSet$timeUnit, "simulated"
    ))
  }
  observedData <- asTimeAfterDose(observedData, min(timeRange), max(timeRange))
  # Add reference observed data only if option is set to true
  # isTRUE is used for mean model workflows that have a NULL field
  if (isTRUE(structureSet$simulationSet$plotReferenceObsData)) {
    observedData <- rbind.data.frame(
      asTimeAfterDose(settings$referenceData$observedData, min(timeRange), max(timeRange)),
      observedData
    )
  }

  logDebug(messages$dataIncludedInTimeRange(
    nrow(simulatedData),
    timeRange,
    structureSet$simulationSet$timeUnit,
    "simulated"
  ))
  logDebug(messages$dataIncludedInTimeRange(
    nrow(observedData),
    timeRange,
    structureSet$simulationSet$timeUnit,
    "observed"
  ))

  # update metaDataFrame if reference data included
  if (!isEmpty(settings$referenceData$metaData)) {
    referenceMetaData <- settings$referenceData$metaData
    referenceMetaData$color <- reEnv$referenceColor
    referenceMetaData$fill <- reEnv$referenceFill
    metaDataFrame <- rbind.data.frame(
      metaDataFrame,
      referenceMetaData
    )
  }

  outputGroups <- getOutputGroups(metaDataFrame)
  for (outputGroup in outputGroups) {
    resultId <- paste0("timeProfile-", length(goodnessOfFitPlots)+1)
    resultIdLog <- paste0("timeProfileLog-", length(goodnessOfFitPlots)+2)

    selectedSimulatedData <- simulatedData[simulatedData$Group %in% outputGroup$group, ]
    selectedObservedData <- observedData[observedData$Group %in% outputGroup$group, ]

    timeProfileMetaData <- list(
      "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit),
      # outputGroup can have multiple rows, thus only use first one
      # Note that dimension and unit were previously checked and warned for consistency
      "Concentration" = list(
        dimension = utils::head(outputGroup$dimension, 1),
        unit = utils::head(outputGroup$unit, 1)
      )
    )

    timeProfilePlotConfiguration <- getTimeProfilePlotConfiguration(
      workflowType = workflowType,
      group = outputGroup,
      data = selectedSimulatedData,
      metaData = timeProfileMetaData,
      observedData = selectedObservedData,
      dataMapping = timeProfileMapping,
      plotConfiguration = settings$plotConfigurations[["timeProfile"]]
    )

    timeProfilePlotConfigurationLog <- getTimeProfilePlotConfiguration(
      workflowType = workflowType,
      group = outputGroup,
      data = selectedSimulatedData,
      metaData = timeProfileMetaData,
      observedData = selectedObservedData,
      dataMapping = timeProfileMapping,
      plotConfiguration = settings$plotConfigurations[["timeProfileLog"]]
    )

    if (workflowType %in% "mean") {
      timeProfilePlot <- plotMeanTimeProfile(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = timeProfilePlotConfiguration
      )
      timeProfilePlotLog <- plotMeanTimeProfileLog(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = timeProfilePlotConfigurationLog
      )
    }

    if (workflowType %in% "population") {
      timeProfilePlot <- plotPopTimeProfile(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = timeProfilePlotConfiguration
      )
      timeProfilePlotLog <- plotPopTimeProfileLog(
        simulatedData = selectedSimulatedData,
        observedData = selectedObservedData,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileMapping,
        plotConfiguration = timeProfilePlotConfigurationLog
      )
    }

    goodnessOfFitPlots[[resultId]] <- timeProfilePlot
    goodnessOfFitPlots[[resultIdLog]] <- timeProfilePlotLog

    goodnessOfFitCaptions[[resultId]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile", "linear")
    goodnessOfFitCaptions[[resultIdLog]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile", "logarithmic")
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
      data = csvResidualsData
    ))
  }

  # Observed vs Predicted Plots
  outputGroups <- getOutputGroups(metaDataFrame)
  outputId <- 1
  for (outputGroup in outputGroups) {
    residualsPlotResults <- getResidualsPlotResultsInGroup(
      data = residualsData,
      metaData = outputGroup,
      outputId = outputId,
      structureSet = structureSet,
      settings = settings
    )
    goodnessOfFitPlots <- c(goodnessOfFitPlots, residualsPlotResults$plots)
    goodnessOfFitCaptions <- c(goodnessOfFitCaptions, residualsPlotResults$captions)
    outputId <- outputId + 1
  }
  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    data = csvResidualsData
  ))
}

#' @title getResidualsPlotResultsInGroup
#' @description Get plots and their captions for residuals
#' @param data A data.frame of residuals data
#' @param metaData metaData represented as a data.frame
#' @param outputId Output identifier to provide unique id name
#' @param structureSet A `SimulationStructure` object or `NULL` if performing residuals across simulations
#' @param settings Optional settings for the plots. In particular, includes reference data for population time profile.
#' @return List of `plots`, their `captions` and `data` to export
#' @keywords internal
getResidualsPlotResultsInGroup <- function(data, metaData, outputId, structureSet = NULL, settings = NULL) {
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()
  resultId <- as.list(sapply(
    c("obsVsPred", "obsVsPredLog", "resVsPred", "resVsTime", "resHisto", "resQQPlot"),
    function(resultName) {
      paste0(resultName, "-", outputId)
    }
  ))

  residualScale <- utils::head(metaData$residualScale, 1)
  residualsLegend <- captions$plotGoF$residualsLabel(residualScale)

  selectedData <- data[data$Group %in% metaData$group, ]
  if (nrow(selectedData) == 0) {
    return(list(plots = NULL, captions = NULL))
  }
  # Meta Data defining the plot labels
  residualsMetaData <- list(
    "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit %||% settings$timeUnit),
    "Observed" = list(dimension = "Observed data", unit = utils::head(metaData$unit, 1)),
    "Simulated" = list(dimension = "Simulated value", unit = utils::head(metaData$unit, 1)),
    "Residuals" = list(dimension = residualsLegend, unit = ""),
    "Group" = utils::head(metaData$group, 1)
  )
  # Data Mapping
  obsVsPredDataMapping <- tlf::ObsVsPredDataMapping$new(
    x = "Observed",
    y = "Simulated",
    group = "Legend",
    lloq = "lloq"
  )
  resVsPredDataMapping <- tlf::ResVsPredDataMapping$new(
    x = "Simulated",
    y = "Residuals",
    group = "Legend"
  )
  resVsTimeDataMapping <- tlf::ResVsTimeDataMapping$new(
    x = "Time",
    y = "Residuals",
    group = "Legend"
  )
  histogramDataMapping <- tlf::HistogramDataMapping$new(
    x = "Residuals",
    fill = "Legend",
    stack = TRUE,
    distribution = "normal",
    frequency = TRUE
  )
  qqDataMapping <- tlf::QQDataMapping$new(y = "Residuals", group = "Legend")
  # Plot Configurations
  obsVsPredPlotConfiguration <- getGOFPlotConfiguration(
    plotType = "obsVsPred",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = obsVsPredDataMapping,
    plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
  )
  obsVsPredPlotConfigurationLog <- getGOFPlotConfiguration(
    plotType = "obsVsPredLog",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = obsVsPredDataMapping,
    plotConfiguration = settings$plotConfigurations[["obsVsPredLog"]]
  )
  resVsPredPlotConfiguration <- getGOFPlotConfiguration(
    plotType = "resVsPred",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = resVsPredDataMapping,
    plotConfiguration = settings$plotConfigurations[["resVsPred"]]
  )
  resVsTimePlotConfiguration <- getGOFPlotConfiguration(
    plotType = "resVsTime",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = resVsTimeDataMapping,
    plotConfiguration = settings$plotConfigurations[["resVsTime"]]
  )
  histogramPlotConfiguration <- getGOFPlotConfiguration(
    plotType = "resHisto",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = histogramDataMapping,
    plotConfiguration = settings$plotConfigurations[["resHisto"]]
  )
  qqPlotConfiguration <- getGOFPlotConfiguration(
    plotType = "resQQPlot",
    group = metaData,
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = qqDataMapping,
    plotConfiguration = settings$plotConfigurations[["resQQPlot"]]
  )
  # Actual Plots and Captions
  goodnessOfFitPlots[[resultId$obsVsPred]] <- tlf::plotObsVsPred(
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = obsVsPredDataMapping,
    plotConfiguration = obsVsPredPlotConfiguration,
    # Add identity line to linear plot
    foldDistance = 0
  )
  goodnessOfFitCaptions[[resultId$obsVsPred]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "linear", settings)

  selectedLogData <- selectedData$Simulated > 0 & selectedData$Observed > 0
  if (sum(selectedLogData) > 0) {
    goodnessOfFitPlots[[resultId$obsVsPredLog]] <- tlf::plotObsVsPred(
      data = selectedData[selectedLogData, ],
      metaData = residualsMetaData,
      dataMapping = obsVsPredDataMapping,
      plotConfiguration = obsVsPredPlotConfigurationLog,
      # Add identity line to log-log plot
      foldDistance = 1
    )
    goodnessOfFitCaptions[[resultId$obsVsPredLog]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "logarithmic", settings)
  }

  goodnessOfFitPlots[[resultId$resVsPred]] <- tlf::plotResVsPred(
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = resVsPredDataMapping,
    plotConfiguration = resVsPredPlotConfiguration
  )
  goodnessOfFitCaptions[[resultId$resVsPred]] <- getGoodnessOfFitCaptions(structureSet, "resVsPred", residualScale, settings)

  goodnessOfFitPlots[[resultId$resVsTime]] <- tlf::plotResVsTime(
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = resVsTimeDataMapping,
    plotConfiguration = resVsTimePlotConfiguration
  )
  goodnessOfFitCaptions[[resultId$resVsTime]] <- getGoodnessOfFitCaptions(structureSet, "resVsTime", residualScale, settings)

  goodnessOfFitPlots[[resultId$resHisto]] <- tlf::plotHistogram(
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = histogramDataMapping,
    plotConfiguration = histogramPlotConfiguration,
    bins = settings$bins %||% reEnv$defaultBins
  )
  goodnessOfFitCaptions[[resultId$resHisto]] <- getGoodnessOfFitCaptions(structureSet, "resHisto", residualScale, settings)

  goodnessOfFitPlots[[resultId$resQQPlot]] <- tlf::plotQQ(
    data = selectedData,
    metaData = residualsMetaData,
    dataMapping = qqDataMapping,
    plotConfiguration = qqPlotConfiguration
  )
  goodnessOfFitCaptions[[resultId$resQQPlot]] <- getGoodnessOfFitCaptions(structureSet, "resQQPlot", residualScale, settings)

  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions
  ))
}

#' @title getOutputGroups
#' @description Group paths with same group IDs and check their units and residuals are consistent
#' @param metaDataFrame A data.frame with variables `path`, `dimension`, `unit`, `group`, `color`, `fill`
#' @return List of data.frames data.frame with variables `path`, `dimension`, `unit`, `group`, `color`, `fill`
#' @keywords internal
getOutputGroups <- function(metaDataFrame) {
  outputGroups <- split(
    x = metaDataFrame,
    f = as.factor(metaDataFrame$group)
  )
  # Validate same unit and residualScale is used within a group ID
  # Use invisible to prevent displaying a list of NULL values
  invisible(lapply(outputGroups, function(metaData) {
    checkMetaDataIsConsistent(metaData)
  }))
  return(outputGroups)
}

#' @title ApplicationRanges
#' @description
#' Keys of reported ranges when simulation includes multiple applications
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists available Application Ranges
#' ApplicationRanges
#'
ApplicationRanges <- enum(c("total", "firstApplication", "lastApplication"))
