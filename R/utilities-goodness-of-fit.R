#' @title plotMeanGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list with `plots`, `tables` and `residuals` objects to be saved
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ggplot2
plotMeanGoodnessOfFit <- function(structureSet,
                                  logFolder = getwd(),
                                  settings = NULL) {
  validateIsOfType(structureSet, "SimulationStructure")

  initializeExpression <- parse(text = paste0(
    c("observedData", "simulatedData", "lloqData", "residualsData", "residualsMetaData", "residuals"),
    " <- NULL"
  ))
  eval(initializeExpression)
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()
  goodnessOfFitResiduals <- list()

  # Load observed and simulated data
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResult <- ospsuite::importResultsFromCSV(simulation, structureSet$simulationResultFileNames)

  observedResult <- loadObservedDataFromSimulationSet(structureSet$simulationSet, logFolder)


  outputSelections <- structureSet$simulationSet$outputs
  if (!is.null(settings$outputSelections)) {
    availableOutputs <- sapply(structureSet$simulationSet$outputs, function(output){output$path})
    selectedOutputs <- availableOutputs %in% settings$outputSelections
    outputSelections <- structureSet$simulationSet$outputs[selectedOutputs]
  }

  outputSimulatedMetaData <- list()
  for (output in outputSelections) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getSimulatedResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet$simulationSet)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet$simulationSet$timeUnit, logFolder)
    outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
    lloqData <- rbind.data.frame(lloqData, outputObservedResults$lloq)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileData <- rbind.data.frame(observedData, lloqData, simulatedData)
  timeProfileMapping <- tlf::XYGDataMapping$new(x = "Time", y = "Concentration", color = "Legend")

  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  # Get list of total, first application, and last application ranges
  # Note: currently simulationSet$applicationRanges includes only logicals
  # timeOffset could also be included into the process
  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet, logFolder)

  # If one or no application, field 'keep' for time range other than total is FALSE
  for (timeRange in timeRanges) {
    if (!timeRange$keep) {
      next
    }
    timeProfilePlotResults <- getTimeProfilePlotResults("mean", timeRange$values, simulatedData, observedData, lloqData, metaDataFrame, timeProfileMapping, structureSet, settings, logFolder)
    goodnessOfFitPlots[[timeRange$name]] <- timeProfilePlotResults$plots
    goodnessOfFitCaptions[[timeRange$name]] <- timeProfilePlotResults$captions
  }

  if (!isOfLength(residualsData, 0)) {
    for (timeRange in timeRanges) {
      if (!timeRange$keep) {
        next
      }
      residualsPlotResults <- getResidualsPlotResults(timeRange$values, residualsData, metaDataFrame, structureSet, settings, logFolder)
      goodnessOfFitPlots[[timeRange$name]] <- c(goodnessOfFitPlots[[timeRange$name]], residualsPlotResults$plots)
      goodnessOfFitCaptions[[timeRange$name]] <- c(goodnessOfFitCaptions[[timeRange$name]], residualsPlotResults$captions)
      goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
      residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
    }
  }
  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  if (!isOfLength(allResiduals, 0)) {
    residuals <- list(
      data = allResiduals,
      metaData = residualsMetaData[[ApplicationRanges$total]]
    )
  }

  return(list(
    plots = goodnessOfFitPlots,
    tables = list(timeProfileData = timeProfileData),
    captions = goodnessOfFitCaptions,
    residuals = residuals
  ))
}

#' @title getSimulatedResultsFromOutput
#' @description
#' Get simulated data from an Output object
#' @param simulationPathResults list with simulated data included
#' @param output An `Output` object
#' @param simulationQuantity Dimension/quantity for unit conversion of dependent variable
#' @param molWeight Molar weight for unit conversion of dependent variable
#' @param simulationSet `SimulationSet` object
#' @return list of data and metaData
getSimulatedResultsFromOutput <- function(simulationPathResults, output, simulationQuantity, molWeight, simulationSet) {
  outputConcentration <- simulationPathResults$data[, output$path]
  if (!isOfLength(output$displayUnit, 0)) {
    outputConcentration <- ospsuite::toUnit(simulationQuantity,
      simulationPathResults$data[, output$path],
      output$displayUnit,
      molWeight = molWeight
    )
  }

  outputSimulatedData <- data.frame(
    "Time" = ospsuite::toUnit("Time", simulationPathResults$data[, "Time"], simulationSet$timeUnit),
    "Concentration" = outputConcentration,
    "Legend" = paste0("Simulated ", output$displayName %||% output$path, " (", simulationSet$simulationSetName, ")"),
    "Path" = output$path
  )

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
    "Legend" = simulatedData[timeMatchedData, "Legend"],
    "Path" = observedData[, "Path"]
  )
  return(residualsData)
}

#' @title plotPopulationGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list with `plots`, `tables` and `residuals` objects to be saved
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ggplot2
plotPopulationGoodnessOfFit <- function(structureSet,
                                        logFolder = getwd(),
                                        settings = NULL) {
  validateIsOfType(structureSet, "SimulationStructure")

  initializeExpression <- parse(text = paste0(
    c("observedData", "simulatedData", "lloqData", "residualsData", "residualsMetaData", "residuals"),
    " <- NULL"
  ))
  eval(initializeExpression)
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()
  goodnessOfFitResiduals <- list()

  residualsAggregationType <- settings$residualsAggregationType %||% "mean"
  selectedVariablesForResiduals <- c("Time", "mean", "legendMean", "Path")
  if (residualsAggregationType == "median") {
    selectedVariablesForResiduals <- c("Time", "median", "legendMedian", "Path")
  }

  # Load observed and simulated data
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResult <- ospsuite::importResultsFromCSV(simulation, structureSet$simulationResultFileNames)

  observedResult <- loadObservedDataFromSimulationSet(structureSet$simulationSet, logFolder)

  outputSimulatedMetaData <- list()
  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getPopulationResultsFromOutput(simulationPathResults, output, simulationQuantity, molWeight, structureSet$simulationSet, settings)
    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    outputObservedResults <- getObservedDataFromOutput(output, observedResult$data, observedResult$dataMapping, molWeight, structureSet$simulationSet$timeUnit, logFolder)

    simulatedDataForResiduals <- outputSimulatedData[, selectedVariablesForResiduals]
    # getResiduals is based on mean workflow whose names are c("Time", "Concentration", "Legend", "Path")
    names(simulatedDataForResiduals) <- c("Time", "Concentration", "Legend", "Path")
    outputResidualsData <- getResiduals(outputObservedResults$data, simulatedDataForResiduals, output$residualScale)

    # Build data.frames to be plotted
    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedResults$data)
    lloqData <- rbind.data.frame(lloqData, outputObservedResults$lloq)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileMapping <- tlf::XYGDataMapping$new(x = "Time", y = "Concentration", color = "Legend")
  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet, logFolder)

  for (timeRange in timeRanges) {
    if (!timeRange$keep) {
      next
    }
    timeProfilePlotResults <- getTimeProfilePlotResults("population", timeRange$values, simulatedData, observedData, lloqData, metaDataFrame, timeProfileMapping, structureSet, settings, logFolder)
    goodnessOfFitPlots[[timeRange$name]] <- timeProfilePlotResults$plots
    goodnessOfFitCaptions[[timeRange$name]] <- timeProfilePlotResults$captions
  }

  if (!isOfLength(residualsData, 0)) {
    for (timeRange in timeRanges) {
      if (!timeRange$keep) {
        next
      }
      residualsPlotResults <- getResidualsPlotResults(timeRange$values, residualsData, metaDataFrame, structureSet, settings, logFolder)
      goodnessOfFitPlots[[timeRange$name]] <- c(goodnessOfFitPlots[[timeRange$name]], residualsPlotResults$plots)
      goodnessOfFitCaptions[[timeRange$name]] <- c(goodnessOfFitCaptions[[timeRange$name]], residualsPlotResults$captions)
      goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
      residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
    }
  }
  allResiduals <- goodnessOfFitResiduals[[ApplicationRanges$total]]
  if (!isOfLength(allResiduals, 0)) {
    residuals <- list(
      data = allResiduals,
      metaData = residualsMetaData[[ApplicationRanges$total]]
    )
  }
  goodnessOfFitTables <- list(simulatedData = simulatedData)
  if (!isOfLength(observedData, 0)) {
    goodnessOfFitTables <- list(
      observedData = observedData,
      simulatedData = simulatedData
    )
  }
  referenceData <- list(
    simulatedData = simulatedData,
    observedData = observedData,
    lloqData = lloqData,
    residualsData = residualsData
  )
  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    tables = goodnessOfFitTables,
    residuals = residuals,
    referenceData = referenceData
  ))
}

#' @title getPopulationResultsFromOutput
#' @description
#' Get simulated population data from an Output object
#' @param simulationPathResults list with simulated data included
#' @param output An `Output` object
#' @param simulationQuantity Dimension/quantity for unit conversion of dependent variable
#' @param molWeight Molar weight for unit conversion of dependent variable
#' @param simulationSet `SimulationSet` object
#' @param settings TaskSetting object
#' @return list of data and metaData
getPopulationResultsFromOutput <- function(simulationPathResults, output, simulationQuantity, molWeight, simulationSet, settings = NULL) {
  aggregateNames <- c("mean", "median", "lowPerc", "highPerc")
  aggregateFunctions <- c(mean, median, AggregationConfiguration$functions$ymin, AggregationConfiguration$functions$ymax)

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
  # Expressions are used to prevent copy/paste of the code for mean, median and range conversions
  aggregateData <- aggregateSummary$dfHelper
  aggregateData$Time <- toUnit("Time", aggregateData$Time, simulationSet$timeUnit)

  convertExpressions <- parse(text = paste0(
    "aggregateData$", aggregateNames, "<- ifnotnull(output$displayUnit,",
    "toUnit(simulationQuantity, aggregateData$", aggregateNames, ", output$displayUnit, molWeight = molWeight),",
    "aggregateData$", aggregateNames, ")"
  ))
  eval(convertExpressions)

  # Legend using expressions
  # The generated code to eval follows the example below
  # aggregateData$legendMean <- paste0("Simulated mean for ",
  # output$displayName %||% output$path, " (", simulationSet$simulationSetName, ")")
  legendExpressions <- parse(text = paste0(
    "aggregateData$", c("legendMean", "legendMedian", "legendRange"),
    '<- paste0("Simulated ', c("mean", "median", AggregationConfiguration$names$range),
    ' for ", output$displayName %||% output$path, " (", simulationSet$simulationSetName, ")")'
  ))
  eval(legendExpressions)

  outputSimulatedData <- aggregateData
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
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanTimeProfile <- function(simulatedData,
                                observedData = NULL,
                                lloqData = NULL,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  timeProfilePlot <- tlf::addLine(
    data = simulatedData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  if (!isOfLength(observedData, 0)) {
    timeProfilePlot <- tlf::addScatter(
      data = observedData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotObject = timeProfilePlot
    )
  }
  if (!isOfLength(lloqData, 0)) {
    timeProfilePlot <- tlf::addLine(
      data = lloqData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotObject = timeProfilePlot
    )
  }
  timeProfilePlot <- tlf::setLegendPosition(plotObject = timeProfilePlot, position = reDefaultLegendPosition)
  return(timeProfilePlot)
}

#' @title plotMeanObsVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanObsVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  identityMinMax <- c(
    0.8 * min(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE),
    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE)
  )
  identityLine <- data.frame(
    "Observed" = identityMinMax,
    "Simulated" = identityMinMax,
    "Legend" = "Line of identity"
  )
  obsVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Observed",
    y = "Simulated",
    color = "Legend"
  )

  meanObsVsPredPlot <- tlf::addLine(
    data = identityLine,
    metaData = metaData,
    dataMapping = obsVsPredDataMapping,
    plotConfiguration = plotConfiguration
  )

  meanObsVsPredPlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = obsVsPredDataMapping,
    plotObject = meanObsVsPredPlot
  )
  meanObsVsPredPlot <- tlf::setLegendPosition(plotObject = meanObsVsPredPlot, position = reDefaultLegendPosition)

  return(meanObsVsPredPlot)
}

#' @title plotMeanResVsTime
#' @description Plot Residual vs time for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanResVsTime <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals",
    color = "Legend"
  )

  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]), na.rm = TRUE)

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping
  )
  meanResVsTimePlot <- tlf::initializePlot(plotConfiguration)

  meanResVsTimePlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping,
    plotObject = meanResVsTimePlot
  )

  meanResVsTimePlot <- meanResVsTimePlot + ggplot2::geom_hline(
    yintercept = 0,
    size = 1
  )

  meanResVsTimePlot <- tlf::setLegendPosition(plotObject = meanResVsTimePlot, position = reDefaultLegendPosition)
  meanResVsTimePlot <- tlf::setYAxis(
    plotObject = meanResVsTimePlot,
    limits = c(-maxRes, maxRes)
  )

  return(meanResVsTimePlot)
}

#' @title plotMeanResVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanResVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  resVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Simulated",
    y = "Residuals",
    color = "Legend"
  )

  maxRes <- 1.2 * max(abs(data[, resVsPredDataMapping$y]), na.rm = TRUE)

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping
  )
  meanResVsPredPlot <- tlf::initializePlot(plotConfiguration)

  meanResVsPredPlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping,
    plotObject = meanResVsPredPlot
  )

  meanResVsPredPlot <- meanResVsPredPlot + ggplot2::geom_hline(
    yintercept = 0,
    size = 1
  )

  meanResVsPredPlot <- tlf::setLegendPosition(plotObject = meanResVsPredPlot, position = reDefaultLegendPosition)
  meanResVsTimePlot <- tlf::setYAxis(
    plotObject = meanResVsPredPlot,
    limits = c(-maxRes, maxRes)
  )

  return(meanResVsPredPlot)
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
#' @import ggplot2
plotPopulationTimeProfile <- function(simulatedData,
                                      observedData = NULL,
                                      lloqData = NULL,
                                      dataMapping = NULL,
                                      metaData = NULL,
                                      plotConfiguration = NULL) {

  # metaData needs to be transfered to ymin and ymax
  # so that y label shows dimension [unit] by default
  metaData$x <- metaData$Time
  metaData$ymin <- metaData$Concentration
  metaData$ymax <- metaData$Concentration

  timeProfilePlot <- tlf::addRibbon(
    x = simulatedData$Time,
    ymin = simulatedData$lowPerc,
    ymax = simulatedData$highPerc,
    metaData = metaData,
    caption = simulatedData$legendRange,
    alpha = 0.6,
    plotConfiguration = plotConfiguration
  )
  timeProfilePlot <- tlf::addLine(
    x = simulatedData$Time,
    y = simulatedData$median,
    caption = simulatedData$legendMedian,
    plotObject = timeProfilePlot
  )
  timeProfilePlot <- tlf::addLine(
    x = simulatedData$Time,
    y = simulatedData$mean,
    caption = simulatedData$legendMean,
    plotObject = timeProfilePlot
  )
  if (!isOfLength(observedData, 0)) {
    timeProfilePlot <- tlf::addScatter(
      data = observedData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotObject = timeProfilePlot
    )
  }
  if (!isOfLength(lloqData, 0)) {
    timeProfilePlot <- tlf::addLine(
      data = lloqData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotObject = timeProfilePlot
    )
  }
  timeProfilePlot <- tlf::setLegendPosition(plotObject = timeProfilePlot, position = reDefaultLegendPosition)

  return(timeProfilePlot)
}

#' @title plotResidualsHistogram
#' @description Plot histogram of residuals
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `HistogramDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @param bins number of bins defined in the histogram
#' @return ggplot object of log residuals histogram
#' @export
#' @import tlf
#' @import ggplot2
#' @import stats
plotResidualsHistogram <- function(data,
                                   metaData = NULL,
                                   dataMapping = NULL,
                                   plotConfiguration = NULL,
                                   bins = NULL) {
  dataMapping <- dataMapping %||% tlf::HistogramDataMapping$new(x = "Residuals", fill = "Legend")

  plotConfiguration <- plotConfiguration %||% tlf::HistogramPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping,
    ylabel = "Number of residuals"
  )

  bins <- bins %||% 15

  # To fit normal distribution density curve to histogram, the density curve needs to be scaled
  # graphics::hist provides density and counts for histograms from which the scaling factor can be directly obtained
  histResult <- graphics::hist(data[, dataMapping$x], breaks = bins, plot = FALSE)
  scalingFactor <- mean(histResult$counts[histResult$counts > 0] / histResult$density[histResult$counts > 0])

  xmax <- 1.1 * max(abs(data[, dataMapping$x]), na.rm = TRUE)
  xDensityData <- seq(-xmax, xmax, 2 * xmax / 100)
  yDensityData <- scalingFactor * stats::dnorm(xDensityData, sd = stats::sd(data[, dataMapping$x], na.rm = TRUE))
  densityData <- data.frame(x = xDensityData, y = yDensityData)

  resHistoPlot <- tlf::initializePlot(plotConfiguration)

  # TO DO: transfer the histogram wrapper into TLF
  resHistoPlot <- resHistoPlot +
    ggplot2::geom_histogram(
      data = data,
      mapping = ggplot2::aes_string(
        x = dataMapping$x,
        fill = "Legend"
      ),
      position = ggplot2::position_stack(),
      bins = bins,
      size = 0.5,
      color = "black",
      alpha = 0.8
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      size = 1
    ) +
    ggplot2::geom_line(
      data = densityData,
      mapping = ggplot2::aes_string(x = "x", y = "y"),
      size = 1
    )
  # Legends and axis
  resHistoPlot <- tlf::setLegendPosition(plotObject = resHistoPlot, position = reDefaultLegendPosition)

  # Ensure that the legend has no title
  resHistoPlot <- resHistoPlot + ggplot2::theme(legend.title = element_blank())

  return(resHistoPlot)
}

#' @title plotResidualsQQPlot
#' @description Plot quantile-quantile plot for residuals
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `HistogramDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of log residuals qq-plot
#' @export
#' @import tlf
#' @import stats
#' @import ggplot2
plotResidualsQQPlot <- function(data,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  dataMapping <- dataMapping %||% tlf::HistogramDataMapping$new(x = "Residuals", fill = "Legend")

  plotConfiguration <- plotConfiguration %||% tlf::HistogramPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping,
    xlabel = "Standard Normal Quantiles",
    ylabel = "Quantiles of residuals"
  )

  qqPlot <- tlf::initializePlot(plotConfiguration)

  qqPlot <- qqPlot +
    ggplot2::geom_qq_line(
      data = data,
      mapping = ggplot2::aes_string(
        sample = dataMapping$x
      ),
      size = 1
    ) +
    ggplot2::geom_qq(
      data = data,
      mapping = ggplot2::aes_string(
        sample = dataMapping$x,
        color = "Legend"
      )
    )

  # Legends and axis
  qqPlot <- tlf::setLegendPosition(plotObject = qqPlot, position = reDefaultLegendPosition)
  qqPlot <- qqPlot + ggplot2::theme(legend.title = element_blank())

  return(qqPlot)
}

getSimulationTimeRanges <- function(simulation, path, simulationSet, logFolder) {
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
  logWorkflow(
    message = paste0(
      "'", length(applications), "' applications identified for path '",
      path, "' in simulation '", simulation$name, "'"
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  logWorkflow(
    message = paste0(
      "Corresponding time ranges: '",
      paste0(simulationRanges, collapse = "', '"), "'"
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  # Define ranges for output
  # Depending on expected behaviour of settings$applicationRange
  # It would be possible to set these values
  timeRanges$total$values <- c(min(simulationRanges), max(simulationRanges))

  # Case of multiple applications, get first and last
  if (!isOfLength(simulationRanges, 2)) {
    timeRanges$firstApplication$values <- utils::head(simulationRanges, 2)
    timeRanges$lastApplication$values <- utils::tail(simulationRanges, 2)
    timeRanges$firstApplication$keep <- applicationRanges[[ApplicationRanges$firstApplication]]
    timeRanges$lastApplication$keep <- applicationRanges[[ApplicationRanges$lastApplication]]
  }

  return(timeRanges)
}

asTimeAfterDose <- function(data, doseTime, maxTime = NULL) {
  if (isOfLength(data, 0)) {
    # Return empty data.frame (consitent class with data[dataFilter, ])
    return(data.frame())
  }
  dataFilter <- data$Time >= doseTime
  if (!is.null(maxTime)) {
    dataFilter <- dataFilter & data$Time <= maxTime
  }
  data$Time <- data$Time - doseTime
  return(data[dataFilter, ])
}

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


getTimeProfilePlotResults <- function(workflowType, timeRange, simulatedData, observedData = NULL, lloqData = NULL, metaDataFrame, timeProfileMapping, structureSet, settings = NULL, logFolder) {
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  # Add reference data, if any, to the existing data
  # rbind.data.frame enforces data.frame type and nrow can be used as is
  refSimulatedData <- rbind.data.frame(settings$referenceData$simulatedData, simulatedData)
  refObservedData <- rbind.data.frame(settings$referenceData$observedData, observedData)
  refLloqData <- rbind.data.frame(settings$referenceData$lloqData, lloqData)

  simulatedData <- asTimeAfterDose(refSimulatedData, min(timeRange), max(timeRange))
  observedData <- asTimeAfterDose(refObservedData, min(timeRange), max(timeRange))
  lloqData <- asTimeAfterDose(refLloqData, min(timeRange), max(timeRange))

  logWorkflow(
    message = messages$dataIncludedInTimeRange(nrow(simulatedData), nrow(refSimulatedData), timeRange, structureSet$simulationSet$timeUnit, "simulated"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  logWorkflow(
    message = messages$dataIncludedInTimeRange(nrow(observedData), nrow(refObservedData), timeRange, structureSet$simulationSet$timeUnit, "observed"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  logWorkflow(
    message = messages$dataIncludedInTimeRange(nrow(lloqData), nrow(refLloqData), timeRange, structureSet$simulationSet$timeUnit, "lloq observed"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  for (unit in unique(metaDataFrame$unit)) {
    selectedDimension <- utils::head(metaDataFrame$dimension[metaDataFrame$unit %in% unit], 1)
    selectedPaths <- metaDataFrame$path[metaDataFrame$unit %in% unit]
    selectedSimulatedData <- simulatedData[simulatedData$Path %in% selectedPaths, ]
    selectedObservedData <- observedData[observedData$Path %in% selectedPaths, ]
    selectedLloqData <- lloqData[lloqData$Path %in% selectedPaths, ]

    if (nrow(selectedObservedData) == 0) {
      selectedObservedData <- NULL
    }
    if (nrow(selectedLloqData) == 0) {
      selectedLloqData <- NULL
    }

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

getResidualsPlotResults <- function(timeRange, residualsData, metaDataFrame, structureSet, settings = NULL, logFolder) {
  residualsMetaData <- NULL
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  # Residuals can contain Inf/NA values which need to be seen in csv output file
  # While removed from the ggplot object
  csvResidualsData <- residualsData
  csvResidualsData[, "Residuals"] <- replaceInfWithNA(csvResidualsData[, "Residuals"], logFolder)

  # rbind.data.frame enforces data.frame type and nrow can be used as is
  refResidualsData <- rbind.data.frame(settings$referenceData$residualsData, csvResidualsData)
  refResidualsData <- removeMissingValues(refResidualsData, "Residuals", logFolder)
  residualsData <- asTimeAfterDose(refResidualsData, min(timeRange), max(timeRange))

  logWorkflow(
    message = messages$dataIncludedInTimeRange(nrow(residualsData), nrow(refResidualsData), timeRange, structureSet$simulationSet$timeUnit, "residuals"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

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

    obsVsPredPlot <- plotMeanObsVsPred(
      data = selectedResidualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
    )
    obsVsPredPlotLog <- tlf::setYAxis(plotObject = obsVsPredPlot, scale = tlf::Scaling$log)
    obsVsPredPlotLog <- tlf::setXAxis(plotObject = obsVsPredPlotLog, scale = tlf::Scaling$log)

    goodnessOfFitPlots[[paste0("obsVsPred-", selectedDimension)]] <- obsVsPredPlot
    goodnessOfFitPlots[[paste0("obsVsPredLog-", selectedDimension)]] <- obsVsPredPlotLog

    goodnessOfFitCaptions[[paste0("obsVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "linear")
    goodnessOfFitCaptions[[paste0("obsVsPredLog-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "logarithmic")

    goodnessOfFitPlots[[paste0("resVsPred-", selectedDimension)]] <- plotMeanResVsPred(
      data = selectedResidualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsPred"]]
    )

    goodnessOfFitCaptions[[paste0("resVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "resVsPred", residualScale)
  }

  residualsMetaData <- list(
    "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit),
    "Residuals" = list(dimension = residualsLegend, unit = "")
  )

  goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
    data = residualsData,
    metaData = residualsMetaData,
    plotConfiguration = settings$plotConfigurations[["resVsTime"]]
  )
  goodnessOfFitCaptions[["resVsTime"]] <- getGoodnessOfFitCaptions(structureSet, "resVsTime", residualScale)

  goodnessOfFitPlots[["resHisto"]] <- plotResidualsHistogram(
    data = residualsData,
    metaData = residualsMetaData,
    plotConfiguration = settings$plotConfigurations[["resHisto"]],
    bins = settings$bins
  )
  goodnessOfFitCaptions[["resHisto"]] <- getGoodnessOfFitCaptions(structureSet, "resHisto", residualScale)

  goodnessOfFitPlots[["resQQPlot"]] <- plotResidualsQQPlot(
    data = residualsData,
    metaData = residualsMetaData,
    plotConfiguration = settings$plotConfigurations[["resQQPlot"]]
  )
  goodnessOfFitCaptions[["resQQPlot"]] <- getGoodnessOfFitCaptions(structureSet, "resQQPlot", residualScale)

  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    data = csvResidualsData,
    metaData = residualsMetaData
  ))
}
