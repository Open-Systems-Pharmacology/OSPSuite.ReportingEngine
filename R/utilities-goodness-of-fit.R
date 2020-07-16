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

  observedData <- NULL
  simulatedData <- NULL
  lloqData <- NULL
  residualsData <- NULL
  residualsMetaData <- NULL
  residuals <- NULL
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()
  goodnessOfFitResiduals <- list()

  # Load observed and simulated data
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResult <- ospsuite::importResultsFromCSV(simulation, structureSet$simulationResultFileNames)

  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$observedDataFile)
    observedDataset <- readObservedDataFile(structureSet$simulationSet$observedDataFile)

    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$observedMetaDataFile)
    dictionary <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)

    timeColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
    dvColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
    lloqColumn <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)
  }

  outputSimulatedMetaData <- list()
  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL
    outputObservedData <- NULL
    outputLloqData <- NULL
    outputResidualsData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getOutputSimulatedResults(simulationPathResults, output, simulationQuantity, molWeight, structureSet$simulationSet$timeUnit)

    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    if (!is.null(output$dataSelection)) {
      rowFilter <- evalDataFilter(observedDataset, output$dataSelection)
      logWorkflow(
        message = paste0("Output '", output$path, "'. Number of observations filtered: ", sum(rowFilter)),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )

      outputObservedData <- data.frame(
        "Time" = observedDataset[rowFilter, timeColumn],
        "Concentration" = observedDataset[rowFilter, dvColumn],
        "Legend" = output$dataDisplayName,
        "Path" = output$path
      )
      outputResidualsData <- getResiduals(outputObservedData, outputSimulatedData)
      outputResidualsData <- removeMissingValues(outputResidualsData, "Residuals", logFolder)
      
      if (!isOfLength(lloqColumn, 0)) {
        outputLloqData <- data.frame(
          "Time" = observedDataset[rowFilter, timeColumn],
          "Concentration" = observedDataset[rowFilter, lloqColumn],
          "Legend" = "LLOQ",
          "Path" = output$path
        )
      }
    }

    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedData)
    lloqData <- rbind.data.frame(lloqData, outputLloqData)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileData <- rbind.data.frame(observedData, lloqData, simulatedData)
  timeProfileMapping <- tlf::XYGDataMapping$new(x = "Time", y = "Concentration", color = "Legend")

  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet$timeUnit)

  for (timeRange in timeRanges) {
    if (!is.null(timeRange$values)) {
      timeProfilePlotResults <- getTimeProfilePlotResults("mean", timeRange$values, simulatedData, observedData, lloqData, metaDataFrame, timeProfileMapping, structureSet, settings, logFolder)
      goodnessOfFitPlots[[timeRange$name]] <- timeProfilePlotResults$plots
      goodnessOfFitCaptions[[timeRange$name]] <- timeProfilePlotResults$captions
    }
  }

  if (!isOfLength(residualsData, 0)) {
    for (timeRange in timeRanges) {
      if (!is.null(timeRange$values)) {
        residualsPlotResults <- getResidualsPlotResults(timeRange$values, residualsData, metaDataFrame, structureSet, settings, logFolder)
        goodnessOfFitPlots[[timeRange$name]] <- c(goodnessOfFitPlots[[timeRange$name]], residualsPlotResults$plots)
        goodnessOfFitCaptions[[timeRange$name]] <- c(goodnessOfFitCaptions[[timeRange$name]], residualsPlotResults$captions)
        goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
        residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
      }
    }
  }
  if (!isOfLength(goodnessOfFitResiduals[["totalRange"]], 0)) {
    residuals <- list(
      data = goodnessOfFitResiduals[["totalRange"]],
      metaData = residualsMetaData[["totalRange"]]
    )
  }

  return(list(
    plots = goodnessOfFitPlots,
    tables = list(timeProfileData = timeProfileData),
    captions = goodnessOfFitCaptions,
    residuals = residuals
  ))
}

getOutputSimulatedResults <- function(simulationPathResults, output, simulationQuantity, molWeight, timeUnit) {
  outputSimulatedData <- data.frame(
    "Time" = ospsuite::toUnit("Time", simulationPathResults$data[, "Time"], timeUnit),
    "Concentration" = ifnotnull(
      output$displayUnit,
      ospsuite::toUnit(simulationQuantity,
        simulationPathResults$data[, output$path],
        output$displayUnit,
        molWeight = molWeight
      ),
      simulationPathResults$data[, output$path]
    ),
    "Legend" = output$displayName %||% output$path,
    "Path" = output$path
  )

  outputSimulatedMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = timeUnit
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
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
getResiduals <- function(observedData,
                         simulatedData) {

  # Time matrix to match observed time with closest simulation time
  obsTimeMatrix <- matrix(observedData[, "Time"], nrow(simulatedData), nrow(observedData), byrow = TRUE)
  simTimeMatrix <- matrix(simulatedData[, "Time"], nrow(simulatedData), nrow(observedData))

  timeMatchedData <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))

  residualsData <- data.frame(
    "Time" = observedData[, "Time"],
    "Observed" = observedData[, "Concentration"],
    "Simulated" = simulatedData[timeMatchedData, "Concentration"],
    "Residuals" = log(observedData[, "Concentration"]) - log(simulatedData[timeMatchedData, "Concentration"]),
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

  observedData <- NULL
  simulatedData <- NULL
  lloqData <- NULL
  residualsData <- NULL
  residualsMetaData <- NULL
  residuals <- NULL
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

  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$observedDataFile)
    observedDataset <- readObservedDataFile(structureSet$simulationSet$observedDataFile)

    re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$observedMetaDataFile)
    dictionary <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)

    timeColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
    dvColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
    lloqColumn <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)
  }

  outputSimulatedMetaData <- list()
  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL
    outputObservedData <- NULL
    outputLloqData <- NULL
    outputResidualsData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult, quantitiesOrPaths = output$path)
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedResults <- getPopulationOutputSimulatedResults(simulationPathResults, output, simulationQuantity, molWeight, structureSet$simulationSet$timeUnit, settings)

    outputSimulatedData <- outputSimulatedResults$data
    outputSimulatedMetaData[[output$path]] <- outputSimulatedResults$metaData

    if (!is.null(output$dataSelection)) {
      rowFilter <- evalDataFilter(observedDataset, output$dataSelection)
      logWorkflow(
        message = paste0("Output '", output$path, "'. Number of observations filtered: ", sum(rowFilter)),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )

      outputObservedData <- data.frame(
        "Time" = observedDataset[rowFilter, timeColumn],
        "Concentration" = observedDataset[rowFilter, dvColumn],
        "Legend" = output$dataDisplayName,
        "Path" = output$path
      )

      simulatedDataForResiduals <- outputSimulatedData[, selectedVariablesForResiduals]
      # getResiduals is based on mean workflow whose names are c("Time", "Concentration", "Legend", "Path")
      names(simulatedDataForResiduals) <- c("Time", "Concentration", "Legend", "Path")

      outputResidualsData <- getResiduals(outputObservedData, simulatedDataForResiduals)
      outputResidualsData <- removeMissingValues(outputResidualsData, "Residuals", logFolder)
      if (!isOfLength(lloqColumn, 0)) {
        outputLloqData <- data.frame(
          "Time" = observedDataset[rowFilter, timeColumn],
          "Concentration" = observedDataset[rowFilter, lloqColumn],
          "Legend" = "LLOQ",
          "Path" = output$path
        )
      }
    }

    simulatedData <- rbind.data.frame(simulatedData, outputSimulatedData)
    observedData <- rbind.data.frame(observedData, outputObservedData)
    lloqData <- rbind.data.frame(lloqData, outputLloqData)
    residualsData <- rbind.data.frame(residualsData, outputResidualsData)
  }

  timeProfileMapping <- tlf::XYGDataMapping$new(x = "Time", y = "Concentration", color = "Legend")
  # metaDataFrame summarizes paths, dimensions and units
  metaDataFrame <- getMetaDataFrame(outputSimulatedMetaData)

  timeRanges <- getSimulationTimeRanges(simulation, output$path, structureSet$simulationSet$timeUnit)

  for (timeRange in timeRanges) {
    if (!is.null(timeRange$values)) {
      timeProfilePlotResults <- getTimeProfilePlotResults("population", timeRange$values, simulatedData, observedData, lloqData, metaDataFrame, timeProfileMapping, structureSet, settings, logFolder)
      goodnessOfFitPlots[[timeRange$name]] <- timeProfilePlotResults$plots
      goodnessOfFitCaptions[[timeRange$name]] <- timeProfilePlotResults$captions
    }
  }

  if (!isOfLength(residualsData, 0)) {
    for (timeRange in timeRanges) {
      if (!is.null(timeRange$values)) {
        residualsPlotResults <- getResidualsPlotResults(timeRange$values, residualsData, metaDataFrame, structureSet, settings, logFolder)
        goodnessOfFitPlots[[timeRange$name]] <- c(goodnessOfFitPlots[[timeRange$name]], residualsPlotResults$plots)
        goodnessOfFitCaptions[[timeRange$name]] <- c(goodnessOfFitCaptions[[timeRange$name]], residualsPlotResults$captions)
        goodnessOfFitResiduals[[timeRange$name]] <- residualsPlotResults$data
        residualsMetaData[[timeRange$name]] <- residualsPlotResults$metaData
      }
    }
  }
  if (!isOfLength(goodnessOfFitResiduals[["totalRange"]], 0)) {
    residuals <- list(
      data = goodnessOfFitResiduals[["totalRange"]],
      metaData = residualsMetaData[["totalRange"]]
    )
  }
  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    tables = list(
      observedData = observedData,
      simulatedData = simulatedData
    ),
    residuals = residuals
  ))
}

getPopulationOutputSimulatedResults <- function(simulationPathResults, output, simulationQuantity, molWeight, timeUnit, settings = NULL) {
  aggregateNames <- c("mean", "median", "lowPerc", "highPerc")
  aggregateFunctions <- c(mean, median, AggregationConfiguration$functions$ymin, AggregationConfiguration$functions$ymax)

  # Get the aggregation results
  aggregateSummary <- tlf::AggregationSummary$new(
    data = simulationPathResults$data,
    metaData = simulationPathResults$metaData,
    xColumnNames = "Time",
    yColumnNames = output$path,
    aggregationFunctionsVector = aggregateFunctions,
    aggregationFunctionNames = aggregateNames
  )

  aggregateData <- aggregateSummary$dfHelper
  aggregateData$Time <- toUnit("Time", aggregateData$Time, timeUnit)

  convertExpressions <- parse(text = paste0(
    "aggregateData$", aggregateNames, "<- ifnotnull(output$displayUnit,",
    "toUnit(simulationQuantity, aggregateData$", aggregateNames, ", output$displayUnit, molWeight = molWeight),",
    "aggregateData$", aggregateNames, ")"
  ))
  eval(convertExpressions)

  legendExpressions <- parse(text = paste0(
    "aggregateData$", c("legendMean", "legendMedian", "legendRange"),
    '<- paste0("Simulated ', c("mean", "median", AggregationConfiguration$names$range),
    ' for ", output$displayName)'
  ))
  eval(legendExpressions)

  outputSimulatedData <- aggregateData
  outputSimulatedData$Path <- output$path

  outputSimulatedMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = timeUnit
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
    0.8 * min(cbind(data[, "Observed"], data[, "Simulated"])),
    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"]))
  )
  identityLine <- data.frame(
    "Observed" = identityMinMax,
    "Simulated" = identityMinMax
  )

  meanObsVsPredPlot <- tlf::addLine(data = identityLine, metaData = metaData, caption = "Line of identity", plotConfiguration = plotConfiguration)

  meanObsVsPredPlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Observed",
      y = "Simulated",
      color = "Legend"
    ),
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

  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]))

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping
  )

  meanResVsTimePlot <- tlf::addLine(y = 0, caption = "Line of residuals = 0", plotConfiguration = plotConfiguration)

  meanResVsTimePlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping,
    plotObject = meanResVsTimePlot
  )

  meanResVsTimePlot <- tlf::setYAxis(plotObject = meanResVsTimePlot, limits = c(-maxRes, maxRes))
  meanResVsTimePlot <- tlf::setLegendPosition(plotObject = meanResVsTimePlot, position = reDefaultLegendPosition)

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

  maxRes <- 1.2 * max(abs(data[, resVsPredDataMapping$y]))

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping
  )

  meanResVsPredPlot <- tlf::addLine(y = 0, caption = "Line of residuals = 0", plotConfiguration = plotConfiguration)

  meanResVsPredPlot <- tlf::addScatter(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping,
    plotObject = meanResVsPredPlot
  )

  meanResVsPredPlot <- tlf::setYAxis(plotObject = meanResVsPredPlot, limits = c(-maxRes, maxRes))
  meanResVsPredPlot <- tlf::setLegendPosition(plotObject = meanResVsPredPlot, position = reDefaultLegendPosition)

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
  timeProfilePlot <- tlf::addRibbon(
    x = simulatedData$Time,
    ymin = simulatedData$lowPerc,
    ymax = simulatedData$highPerc,
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
  timeProfilePlot <- timeProfilePlot +
    ggplot2::xlab(tlf::getLabelWithUnit(metaData$Time$dimension, metaData$Time$unit)) +
    ggplot2::ylab(tlf::getLabelWithUnit(metaData$Concentration$dimension, metaData$Concentration$unit))

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
    dataMapping = dataMapping
  )
  bins <- bins %||% 15
  xmax <- 1.1 * max(abs(data[, dataMapping$x]))
  xDensityData <- seq(-xmax, xmax, 2 * xmax / 100)
  yDensityData <- (nrow(data) / bins) * stats::dnorm(xDensityData, sd = stats::sd(data[, dataMapping$x]))
  densityData <- data.frame(x = xDensityData, y = yDensityData)

  resHistoPlot <- tlf::initializePlot(plotConfiguration)

  # TO DO: Create a place where all the default values are stored
  resHistoPlot <- resHistoPlot +
    ggplot2::geom_histogram(
      data = data,
      mapping = ggplot2::aes_string(
        x = dataMapping$x,
        fill = dataMapping$groupMapping$fill$label
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

  resHistoPlot <- resHistoPlot +
    ggplot2::ylab("Number of residuals") +
    ggplot2::theme(legend.title = element_blank())

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
    dataMapping = dataMapping
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
        color = dataMapping$groupMapping$fill$label
      )
    )

  # Legends and axis
  qqPlot <- tlf::setLegendPosition(plotObject = qqPlot, position = reDefaultLegendPosition)
  qqPlot <- qqPlot +
    ggplot2::xlab("Standard Normal Quantiles") + ggplot2::ylab("Quantiles of residuals") +
    ggplot2::theme(legend.title = element_blank())

  return(qqPlot)
}

getSimulationTimeRanges <- function(simulation, path, timeUnit) {
  firstApplicationRange <- list(name = "firstApplicationRange", values = NULL)
  lastApplicationRange <- list(name = "lastApplicationRange", values = NULL)
  applicationTimes <- 0

  applications <- simulation$allApplicationsFor(path)
  if (!isOfLength(applications, 0)) {
    applicationTimes <- sapply(applications, function(application) {
      application$startTime$value
    })
  }
  simulationRanges <- c(applicationTimes, simulation$outputSchema$endTime)
  simulationRanges <- sort(ospsuite::toUnit("Time", simulationRanges, timeUnit))

  totalRange <- list(name = "totalRange", values = c(min(simulationRanges), max(simulationRanges)))

  if (!isOfLength(simulationRanges, 2)) {
    firstApplicationRange$values <- utils::head(simulationRanges, 2)
    lastApplicationRange$values <- utils::tail(simulationRanges, 2)
  }

  return(list(
    totalRange = totalRange,
    firstApplicationRange = firstApplicationRange,
    lastApplicationRange = lastApplicationRange
  ))
}

asTimeAfterDose <- function(data, doseTime, maxTime = NULL) {
  if (isOfLength(data, 0)) {
    return()
  }
  dataFilter <- data$Time >= doseTime
  if (!is.null(maxTime)) {
    dataFilter <- dataFilter & data$Time <= maxTime
  }
  data$Time <- data$Time - doseTime
  return(data[dataFilter, ])
}

getMetaDataFrame <- function(listOfMetaData) {
  return(data.frame(
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
  ))
}


getTimeProfilePlotResults <- function(workflowType, timeRange, simulatedData, observedData = NULL, lloqData = NULL, metaDataFrame, timeProfileMapping, structureSet, settings = NULL, logFolder) {
  goodnessOfFitPlots <- list()
  goodnessOfFitCaptions <- list()

  refLengthSimulatedData <- nrow(simulatedData) %||% 0
  refLengthObservedData <- nrow(observedData) %||% 0
  refLengthLloqData <- nrow(lloqData) %||% 0

  simulatedData <- asTimeAfterDose(simulatedData, min(timeRange), max(timeRange))
  observedData <- asTimeAfterDose(observedData, min(timeRange), max(timeRange))
  lloqData <- asTimeAfterDose(lloqData, min(timeRange), max(timeRange))

  newLengthSimulatedData <- nrow(simulatedData) %||% 0
  newLengthObservedData <- nrow(observedData) %||% 0
  newLengthLloqData <- nrow(lloqData) %||% 0

  logWorkflow(
    message = paste0(
      newLengthSimulatedData, " simulation data were included in the analysis between ",
      min(timeRange), " and ", max(timeRange), " ", structureSet$simulationSet$timeUnit,
      ". Initial size was ", refLengthSimulatedData, "."
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  logWorkflow(
    message = paste0(
      newLengthObservedData, " observed data were included in the analysis between ",
      min(timeRange), " and ", max(timeRange), " ", structureSet$simulationSet$timeUnit,
      ". Initial size was ", refLengthObservedData, "."
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  logWorkflow(
    message = paste0(
      newLengthLloqData, " lloq data were included in the analysis between ",
      min(timeRange), " and ", max(timeRange), " ", structureSet$simulationSet$timeUnit,
      ". Initial size was ", refLengthLloqData, "."
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  for (unit in unique(metaDataFrame$unit)) {
    selectedDimension <- utils::head(metaDataFrame$dimension[metaDataFrame$unit %in% unit], 1)
    selectedPaths <- metaDataFrame$path[metaDataFrame$unit %in% unit]
    selectedSimulatedData <- simulatedData[simulatedData$Path %in% selectedPaths, ]
    selectedObservedData <- observedData[observedData$Path %in% selectedPaths, ]
    selectedLloqData <- lloqData[lloqData$Path %in% selectedPaths, ]
    if (sum(observedData$Path %in% selectedPaths) == 0) {
      selectedObservedData <- NULL
    }
    if (sum(lloqData$Path %in% selectedPaths) == 0) {
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

    timeProfilePlotLog <- tlf::setYAxis(plotObject = timeProfilePlot, scale = tlf::Scaling$log10)

    goodnessOfFitPlots[[paste0("timeProfile-", selectedDimension)]] <- timeProfilePlot
    goodnessOfFitPlots[[paste0("timeProfileLog-", selectedDimension)]] <- timeProfilePlotLog

    goodnessOfFitCaptions[[paste0("timeProfile-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile")
    goodnessOfFitCaptions[[paste0("timeProfileLog-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "timeProfile", "log")
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

  refLengthResidualsData <- nrow(residualsData) %||% 0
  residualsData <- asTimeAfterDose(residualsData, min(timeRange), max(timeRange))
  newLengthResidualsData <- nrow(residualsData) %||% 0

  logWorkflow(
    message = paste0(
      newLengthResidualsData, " residuals data were included in the analysis between ",
      min(timeRange), " and ", max(timeRange), " ", structureSet$simulationSet$timeUnit,
      ". Initial size was ", refLengthResidualsData, "."
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  if (newLengthResidualsData > 0) {
    for (unit in unique(metaDataFrame$unit)) {
      selectedDimension <- utils::head(metaDataFrame$dimension[metaDataFrame$unit %in% unit], 1)
      selectedPaths <- metaDataFrame$path[metaDataFrame$unit %in% unit]
      selectedResidualsData <- residualsData[residualsData$Path %in% selectedPaths, ]

      if (sum(residualsData$Path %in% selectedPaths) > 0) {
        residualsMetaData <- list(
          "Observed" = list(dimension = "Observed data", unit = unit),
          "Simulated" = list(dimension = "Simulated data", unit = unit),
          "Residuals" = list(unit = "", dimension = "Residuals\nlog(Observed)-log(Simulated)")
        )

        obsVsPredPlot <- plotMeanObsVsPred(
          data = selectedResidualsData,
          metaData = residualsMetaData,
          plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
        )
        obsVsPredPlotLog <- tlf::setYAxis(plotObject = obsVsPredPlot, scale = tlf::Scaling$log10)
        obsVsPredPlotLog <- tlf::setXAxis(plotObject = obsVsPredPlotLog, scale = tlf::Scaling$log10)

        goodnessOfFitPlots[[paste0("obsVsPred-", selectedDimension)]] <- obsVsPredPlot
        goodnessOfFitPlots[[paste0("obsVsPredLog-", selectedDimension)]] <- obsVsPredPlotLog

        goodnessOfFitCaptions[[paste0("obsVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred")
        goodnessOfFitCaptions[[paste0("obsVsPredLog-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "obsVsPred", "log")

        goodnessOfFitPlots[[paste0("resVsPred-", selectedDimension)]] <- plotMeanResVsPred(
          data = selectedResidualsData,
          metaData = residualsMetaData,
          plotConfiguration = settings$plotConfigurations[["resVsPred"]]
        )

        goodnessOfFitCaptions[[paste0("resVsPred-", selectedDimension)]] <- getGoodnessOfFitCaptions(structureSet, "resVsPred")
      }
    }

    residualsMetaData <- list(
      "Time" = list(dimension = "Time", unit = structureSet$simulationSet$timeUnit),
      "Residuals" = list(dimension = "Residuals\nlog(Observed)-log(Simulated)", unit = "")
    )

    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsTime"]]
    )
    goodnessOfFitCaptions[["resVsTime"]] <- getGoodnessOfFitCaptions(structureSet, "resVsTime")

    goodnessOfFitPlots[["resHisto"]] <- plotResidualsHistogram(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resHisto"]],
      bins = settings$bins
    )
    goodnessOfFitCaptions[["resHisto"]] <- getGoodnessOfFitCaptions(structureSet, "resHisto")

    goodnessOfFitPlots[["resQQPlot"]] <- plotResidualsQQPlot(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resQQPlot"]]
    )
    goodnessOfFitCaptions[["resQQPlot"]] <- getGoodnessOfFitCaptions(structureSet, "resQQPlot")
  }
  return(list(
    plots = goodnessOfFitPlots,
    captions = goodnessOfFitCaptions,
    data = residualsData,
    metaData = residualsMetaData
  ))
}
