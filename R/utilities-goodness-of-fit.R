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
  residualsData <- NULL
  residualsMetaData <- NULL
  goodnessOfFitPlots <- NULL

  # Load observed and simulated data
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  simulationResult <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )

  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    observedDataset <- readObservedDataFile(structureSet$simulationSet$observedDataFile)

    dictionary <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)
    timeColumn <- as.character(dictionary[dictionary[, "ID"] == "time", "nonmenColumn"])
    dvColumn <- as.character(dictionary[dictionary[, "ID"] == "dv", "nonmenColumn"])
  }

  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL
    outputObservedData <- NULL
    outputResidualsData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult,
      quantitiesOrPaths = output$path
    )
    molWeight <- simulation$molWeightFor(output$path)

    outputSimulatedData <- data.frame(
      "Time" = toUnit("Time", simulationPathResults$data[, "Time"], structureSet$simulationSet$timeUnit),
      "Concentration" = ifnotnull(
        output$displayUnit,
        toUnit(simulationQuantity,
          simulationPathResults$data[, output$path],
          output$displayUnit,
          molWeight = molWeight
        ),
        simulationPathResults$data[, output$path]
      ),
      "Legend" = output$displayName %||% output$path
    )

    if (!is.null(output$dataFilter)) {
      rowFilter <- evalDataFilter(observedDataset, output$dataFilter)
      logWorkflow(
        message = paste0("Output '", output$path, "'. Number of observations filtered: ", sum(rowFilter)),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )

      outputObservedData <- data.frame(
        "Time" = observedDataset[rowFilter, timeColumn],
        "Concentration" = observedDataset[rowFilter, dvColumn],
        "Legend" = output$dataDisplayName
      )

      outputResiduals <- getResiduals(outputObservedData, outputSimulatedData)
    }
    simulatedData <- rbind.data.frame(
      simulatedData,
      outputSimulatedData
    )
    observedData <- rbind.data.frame(
      observedData,
      outputObservedData
    )
    residualsData <- rbind.data.frame(
      residualsData,
      outputResiduals
    )
  }

  timeProfileData <- rbind.data.frame(
    observedData,
    simulatedData
  )

  # TO DO: so far only using the last quantity to get the ylabel and its unit
  # Need to document how to change that from settings
  timeProfileMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit %||% simulationQuantity$displayUnit
    )
  )

  timeProfileMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Concentration",
    color = "Legend"
  )

  timeProfilePlot <- plotMeanTimeProfile(
    simulatedData = simulatedData,
    observedData = observedData,
    metaData = timeProfileMetaData,
    dataMapping = timeProfileMapping,
    plotConfiguration = settings$plotConfigurations[["timeProfile"]]
  )

  timeProfilePlotLog <- timeProfilePlot + ggplot2::scale_y_continuous(trans = "log10")

  goodnessOfFitPlots[["timeProfile"]] <- timeProfilePlot
  goodnessOfFitPlots[["timeProfileLog"]] <- timeProfilePlotLog

  if (!is.null(residualsData)) {
    # Smart plotConfig labels metaData$dimension [metaData$unit]
    residualsMetaData <- timeProfileMetaData
    residualsMetaData[["Observed"]] <- timeProfileMetaData[["Concentration"]]
    residualsMetaData[["Simulated"]] <- timeProfileMetaData[["Concentration"]]
    residualsMetaData[["Residuals"]] <- list(unit = "", dimension = "Residuals\nlog(Observed)-log(Simulated)")

    residualsMetaData[["Observed"]]$dimension <- paste0("Observed ", output$displayName %||% output$path)
    residualsMetaData[["Simulated"]]$dimension <- paste0("Simulated ", output$displayName %||% output$path)

    goodnessOfFitPlots[["obsVsPred"]] <- plotMeanObsVsPred(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
    )

    goodnessOfFitPlots[["obsVsPredLog"]] <- goodnessOfFitPlots[["obsVsPred"]] +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::scale_x_continuous(trans = "log10")

    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsTime"]]
    )

    goodnessOfFitPlots[["resVsPred"]] <- plotMeanResVsPred(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsPred"]]
    )
  }
  
  return(list(
    plots = goodnessOfFitPlots,
    tables = list(timeProfileData = timeProfileData),
    residuals = list(
      data = residualsData,
      metaData = residualsMetaData
    )
  ))
}

#' @title plotMeanTimeProfile
#' @description Plot time profile for mean model workflow
#' @param simulatedData data.frame of observed data
#' @param observedData data.frame of simulated data
#' @param metaData meta data on `data`
#' @param dataMapping `TimeProfileDataMapping` R6 class object from `tlf` library
#' @param plotCOnfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanTimeProfile <- function(simulatedData,
                                observedData = NULL,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  timeProfilePlot <- tlf::addLine(
    data = simulatedData,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  if (!is.null(observedData)) {
    timeProfilePlot <- tlf::addScatter(
      data = observedData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotObject = timeProfilePlot
    )
  }
  return(timeProfilePlot)
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
    "Legend" = observedData[, "Legend"]
  )

  # Remove Inf caused by obs = 0 which crash the axis sizing
  residualsData <- residualsData[!is.infinite(residualsData[, "Residuals"]), ]

  return(residualsData)
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

  meanResVsTimePlot <- meanResVsTimePlot + ggplot2::scale_y_continuous(limits = c(-maxRes, maxRes))
    
  return(meanResVsTimePlot)
}

#' @title plotMeanResVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
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

  meanResVsPredPlot <- meanResVsPredPlot + ggplot2::scale_y_continuous(limits = c(-maxRes, maxRes))
  return(meanResVsPredPlot)
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
  residualsData <- NULL
  residualsMetaData <- NULL
  goodnessOfFitPlots <- NULL

  residualsAggregationType <- settings$residualsAggregationType %||% "mean"
  selectedVariablesForResiduals <- c("Time", "mean", "legendMean")
  if (residualsAggregationType == "median") {
    selectedVariablesForResiduals <- c("Time", "median", "legendMedian")
  }
  aggregateNames <- c("mean", "median", "lowPerc", "highPerc")

  lowPerc <- function(x) {
    as.numeric(quantile(x, probs = 0.05))
  }
  highPerc <- function(x) {
    as.numeric(quantile(x, probs = 0.95))
  }

  # Load observed and simulated data
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  simulationResult <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )

  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    observedDataset <- readObservedDataFile(structureSet$simulationSet$observedDataFile)

    dictionary <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)
    timeColumn <- as.character(dictionary[dictionary[, "ID"] == "time", "nonmenColumn"])
    dvColumn <- as.character(dictionary[dictionary[, "ID"] == "dv", "nonmenColumn"])
  }

  for (output in structureSet$simulationSet$outputs) {
    outputSimulatedData <- NULL
    outputObservedData <- NULL
    outputResidualsData <- NULL

    simulationQuantity <- ospsuite::getQuantity(output$path, simulation)
    simulationPathResults <- ospsuite::getOutputValues(simulationResult,
      quantitiesOrPaths = output$path
    )
    molWeight <- simulation$molWeightFor(output$path)

    # Get the aggregation results
    aggregateSummary <- tlf::AggregationSummary$new(
      data = simulationPathResults$data,
      metaData = simulationPathResults$metaData,
      xColumnNames = "Time",
      yColumnNames = output$path,
      aggregationFunctionsVector = c(mean, median, lowPerc, highPerc),
      aggregationFunctionNames = aggregateNames
    )

    aggregateData <- aggregateSummary$dfHelper
    aggregateData$Time <- toUnit("Time", aggregateData$Time, structureSet$simulationSet$timeUnit)

    convertExpressions <- parse(text = paste0(
      "aggregateData$", aggregateNames, "<- ifnotnull(output$displayUnit,",
      "toUnit(simulationQuantity, aggregateData$", aggregateNames, ", output$displayUnit, molWeight = molWeight),",
      "aggregateData$", aggregateNames, ")"
    ))
    eval(convertExpressions)

    legendExpressions <- parse(text = paste0(
      "aggregateData$", c("legendMean", "legendMedian", "legendRange"),
      '<- paste0("Simulated ', c("mean", "median", "5th-95th percentile"),
      ' for ", output$displayName)'
    ))
    eval(legendExpressions)

    outputSimulatedData <- aggregateData

    if (!is.null(output$dataFilter)) {
      rowFilter <- evalDataFilter(observedDataset, output$dataFilter)
      logWorkflow(
        message = paste0("Output '", output$path, "'. Number of observations filtered: ", sum(rowFilter)),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )

      outputObservedData <- data.frame(
        "Time" = observedDataset[rowFilter, timeColumn],
        "Concentration" = observedDataset[rowFilter, dvColumn],
        "Legend" = output$dataDisplayName
      )

      simulatedDataForResiduals <- outputSimulatedData[, selectedVariablesForResiduals]
      # getResiduals is based on mean workflow whose names are c("Time", "Concentration", "Legend")
      names(simulatedDataForResiduals) <- c("Time", "Concentration", "Legend")

      outputResiduals <- getResiduals(
        outputObservedData,
        simulatedDataForResiduals
      )
    }

    simulatedData <- rbind.data.frame(
      simulatedData,
      outputSimulatedData
    )
    observedData <- rbind.data.frame(
      observedData,
      outputObservedData
    )
    residualsData <- rbind.data.frame(
      residualsData,
      outputResiduals
    )
  }

  # TO DO: so far only using the last quantity to get the ylabel and its unit
  # Need to document how to change that from settings
  timeProfileMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = output$displayUnit %||% simulationQuantity$displayUnit
    )
  )

  timeProfileMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Concentration",
    color = "Legend"
  )

  timeProfilePlot <- plotPopulationTimeProfile(
    simulatedData = simulatedData,
    observedData = observedData,
    metaData = timeProfileMetaData,
    dataMapping = timeProfileMapping,
    plotConfiguration = settings$plotConfigurations[["timeProfile"]]
  )

  timeProfilePlotLog <- timeProfilePlot + ggplot2::scale_y_continuous(trans = "log10")

  goodnessOfFitPlots[["timeProfile"]] <- timeProfilePlot
  goodnessOfFitPlots[["timeProfileLog"]] <- timeProfilePlotLog

  if (!is.null(residualsData)) {
    # Smart plotConfig labels metaData$dimension [metaData$unit]
    residualsMetaData <- timeProfileMetaData
    residualsMetaData[["Observed"]] <- timeProfileMetaData[["Concentration"]]
    residualsMetaData[["Simulated"]] <- timeProfileMetaData[["Concentration"]]
    residualsMetaData[["Residuals"]] <- list(unit = "", dimension = "Residuals\nlog(Observed)-log(Simulated)")

    residualsMetaData[["Observed"]]$dimension <- paste0("Observed ", output$displayName %||% output$path)
    residualsMetaData[["Simulated"]]$dimension <- paste0("Simulated ", output$displayName %||% output$path)

    goodnessOfFitPlots[["obsVsPred"]] <- plotMeanObsVsPred(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
    )

    goodnessOfFitPlots[["obsVsPredLog"]] <- goodnessOfFitPlots[["obsVsPred"]] +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::scale_x_continuous(trans = "log10")

    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsTime"]]
    )

    goodnessOfFitPlots[["resVsPred"]] <- plotMeanResVsPred(
      data = residualsData,
      metaData = residualsMetaData,
      plotConfiguration = settings$plotConfigurations[["resVsPred"]]
    )
  }
  return(list(
    plots = goodnessOfFitPlots,
    tables = list(
      observedData = observedData,
      simulatedData = simulatedData
    ),
    residuals = list(
      data = residualsData,
      metaData = residualsMetaData
    )
  ))
}

#' @title plotPopulationTimeProfile
#' @description Plot time profile for population model workflow
#' @param simulatedData data.frame of observed data
#' @param observedData data.frame of simulated data
#' @param metaData meta data on `data`
#' @param dataMapping `TimeProfileDataMapping` R6 class object from `tlf` library
#' @param plotCOnfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotPopulationTimeProfile <- function(simulatedData,
                                      observedData = NULL,
                                      dataMapping = NULL,
                                      metaData = NULL,
                                      plotConfiguration = NULL) {
  timeProfilePlot <- tlf::addRibbon(
    x = simulatedData$Time,
    ymin = simulatedData$lowPerc,
    ymax = simulatedData$highPerc,
    caption = simulatedData$legendRange[1],
    alpha = 0.6,
    plotConfiguration = plotConfiguration
  )
  timeProfilePlot <- tlf::addLine(
    x = simulatedData$Time,
    y = simulatedData$median,
    caption = simulatedData$legendMedian[1],
    plotObject = timeProfilePlot
  )
  timeProfilePlot <- tlf::addLine(
    x = simulatedData$Time,
    y = simulatedData$mean,
    caption = simulatedData$legendMean[1],
    plotObject = timeProfilePlot
  )
  if (!is.null(observedData)) {
    timeProfilePlot <- tlf::addScatter(
      data = observedData,
      metaData = metaData,
      dataMapping = dataMapping,
      plotConfiguration = plotConfiguration,
      plotObject = timeProfilePlot
    )
  }
  timeProfilePlot <- timeProfilePlot +
    ggplot2::xlab(tlf::getLabelWithUnit(metaData$Time$dimension, metaData$Time$unit)) +
    ggplot2::ylab(tlf::getLabelWithUnit(metaData$Concentration$dimension, metaData$Concentration$unit))

  return(timeProfilePlot)
}
