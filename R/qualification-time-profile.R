#' @title plotQualificationTimeProfiles
#' @description Plot time profile for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @param settings `ConfigurationPlan` object
#' @return list with `plots` and `tables`
#' @export
#' @import tlf
#' @import ospsuite
plotQualificationTimeProfiles <- function(configurationPlan,
                                          logFolder = getwd(),
                                          settings) {
  timeProfileResults <- list()
  for (timeProfilePlan in configurationPlan$plots$TimeProfile) {
    # Create a unique ID for the plot name as <Plot index>-<Project>-<Simulation>
    plotID <- paste(length(timeProfileResults) + 1, timeProfilePlan$Project, timeProfilePlan$Simulation, sep = "-")
    # Get simulation and simulation results
    simulationFile <- configurationPlan$getSimulationPath(
      project = timeProfilePlan$Project,
      simulation = timeProfilePlan$Simulation
    )
    simulationResultsFile <- configurationPlan$getSimulationResultsPath(
      project = timeProfilePlan$Project,
      simulation = timeProfilePlan$Simulation
    )

    simulation <- ospsuite::loadSimulation(simulationFile)
    simulationResults <- ospsuite::importResultsFromCSV(simulation, simulationResultsFile)

    # Get axes properties (with scale, limits and display units)
    axesProperties <- getAxesProperties(timeProfilePlan$Plot$Axes) %||% settings$axes
    if (isOfLength(axesProperties, 0)) {
      # TODO Centralize messaging of configurtion plan errors and warnings
      logWorkflow(
        message = paste0(
          "In Time Profile Plots,\n",
          "No axes settings defined for plot: '", timeProfilePlan$Plot$Title %||% timeProfilePlan$Plot$Name, "'\n",
          "From Project: '", timeProfilePlan$Project, "' and Simulation: '", timeProfilePlan$Simulation, "'"
        ),
        pathFolder = logFolder,
        logTypes = LogTypes$Error
      )
      next
    }

    plotConfiguration <- getPlotConfigurationFromPlan(timeProfilePlan$Plot)
    timeProfilePlot <- tlf::initializePlot(plotConfiguration)

    # Currently use ifnotnull to select if mean or population time profile
    # So far only population defines "Type" but this might not be always true
    # Function switch could be used instead
    timeProfilePlot <- ifnotnull(
      timeProfilePlan$Plot$Type,
      plotQualificationPopulationTimeProfile(
        simulationAnalysis = timeProfilePlan$Plot$Analysis,
        observedDataCollection = timeProfilePlan$Plot$ObservedDataCollection,
        simulation = simulation,
        simulationResults = simulationResults,
        axesProperties = axesProperties,
        configurationPlan = configurationPlan,
        plotObject = timeProfilePlot,
        logFolder = logFolder
      ),
      plotQualificationMeanTimeProfile(
        configurationPlanCurves = timeProfilePlan$Plot$Curves,
        simulation = simulation,
        simulationResults = simulationResults,
        axesProperties = axesProperties,
        configurationPlan = configurationPlan,
        plotObject = timeProfilePlot,
        logFolder = logFolder
      )
    )

    # Set axes based on Axes properties
    timeProfilePlot <- updatePlotAxes(timeProfilePlot, axesProperties)

    # Save results
    timeProfileResults[[plotID]] <- saveTaskResults(
      id = plotID,
      sectionId = timeProfilePlan$SectionId,
      plot = timeProfilePlot,
      plotCaption = timeProfilePlan$Plot$Title %||% timeProfilePlan$Plot$Name
    )
  }
  return(timeProfileResults)
}


#' @title plotQualificationMeanTimeProfile
#' @description Plot mean time profile for qualification workflow
#' @param configurationPlanCurve `Curves` fields of configuration plan
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param simulationResults A `SimulationResults` object from `ospsuite` package
#' that includes the data requested from `configurationPlanCurve` properties
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @param plotObject A `ggplot` object
#' @param logFolder folder where the logs are saved
#' @return Mean time profile plot as a `ggplot` object
#' @export
#' @import tlf
plotQualificationMeanTimeProfile <- function(configurationPlanCurves, simulation, simulationResults, axesProperties, configurationPlan, plotObject, logFolder) {
  for (curve in configurationPlanCurves) {
    # TODO handle Observed data and Y2 axis
    curveOutput <- getCurvePropertiesForTimeProfiles(curve, simulation, simulationResults, axesProperties, configurationPlan, logFolder)
    if (is.null(curveOutput)) {
      next
    }
    if (!isOfLength(curveOutput$error, 0)) {
      plotObject <- tlf::addErrorbar(
        x = curveOutput$x,
        ymin = curveOutput$error$ymin,
        ymax = curveOutput$error$ymax,
        caption = curveOutput$caption,
        color = curveOutput$color,
        size = curveOutput$size,
        plotObject = plotObject
      )
    }
    plotObject <- tlf::addLine(
      x = curveOutput$x,
      y = curveOutput$y,
      caption = curveOutput$caption,
      color = curveOutput$color,
      linetype = curveOutput$linetype,
      size = curveOutput$size,
      shape = curveOutput$shape,
      plotObject = plotObject
    )
  }
  return(plotObject)
}

#' @title getCurvePropertiesForTimeProfiles
#' @description Identify and convert properties from `Curves` field of configuration plan
#' @param configurationPlanCurve list of properties defined in `Curves` field of configuration plan
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param simulationResults A `SimulationResults` object from `ospsuite` package
#' that includes the data requested from `configurationPlanCurve` properties
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @param logFolder folder where the logs are saved
#' @return A list of data and properties to be plotted and that follows `tlf` package nomenclature
#' @import ospsuite
getCurvePropertiesForTimeProfiles <- function(configurationPlanCurve,
                                              simulation,
                                              simulationResults,
                                              axesProperties,
                                              configurationPlan,
                                              logFolder) {
  # Check if curve is on first or seconf Y axis
  curveOnSecondAxis <- isTRUE(configurationPlanCurve$CurveOptions$yAxisType == "Y2")
  # TODO handle curve on Y2
  if (curveOnSecondAxis) {
    return()
  }

  # configurationPlanCurve$Y is a quantity path from the cnofiguration plan
  # e.g. "S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)"
  # or "Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc"
  pathArray <- ospsuite::toPathArray(configurationPlanCurve$Y)

  # Case path is Observed Data
  if (isObservedData(configurationPlanCurve$Y)) {
    observedDataId <- getObservedDataIdFromPath(configurationPlanCurve$Y)
    molWeight <- configurationPlan$getMolWeightForObservedData(observedDataId)
    if (is.na(molWeight)) {
      compoundName <- getCompoundNameFromPath(configurationPlanCurve$Y)
      molWeight <- getMolWeightForCompound(compoundName, simulation)
    }
    # observedResults is a list that includes
    # data: a data.frame with column 1 = Time, column 2 = Concentration, column 3 = Error
    # metaData: a list for each column of data that includes their unit
    observedResults <- getObservedDataFromConfigurationPlan(observedDataId, configurationPlan, logFolder)
    observedData <- getTimeProfileObservedDataFromResults(observedResults, molWeight, axesProperties, logFolder)

    outputCurve <- list(
      x = observedData$time,
      y = observedData$y,
      error = observedData$error,
      caption = configurationPlanCurve$Name,
      color = configurationPlanCurve$CurveOptions$Color,
      linetype = tlfLinetype(configurationPlanCurve$CurveOptions$LineStyle),
      shape = tlfShape(configurationPlanCurve$CurveOptions$Symbol),
      size = configurationPlanCurve$CurveOptions$Size,
      id = configurationPlanCurve$CurveOptions$LegendIndex,
      secondAxis = curveOnSecondAxis
    )
    return(outputCurve)
  }
  # Remove simulation name from path
  outputPath <- ospsuite::toPathString(pathArray[-1])
  # Get and convert output path values into display unit
  simulationQuantity <- ospsuite::getQuantity(outputPath, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = outputPath)
  molWeight <- simulation$molWeightFor(outputPath)

  time <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesProperties$x$unit
  )

  outputValues <- ospsuite::toUnit(simulationQuantity,
    simulationPathResults$data[, outputPath],
    axesProperties$y$unit,
    molWeight = molWeight
  )

  # If CurveOptions or one of its field is not defined,
  # the corresponding result will be NULL and default properties will be applied instead
  # If defined, they'll overwrite the default
  outputCurve <- list(
    x = time,
    y = outputValues,
    caption = configurationPlanCurve$Name,
    color = configurationPlanCurve$CurveOptions$Color,
    linetype = tlfLinetype(configurationPlanCurve$CurveOptions$LineStyle),
    shape = tlfShape(configurationPlanCurve$CurveOptions$Symbol),
    size = configurationPlanCurve$CurveOptions$Size,
    id = configurationPlanCurve$CurveOptions$LegendIndex,
    secondAxis = curveOnSecondAxis
  )

  return(outputCurve)
}

#' @title plotQualificationPopulationTimeProfile
#' @description Plot population time profile for qualification workflow
#' @param simulationAnalysis Field `Analysis` from `ConfigurationPlan` population time profile plot
#' @param observedDataCollection Field `ObservedDataCollection` from `ConfigurationPlan` population time profile plot
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param simulationResults A `SimulationResults` object from `ospsuite` package
#' that includes the data requested from `configurationPlanCurve` properties
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @param plotObject A `ggplot` object
#' @param logFolder folder where the logs are saved
#' @return Population time profile plot as a `ggplot` object
#' @export
#' @import tlf
plotQualificationPopulationTimeProfile <- function(simulationAnalysis, observedDataCollection, simulation, simulationResults, axesProperties, configurationPlan, plotObject, logFolder) {

  # Get simulation results from configuration plan field "Fields"
  outputPath <- simulationAnalysis$Fields[[1]]$QuantityPath
  simulationQuantity <- ospsuite::getQuantity(outputPath, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = outputPath)
  molWeight <- simulation$molWeightFor(outputPath)

  # Get and convert output path values into display unit
  time <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesProperties$x$unit
  )
  outputValues <- ospsuite::toUnit(
    simulationQuantity,
    simulationPathResults$data[, outputPath],
    simulationAnalysis$Fields[[1]]$Unit %||% axesProperties$y$unit,
    molWeight = molWeight
  )

  # Aggregate on Time for simulation results
  # based on field "Statistics"
  for (statistic in simulationAnalysis$Statistics) {
    plotObject <- plotStatisticsFromPlan(
      time,
      outputValues,
      statistic$Id,
      outputName = simulationAnalysis$Fields[[1]]$Name,
      color = simulationAnalysis$Fields[[1]]$Color,
      linetype = tlfLinetype(statistic$LineStyle),
      plotObject = plotObject
    )
  }

  # Observed data: Get observed results from configuration plan field "ObservedData"
  observedResults <- NULL
  for (observedDataId in observedDataCollection$ObservedData) {
    # observedResults is a list that includes
    # data: a data.frame with column 1 = Time, column 2 = Concentration, column 3 = Error
    # metaData: a list for each column of data that includes their unit
    observedResults <- getObservedDataFromConfigurationPlan(observedDataId, configurationPlan, logFolder)
    # Currently, the molecular weight is directly taken from the simulation output
    observedData <- getTimeProfileObservedDataFromResults(observedResults, molWeight, axesProperties, logFolder)

    if (!isOfLength(observedData$error, 0)) {
      plotObject <- tlf::addErrorbar(
        x = observedData$time,
        ymin = observedData$error$ymin,
        ymax = observedData$error$ymax,
        caption = observedDataCollection$CurveOptions[[1]]$CurveOptions$Caption,
        color = observedDataCollection$CurveOptions[[1]]$CurveOptions$Color,
        size = observedDataCollection$CurveOptions[[1]]$CurveOptions$Size,
        plotObject = plotObject
      )
    }
    plotObject <- tlf::addScatter(
      x = observedData$time,
      y = observedData$y,
      caption = observedDataCollection$CurveOptions[[1]]$CurveOptions$Caption,
      color = observedDataCollection$CurveOptions[[1]]$CurveOptions$Color,
      linetype = tlfLinetype(observedDataCollection$CurveOptions[[1]]$CurveOptions$LineStyle),
      size = observedDataCollection$CurveOptions[[1]]$CurveOptions$Size,
      shape = tlfShape(observedDataCollection$CurveOptions[[1]]$CurveOptions$Symbol),
      plotObject = plotObject
    )
  }
  return(plotObject)
}

#' @title plotStatisticsFromPlan
#' @description Add summary statistics to a time profile plot from configuration plan
#' @param time Time values on which output values are aggregated
#' @param outputValues Output values to be aggregated
#' @param statisticId Statistic Id as defined in `ConfigurationPlan` used for data aggregation
#' @param outputName Display name of output
#' @param color Color of the line or ribbon
#' @param linetype Linetype of the line
#' @param plotObject A `ggplot` object with previous statistics displayed
#' @return A `ggplot` object updated with new displayed statistic
#' @import tlf
plotStatisticsFromPlan <- function(time, outputValues, statisticId, outputName, color, linetype, plotObject) {
  # Format the data for plots
  aggregatedData <- getAggregateFromStat(statisticId, time, outputValues)
  caption <- getCaptionFromStat(statisticId, outputName)
  # Range plots use addRibbon
  if (grepl(pattern = "Range", statisticId)) {
    plotObject <- tlf::addRibbon(
      x = aggregatedData$x,
      ymin = aggregatedData$ymin,
      ymax = aggregatedData$ymax,
      caption = caption,
      fill = color,
      plotObject = plotObject
    )
    return(plotObject)
  }
  # Deviation provides 2 lines
  if (grepl(pattern = "Deviation", statisticId)) {
    plotObject <- tlf::addLine(
      x = aggregatedData$x,
      y = aggregatedData$ymin,
      caption = caption,
      color = color,
      linetype = linetype,
      plotObject = plotObject
    )
    plotObject <- tlf::addLine(
      x = aggregatedData$x,
      y = aggregatedData$ymax,
      caption = caption,
      color = color,
      linetype = linetype,
      plotObject = plotObject
    )
    return(plotObject)
  }
  # Remaining cases
  plotObject <- tlf::addLine(
    x = aggregatedData$x,
    y = aggregatedData$y,
    caption = caption,
    color = color,
    linetype = linetype,
    plotObject = plotObject
  )
  return(plotObject)
}

#' @title getAggregateFromStat
#' @description Get summary statistics of output values
#' @param statisticId Statistic Id as defined in `ConfigurationPlan` used for data aggregation
#' @param time Time values on which output values are aggregated
#' @param outputValues Output values to be aggregated
#' @return A data.frame of aggregated data to display
getAggregateFromStat <- function(statisticId, time, outputValues) {
  # Range plots use data.frame with x, ymin and ymax
  if (grepl(pattern = "Range", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "Range_", "", statisticId))
    percentileMinValue <- (100 - percentileValue) / 2
    percentileMaxValue <- (100 + percentileValue) / 2

    aggregatedMinData <- aggregate(
      x = outputValues,
      by = list(time = time),
      FUN = function(x) {
        as.numeric(stats::quantile(x, probs = percentileMinValue / 100))
      }
    )
    aggregatedMaxData <- aggregate(
      x = outputValues,
      by = list(time = time),
      FUN = function(x) {
        as.numeric(stats::quantile(x, probs = percentileMaxValue / 100))
      }
    )
    aggregatedData <- data.frame(
      x = aggregatedMinData$time,
      ymin = aggregatedMinData$x,
      ymax = aggregatedMaxData$x
    )
    return(aggregatedData)
  }

  # Deviation use data.frame with x, ymin and ymax
  # Possibility to plot them as range plot
  if (grepl(pattern = "Deviation", statisticId)) {
    aggregatedMinData <- aggregate(
      x = outputValues,
      by = list(time = time),
      FUN = switch(
        statisticId,
        "ArithmeticStandardDeviation" = function(x) {
          mean(x) + stats::sd(x)
        },
        "GeometricStandardDeviation" = function(x) {
          exp(mean(log(x)) + stats::sd(log(x)))
        }
      )
    )
    aggregatedMaxData <- aggregate(
      x = outputValues,
      by = list(time = time),
      FUN = switch(
        statisticId,
        "ArithmeticStandardDeviation" = function(x) {
          mean(x) - stats::sd(x)
        },
        "GeometricStandardDeviation" = function(x) {
          exp(mean(log(x)) - stats::sd(log(x)))
        }
      )
    )
    aggregatedData <- data.frame(
      x = aggregatedMinData$time,
      ymin = aggregatedMinData$x,
      ymax = aggregatedMaxData$x
    )
    return(aggregatedData)
  }
  # Line plots use data.frame with x and y
  if (grepl(pattern = "Percentile", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "Percentile_", "", statisticId))
    aggregatedData <- aggregate(
      x = outputValues,
      by = list(time = time),
      FUN = function(x) {
        as.numeric(stats::quantile(x, probs = percentileValue / 100))
      }
    )
    names(aggregatedData) <- c("x", "y")
    return(aggregatedData)
  }
  aggregatedData <- aggregate(
    x = outputValues,
    by = list(time = time),
    FUN = switch(
      statisticId,
      "ArithmeticMean" = mean,
      "GeometricMean" = function(x) {
        exp(mean(log(x)))
      },
      "Median" = median,
      "Min" = min,
      "Max" = max
    )
  )
  # When aggregate is used on numeric input,
  # the aggregated data.frame has name "x" which can be confusing
  names(aggregatedData) <- c("x", "y")
  return(aggregatedData)
}

#' @title getCaptionFromStat
#' @description Get display legend caption for time profile plot
#' @param statisticId Statistic Id as defined in `ConfigurationPlan` used for data aggregation
#' @param outputName Display name of output values
#' @return A character value used as caption for the time profile plot
getCaptionFromStat <- function(statisticId, outputName) {
  if (grepl(pattern = "Percentile", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "Percentile_", "", statisticId))
    return(paste0(outputName, "-Percentile ", percentileValue, "%"))
  }
  if (grepl(pattern = "Range", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "Range_", "", statisticId))
    percentileMinValue <- (100 - percentileValue) / 2
    percentileMaxValue <- (100 + percentileValue) / 2
    return(return(paste0(outputName, "-Range ", percentileMinValue, " to ", percentileMaxValue, "%")))
  }
  return(paste(outputName,
    switch(
      statisticId,
      "ArithmeticMean" = "Arithmetic Mean",
      "ArithmeticStandardDeviation" = "Arithmetic Standard Deviation",
      "GeometricMean" = "Geometric Mean",
      "GeometricStandardDeviation" = "Geometric Standard Deviation",
      "Median" = "Median",
      "Min" = "Min",
      "Max" = "Max"
    ),
    sep = "-"
  ))
}

#' @title getTimeProfileObservedDataFromResults
#' @description Get the time profile observed data in appropriate unit from the results from the observed data file
#' @param observedResults List that includes
#' \itemize{
#' \item `data`: a data.frame with column 1 = Time, column 2 = Concentration, column 3 = Error
#' \item `metaData`: a list for each column of data that includes their unit
#' }
#' @param molWeight Molecular weight of compound
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param logFolder folder where the logs are saved
#' @return List with `time`, `y` and `error` values
getTimeProfileObservedDataFromResults <- function(observedResults, molWeight, axesProperties, logFolder) {
  time <- ospsuite::toUnit(
    quantityOrDimension = "Time",
    values = as.numeric(observedResults$data[, 1]),
    targetUnit = axesProperties$x$unit,
    sourceUnit = observedResults$metaData$time$unit
  )
  # Convert output values, if molWeight is NA but not required, then toUnit works without any issue
  # if molWeight is NA and required, then toUnit crashes, error is caught
  # and the error message indictes which observed data Id need molWeight
  outputValues <- tryCatch({
    ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(observedResults$metaData$output$unit),
      values = observedResults$data[, 2],
      targetUnit = axesProperties$y$unit,
      sourceUnit = observedResults$metaData$output$unit,
      molWeight = molWeight
    )
  },
  error = function(e) {
    NULL
  }
  )
  if (isOfLength(outputValues, 0)) {
    logErrorThenStop(
      message = paste0(
        "Molecular weight not found but required for observed data Id '", pathArray[1], "' in Time Profile plot."
      ),
      logFolderPath = logFolder
    )
  }

  outputError <- NULL
  if (!isOfLength(observedResults$metaData$error, 0)) {
    # No unit means that error is geometric
    outputError$ymin <- outputValues / observedResults$data[, 3]
    outputError$ymax <- outputValues * observedResults$data[, 3]

    # In case scale is log,
    if (!isIncluded(observedResults$metaData$error$unit, "")) {
      outputError$ymin <- outputValues - ospsuite::toUnit(
        ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
        observedResults$data[, 3],
        targetUnit = axesProperties$y$unit,
        sourceUnit = observedResults$metaData$error$unit,
        molWeight = molWeight
      )

      outputError$ymax <- outputValues + ospsuite::toUnit(
        ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
        observedResults$data[, 3],
        targetUnit = axesProperties$y$unit,
        sourceUnit = observedResults$metaData$error$unit,
        molWeight = molWeight
      )
      # In case of log scale, ymin<0 are replaced by y so upper branch is still plotted
      if(isIncluded(axesProperties$y$scale, tlf::Scaling$log)){
        outputError$ymin[outputError$ymin<=0] <- outputValues[outputError$ymin<=0]
      }
    }
  }
  return(list(
    time = time,
    y = outputValues,
    error = outputError
  ))
}
