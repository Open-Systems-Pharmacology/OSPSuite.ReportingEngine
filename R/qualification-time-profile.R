#' @title plotQualificationTimeProfiles
#' @description Plot time profile for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param settings `ConfigurationPlan` object
#' @return list with `plots` and `tables`
#' @import tlf
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
plotQualificationTimeProfiles <- function(configurationPlan, settings) {
  timeProfileResults <- list()
  for (timeProfilePlan in configurationPlan$plots$TimeProfile) {
    qualificationCatch(
      {
        # Create a unique ID for the plot name as <Plot index>-<Project>-<Simulation>
        plotID <- defaultFileNames$resultID(
          length(timeProfileResults) + 1, "time_profile_plot",
          timeProfilePlan$Project, timeProfilePlan$Simulation
        )
        # Get simulation and simulation results
        simulationFile <- configurationPlan$getSimulationPath(
          project = timeProfilePlan$Project,
          simulation = timeProfilePlan$Simulation
        )
        simulationResultsFile <- configurationPlan$getSimulationResultsPath(
          project = timeProfilePlan$Project,
          simulation = timeProfilePlan$Simulation
        )

        simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
        simulationResults <- ospsuite::importResultsFromCSV(simulation, simulationResultsFile)

        # Get axes properties (with scale, limits and display units)
        axesProperties <- getAxesProperties(timeProfilePlan$Plot$Axes) %||% settings$axes
        plotConfiguration <- getPlotConfigurationFromPlan(
          timeProfilePlan$Plot, 
          plotType = "TimeProfile"
          )
        timeProfilePlot <- tlf::initializePlot(plotConfiguration)

        # Currently use ifNotNull to select if mean or population time profile
        # So far only population defines "Type" but this might not be always true
        # Function switch could be used instead
        timeProfilePlot <- ifNotNull(
          timeProfilePlan$Plot$Type,
          plotQualificationPopulationTimeProfile(
            simulationAnalysis = timeProfilePlan$Plot$Analysis,
            observedDataCollection = timeProfilePlan$Plot$ObservedDataCollection,
            simulation = simulation,
            simulationResults = simulationResults,
            axesProperties = axesProperties,
            configurationPlan = configurationPlan,
            plotObject = timeProfilePlot
          ),
          plotQualificationMeanTimeProfile(
            configurationPlanCurves = timeProfilePlan$Plot$Curves,
            simulation = simulation,
            simulationResults = simulationResults,
            axesProperties = axesProperties,
            configurationPlan = configurationPlan,
            plotConfiguration = plotConfiguration
          )
        )

        # Save results
        timeProfileResults[[plotID]] <- saveTaskResults(
          id = plotID,
          sectionId = timeProfilePlan$SectionReference %||% timeProfilePlan$SectionId,
          plot = timeProfilePlot,
          plotCaption = timeProfilePlan$Plot$Title %||% timeProfilePlan$Plot$Name
        )
      },
      configurationPlanField = timeProfilePlan
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
#' @param plotConfiguration A `TimeProfilePlotConfiguration` object
#' @return Mean time profile plot as a `ggplot` object
#' @import tlf
#' @keywords internal
plotQualificationMeanTimeProfile <- function(configurationPlanCurves,
                                             simulation,
                                             simulationResults,
                                             axesProperties,
                                             configurationPlan,
                                             plotConfiguration) {
  # Define tlf data mapping for observed and simulated time profile
  simDataMapping <- tlf::TimeProfileDataMapping$new(
    x = "x", y = "y", group = "legend", y2Axis = "y2Axis"
  )
  obsDataMapping <- tlf::ObservedDataMapping$new(
    x = "x", y = "y", ymin = "ymin", ymax = "ymax",
    group = "legend", y2Axis = "y2Axis"
  )

  # Initialize data and metaData
  simData <- data.frame()
  obsData <- data.frame()
  simMetaData <- list(id = NULL, legend = NULL, color = NULL, linetype = NULL, size = NULL)
  obsMetaData <- list(id = NULL, legend = NULL, color = NULL, shape = NULL, size = NULL)
  for (curve in configurationPlanCurves) {
    curveAxesProperties <- axesProperties
    # Update features related to observed data
    if (isObservedData(curve$Y)) {
      curveProperties <- getObservedCurveProperties(
        configurationPlanCurve = curve,
        simulation = simulation,
        axesProperties = curveAxesProperties,
        configurationPlan = configurationPlan
      )
      obsData <- rbind.data.frame(obsData, curveProperties$data)
      # Use default property if property is undefined
      obsMetaData$id <- c(obsMetaData$id, curveProperties$id %||% NA)
      obsMetaData$legend <- c(obsMetaData$legend, curveProperties$legend)
      obsMetaData$color <- c(obsMetaData$color, curveProperties$color %||% "black")
      obsMetaData$size <- c(obsMetaData$size, curveProperties$size %||% 2)
      obsMetaData$shape <- c(obsMetaData$shape, curveProperties$shape %||% tlf::Shapes$circle)
      next
    }
    # Update features related to simulated data
    curveProperties <- getSimulatedCurveProperties(
      configurationPlanCurve = curve,
      simulation = simulation,
      simulationResults = simulationResults,
      axesProperties = curveAxesProperties,
      configurationPlan = configurationPlan
    )
    simData <- rbind.data.frame(simData, curveProperties$data)
    # Use default property if property is undefined
    simMetaData$id <- c(simMetaData$id, curveProperties$id %||% NA)
    simMetaData$legend <- c(simMetaData$legend, curveProperties$legend)
    simMetaData$color <- c(simMetaData$color, curveProperties$color %||% "black")
    simMetaData$size <- c(simMetaData$size, curveProperties$size %||% 1)
    simMetaData$linetype <- c(simMetaData$linetype, curveProperties$linetype %||% tlf::Linetypes$solid)
  }
  # Update legend captions based on expected plot width
  # keep order as provided in legend index (collected as id in metaData)
  if (!isEmpty(simData)) {
    simData$legend <- factor(
      prettyCaption(simData$legend, tlf::initializePlot(plotConfiguration)),
      levels = prettyCaption(
        simMetaData$legend[order(simMetaData$id)], 
        tlf::initializePlot(plotConfiguration))
    ) 
  }
  if (!isEmpty(obsData)) {
    obsData$legend <- factor(
      prettyCaption(obsData$legend, tlf::initializePlot(plotConfiguration)),
      levels = prettyCaption(
        obsMetaData$legend[order(obsMetaData$id)], 
        tlf::initializePlot(plotConfiguration))
    ) 
  }
  # If ticks and ticklabels are undefined, time profile ticks are based on unit and range
  timeTicks <- getTimeTicksFromUnit(
    unit = axesProperties$x$unit, 
    timeValues = c(simData$x, obsData$x)
  )
  axesProperties$x$ticks <- axesProperties$x$ticks %||% timeTicks$ticks
  axesProperties$x$ticklabels <- axesProperties$x$ticklabels %||% timeTicks$ticklabels

  # Check necessity if dual axis by getting requested axes
  requestedAxes <- sort(unique(c(simData$yAxis, obsData$yAxis)))
  
  # If only one axis requested, the only axis becomes the new y of axesProperties
  if (isOfLength(requestedAxes, 1)) {
    axesProperties$y <- axesProperties[[tolower(requestedAxes)]]
    simData$y2Axis <- ifNotNull(simData$yAxis, FALSE)
    obsData$y2Axis <- ifNotNull(obsData$yAxis, FALSE)
    plotConfiguration <- updateQualificationTimeProfilePlotConfiguration(
      simulatedMetaData = simMetaData,
      observedMetaData = obsMetaData,
      requestedAxes = requestedAxes,
      axesProperties = axesProperties,
      plotConfiguration = plotConfiguration
    )
    
    plotObject <- tlf::plotTimeProfile(
      data = simData,
      observedData = obsData,
      dataMapping = simDataMapping,
      observedDataMapping = obsDataMapping,
      plotConfiguration = plotConfiguration
    )
    return(plotObject)
  }
  # If all Y, Y2 and Y3 axes are defined and used
  # Keep Y and Y2 with a warning message
  if (isOfLength(requestedAxes, 3)) {
    logError(messages$warningTooManyAxes())
    requestedAxes <- c("Y", "Y2")
  }
  # Use dual axis but needs to map them first
  # New Y axis will be either Y or Y2 (ie. axis found first because they were sorted)
  # New Y2 axis will be either Y2 or Y3 (ie. axis found last because they were sorted)
  axesProperties$y <- axesProperties[[head(tolower(requestedAxes), 1)]]
  axesProperties$y2 <- axesProperties[[tail(tolower(requestedAxes), 1)]]

  # y2Axis is the variable mapped to the dual axis and expected as logical by tlf package
  # FALSE y2Axis values correspond to data on the left axis
  # TRUE y2Axis values correspond to data on the right axis
  # Since yAxis values are included in "Y", "Y2" or "Y3"
  # TRUE values will correspond to the values included (%in%) last found axis
  simData$y2Axis <- ifNotNull(simData$yAxis, simData$yAxis %in% tail(requestedAxes, 1))
  obsData$y2Axis <- ifNotNull(obsData$yAxis, obsData$yAxis %in% tail(requestedAxes, 1))

  plotConfiguration <- updateQualificationTimeProfilePlotConfiguration(
    simulatedMetaData = simMetaData,
    observedMetaData = obsMetaData,
    requestedAxes = requestedAxes,
    axesProperties = axesProperties,
    plotConfiguration = plotConfiguration
  )

  plotObject <- tlf::plotTimeProfile(
    data = simData,
    observedData = obsData,
    dataMapping = simDataMapping,
    observedDataMapping = obsDataMapping,
    plotConfiguration = plotConfiguration
  )


  # We might need in that case to create a dummy plot
  # to update the exported size of the plot
  simData$y2Axis <- ifNotNull(simData$yAxis, FALSE)
  obsData$y2Axis <- ifNotNull(obsData$yAxis, FALSE)
  dummyPlotObject <- tlf::plotTimeProfile(
    data = simData,
    observedData = obsData,
    dataMapping = simDataMapping,
    observedDataMapping = obsDataMapping,
    plotConfiguration = plotConfiguration
  )
  dummyPlotObject <- updatePlotDimensions(dummyPlotObject)
  plotObject$plotConfiguration$export <- dummyPlotObject$plotConfiguration$export

  return(plotObject)
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
#' @return Population time profile plot as a `ggplot` object
#' @import tlf
#' @importFrom ospsuite.utils %||%
#' @keywords internal
plotQualificationPopulationTimeProfile <- function(simulationAnalysis, observedDataCollection, simulation, simulationResults, axesProperties, configurationPlan, plotObject) {

  # Get simulation results from configuration plan field "Fields"
  outputPath <- simulationAnalysis$Fields[[1]]$QuantityPath
  simulationQuantity <- ospsuite::getQuantity(outputPath, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = outputPath)
  molWeight <- simulation$molWeightFor(outputPath)
  outputDimension <- simulationPathResults$metaData[[outputPath]]$dimension
  
  # Overwrite dimension, unit and scale if found in Analysis field
  # Keep compatibility with Config Plan from Matlab version
  axesProperties$y$dimension <- simulationAnalysis$Fields[[1]]$Dimension %||% axesProperties$y$dimension
  # If unit is left undefined, use base unit
  axesProperties$y$unit <- simulationAnalysis$Fields[[1]]$Unit %||% ospsuite::getBaseUnit(outputDimension)
  axesProperties$y$scale <- tlfScale(simulationAnalysis$Fields[[1]]$Scaling %||% axesProperties$y$scale)

  # Get and convert output path values into display unit
  time <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesProperties$x$unit
  )
  axesProperties$x <- c(
    axesProperties$x,
    getTimeTicksFromUnit(axesProperties$x$unit, time)
  )
  outputValues <- ospsuite::toUnit(
    simulationQuantity,
    simulationPathResults$data[, outputPath],
    axesProperties$y$unit,
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
    observedResults <- getObservedDataFromConfigurationPlan(observedDataId, configurationPlan)
    # Currently, the molecular weight is directly taken from the simulation output
    observedData <- getTimeProfileObservedDataFromResults(observedResults, molWeight, axesProperties)

    if (!isEmpty(observedData$error)) {
      plotObject <- tlf::addErrorbar(
        x = observedData$time,
        ymin = observedData$error$ymin,
        ymax = observedData$error$ymax,
        caption = prettyCaption(observedDataCollection$CurveOptions[[1]]$Caption %||% "Observed data", plotObject),
        color = observedDataCollection$CurveOptions[[1]]$CurveOptions$Color,
        size = observedDataCollection$CurveOptions[[1]]$CurveOptions$Size,
        plotObject = plotObject
      )
    }
    plotObject <- tlf::addScatter(
      x = observedData$time,
      y = observedData$y,
      caption = prettyCaption(observedDataCollection$CurveOptions[[1]]$Caption %||% "Observed data", plotObject),
      color = observedDataCollection$CurveOptions[[1]]$CurveOptions$Color,
      linetype = tlfLinetype(observedDataCollection$CurveOptions[[1]]$CurveOptions$LineStyle),
      size = observedDataCollection$CurveOptions[[1]]$CurveOptions$Size,
      shape = tlfShape(observedDataCollection$CurveOptions[[1]]$CurveOptions$Symbol),
      plotObject = plotObject
    )
  }
  # Set axes based on Axes properties
  plotObject <- updatePlotAxes(plotObject, axesProperties)
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
#' @keywords internal
plotStatisticsFromPlan <- function(time, outputValues, statisticId, outputName, color, linetype, plotObject) {
  # Format the data for plots
  aggregatedData <- getAggregateFromStat(statisticId, time, outputValues)
  caption <- prettyCaption(getCaptionFromStat(statisticId, outputName), plotObject)
  # Range and Deviation plots use addRibbon
  if (grepl(pattern = "Range", statisticId) | grepl(pattern = "Deviation", statisticId)) {
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
#' @keywords internal
getAggregateFromStat <- function(statisticId, time, outputValues) {
  # Range plots use data.frame with x, ymin and ymax
  if (grepl(pattern = "Range", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "[Range_]", "", statisticId))
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

  # Deviation will lead to range plot using data.frame with x, ymin and ymax
  if (grepl(pattern = "Deviation", statisticId)) {
    aggregatedMinData <- aggregate(
      x = outputValues,
      by = list(time = time),
      # Plot will show mean +/- 1*SD is plotted,
      # This can be changed to plot +/- 1.96*SD representing a 95% CI
      FUN = switch(statisticId,
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
      FUN = switch(statisticId,
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
    FUN = switch(statisticId,
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
#' @keywords internal
getCaptionFromStat <- function(statisticId, outputName) {
  if (grepl(pattern = "Percentile", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "Percentile_", "", statisticId))
    return(paste0(outputName, "-Percentile ", percentileValue, "%"))
  }
  if (grepl(pattern = "Range", statisticId)) {
    percentileValue <- as.numeric(gsub(pattern = "[Range_]", "", statisticId))
    percentileMinValue <- (100 - percentileValue) / 2
    percentileMaxValue <- (100 + percentileValue) / 2
    return(return(paste0(outputName, "-Range ", percentileMinValue, " to ", percentileMaxValue, "%")))
  }
  return(paste(outputName,
    switch(statisticId,
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
#' @return List with `time`, `y` and `error` values
#' @keywords internal
getTimeProfileObservedDataFromResults <- function(observedResults, molWeight, axesProperties) {
  time <- ospsuite::toUnit(
    quantityOrDimension = "Time",
    values = as.numeric(observedResults$data[, 1]),
    targetUnit = axesProperties$x$unit,
    sourceUnit = observedResults$metaData$time$unit
  )
  # Convert output values, if molWeight is NA but not required, then toUnit works without any issue
  # if molWeight is NA and required, then toUnit crashes, error is caught
  # and the error message indictes which observed data Id need molWeight
  outputValues <- tryCatch(
    {
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
  if (isEmpty(outputValues)) {
    stop(messages$errorMolecularWeightRequired("")) #pathArray[1]
  }

  outputError <- NULL
  if (!isEmpty(observedResults$metaData$error)) {
    outputError <- getObservedErrorValues(outputValues, observedResults, axesProperties, molWeight = molWeight)
  }
  return(list(
    time = time,
    y = outputValues,
    error = outputError
  ))
}

#' @title getDefaultTimeProfileAxesSettings
#' @description Get the default axes settings for mean and population time profiles
#' for keeping compatibility with configuration plan from Matlab version
#' @return List of `x` and `y` axes settings
#' @keywords internal
getDefaultTimeProfileAxesSettings <- function() {
  xAxis <- list(
    dimension = ospsuite::ospDimensions$Time, unit = ospsuite::ospUnits$Time$h,
    min = NULL, max = NULL, scale = tlf::Scaling$lin,
    grid = list(color = reEnv$theme$background$xGrid$color, linetype = reEnv$theme$background$xGrid$linetype)
  )
  yAxis <- list(
    dimension = ospsuite::ospDimensions$`Concentration (mass)`, unit = ospsuite::ospUnits$`Concentration [mass]`$`µg/l`,
    min = NULL, max = NULL, scale = tlf::Scaling$log,
    grid = list(color = reEnv$theme$background$yGrid$color, linetype = reEnv$theme$background$yGrid$linetype)
  )
  return(list(x = xAxis, y = yAxis))
}

#' @title getObservedErrorValues
#' @description
#' Get the observed data error range to display on time profile plots
#' @param observedValues Numeric values of observed data
#' @param observedResults A named list, including `data` and `metaData`, of observed results.
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param molWeight Molecular weight if unit conversion is required
#' @return A named list, with `ymin` and `ymax`, of the observed data error range
#' @keywords internal
getObservedErrorValues <- function(observedValues, observedResults, axesProperties, molWeight = NA) {
  # Compute geometric error by default
  observedError <- calculateGeometricErrorRange(observedValues, observedResults$data[, 3])

  # If error has a unit, compute arithmetic error instead
  if (!isIncluded(observedResults$metaData$error$unit, "")) {
    # First convert error to the appropriate unit
    errorValues <- ospsuite::toUnit(
      ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
      observedResults$data[, 3],
      targetUnit = axesProperties$y$unit,
      sourceUnit = observedResults$metaData$error$unit,
      molWeight = molWeight
    )
    observedError <- calculateArithmeticErrorRange(observedValues, errorValues)
  }

  # If error has no unit but values lower than 1,
  # Check output has also no unit and then compute arithmetic error
  if (isIncluded(observedResults$metaData$error$unit, "") & any(observedResults$data[, 3] < 1, na.rm = TRUE)) {
    tryCatch(
      {
        validateIsIncluded(observedResults$metaData$output$unit, "")
      },
      error = function(e) {
        logError(messages$warningErrorAssumedArithmetic())
      }
    )
    observedError <- calculateArithmeticErrorRange(observedValues, observedResults$data[, 3])
  }

  # Caution: errors input as NA values leads to ymin and ymax being also NA values
  # NA values are not well handled by ggplot2 which tends to crash
  # Thus, NAs need to be replaced by observedValues (no error bar) which is virtually the same
  observedError$ymin[is.na(observedError$ymin)] <- observedValues[is.na(observedError$ymin)]
  observedError$ymax[is.na(observedError$ymax)] <- observedValues[is.na(observedError$ymax)]

  # For log scale plots, ymin<0 are replaced by observedValues so upper branch is still plotted
  if (isIncluded(axesProperties$y$scale, tlf::Scaling$log)) {
    observedError$ymin[observedError$ymin <= 0] <- observedValues[observedError$ymin <= 0]
  }
  return(observedError)
}


#' @title getObservedCurveProperties
#' @description Get Curve Properties and values of Observed data
#' @param configurationPlanCurve `Curves` fields of configuration plan
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @return A named list data and meta data parameters
#' @keywords internal
getObservedCurveProperties <- function(configurationPlanCurve,
                                       simulation,
                                       axesProperties,
                                       configurationPlan) {
  # Update Axis Properties based on yAxisType
  yAxisType <- configurationPlanCurve$CurveOptions$yAxisType %||% "Y"
  axesProperties$y <- switch(yAxisType,
    "Y2" = axesProperties$y2,
    "Y3" = axesProperties$y3,
    axesProperties$y
  )

  observedDataId <- getObservedDataIdFromPath(configurationPlanCurve$Y)
  molWeight <- configurationPlan$getMolWeightForObservedData(observedDataId)
  if (is.na(molWeight)) {
    compoundName <- getCompoundNameFromPath(configurationPlanCurve$Y)
    molWeight <- getMolWeightForCompound(compoundName, simulation)
  }
  # observedResults is a list that includes
  # data: a data.frame with column 1 = Time, column 2 = Concentration, column 3 = Error
  # metaData: a list for each column of data that includes their unit
  observedResults <- getObservedDataFromConfigurationPlan(observedDataId, configurationPlan)
  observedData <- getTimeProfileObservedDataFromResults(observedResults, molWeight, axesProperties)

  outputData <- data.frame(
    x = observedData$time,
    y = observedData$y,
    ymin = observedData$error$ymin %||% observedData$y,
    ymax = observedData$error$ymax %||% observedData$y,
    legend = configurationPlanCurve$Name,
    yAxis = yAxisType
  )

  outputCurve <- list(
    data = outputData,
    color = configurationPlanCurve$CurveOptions$Color,
    shape = tlfShape(configurationPlanCurve$CurveOptions$Symbol),
    size = configurationPlanCurve$CurveOptions$Size,
    id = configurationPlanCurve$CurveOptions$LegendIndex,
    legend = configurationPlanCurve$Name
  )

  return(outputCurve)
}

#' @title getSimulatedCurveProperties
#' @description Get Curve Properties and values of Simulated data
#' @param configurationPlanCurve `Curves` fields of configuration plan
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param simulationResults A `SimulationResults` object from `ospsuite` package
#' that includes the data requested from `configurationPlanCurve` properties
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @return A named list data and meta data parameters
#' @keywords internal
getSimulatedCurveProperties <- function(configurationPlanCurve,
                                        simulation,
                                        simulationResults,
                                        axesProperties,
                                        configurationPlan) {
  # Update Axis Properties based on yAxisType
  yAxisType <- configurationPlanCurve$CurveOptions$yAxisType %||% "Y"
  axesProperties$y <- switch(yAxisType,
    "Y2" = axesProperties$y2,
    "Y3" = axesProperties$y3,
    axesProperties$y
  )
  # configurationPlanCurve$Y is a quantity path from the cnofiguration plan
  # e.g. "S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)"
  # or "Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc"
  pathArray <- ospsuite::toPathArray(configurationPlanCurve$Y)
  # Remove simulation name from path
  outputPath <- ospsuite::toPathString(pathArray[-1])
  # Get and convert output path values into display unit
  simulationQuantity <- ospsuite::getQuantity(outputPath, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = outputPath)
  molWeight <- simulation$molWeightFor(outputPath)

  outputData <- data.frame(
    x = ospsuite::toUnit(
      "Time",
      simulationPathResults$data[, "Time"],
      axesProperties$x$unit
    ),
    y = ospsuite::toUnit(
      simulationQuantity,
      simulationPathResults$data[, outputPath],
      axesProperties$y$unit,
      molWeight = molWeight
    ),
    legend = configurationPlanCurve$Name,
    yAxis = yAxisType
  )

  outputCurve <- list(
    data = outputData,
    color = configurationPlanCurve$CurveOptions$Color,
    linetype = tlfLinetype(configurationPlanCurve$CurveOptions$LineStyle),
    size = configurationPlanCurve$CurveOptions$Size,
    id = configurationPlanCurve$CurveOptions$LegendIndex,
    legend = configurationPlanCurve$Name
  )

  return(outputCurve)
}

#' @title updateQualificationTimeProfilePlotConfiguration
#' @description Update TimeProfilePlotConfiguration properties based on meta data and requested axes
#' @param simulatedMetaData List of meta data on simulated data obtained from `getSimulatedCurveProperties`
#' @param observedMetaData List of meta data on observed data obtained from `getObservedCurveProperties`
#' @param requestedAxes Array of requested axes included in `"Y"`, `"Y2"` and `"Y3"`
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param plotConfiguration A `PlotConfiguration` object
#' @return Updated `TimeProfilePlotConfiguration` object
#' @keywords internal
updateQualificationTimeProfilePlotConfiguration <- function(simulatedMetaData = NULL,
                                                            observedMetaData  = NULL,
                                                            requestedAxes = "Y",
                                                            axesProperties = NULL,
                                                            plotConfiguration) {
  # Update plot configuration for simulated values
  if (!isEmpty(simulatedMetaData)) {
    sortedSimulatedMetaData <- order(simulatedMetaData$id)
    plotConfiguration$lines$color <- simulatedMetaData$color[sortedSimulatedMetaData]
    plotConfiguration$lines$linetype <- simulatedMetaData$linetype[sortedSimulatedMetaData]
    plotConfiguration$lines$size <- simulatedMetaData$size[sortedSimulatedMetaData]
  }
  # Update plot configuration for observed values
  if (!isEmpty(observedMetaData)) {
    sortedObservedMetaData <- order(observedMetaData$id)
    # offset of simulated data colors
    plotConfiguration$points$color <- c(
      simulatedMetaData$color, 
      observedMetaData$color[sortedObservedMetaData]
    )
    plotConfiguration$errorbars$color <- c(
      simulatedMetaData$color, 
      observedMetaData$color[sortedObservedMetaData]
    )
    plotConfiguration$points$shape <- observedMetaData$shape[sortedObservedMetaData]
    plotConfiguration$points$size <- observedMetaData$size[sortedObservedMetaData]
  }
  
  # Update labels, axes and background properties
  # These cannot be performed at the plotObject level any more because of dual axis
  plotConfiguration$labels$xlabel <- tlf::getLabelWithUnit(
    displayDimension(axesProperties$x$dimension), 
    axesProperties$x$unit
    )
  plotConfiguration$labels$ylabel <- tlf::getLabelWithUnit(
    displayDimension(axesProperties$y$dimension), 
    axesProperties$y$unit
    )
  
  plotConfiguration$background$xGrid$color <- axesProperties$x$grid$color
  plotConfiguration$background$xGrid$linetype <- axesProperties$x$grid$linetype
  plotConfiguration$background$yGrid$color <- axesProperties$y$grid$color
  plotConfiguration$background$yGrid$linetype <- axesProperties$y$grid$linetype
  
  plotConfiguration$xAxis$scale <- axesProperties$x$scale
  plotConfiguration$xAxis$limits <- c(axesProperties$x$min, axesProperties$x$max)
  plotConfiguration$xAxis$ticks <- axesProperties$x$ticks
  plotConfiguration$xAxis$ticklabels <- axesProperties$x$ticklabels
  
  plotConfiguration$yAxis$scale <- axesProperties$y$scale
  plotConfiguration$yAxis$limits <- c(axesProperties$y$min, axesProperties$y$max)
  plotConfiguration$yAxis$ticks <- axesProperties$y$ticks
  plotConfiguration$yAxis$ticklabels <- axesProperties$y$ticklabels
  
  if (isOfLength(requestedAxes, 1)) {
    return(plotConfiguration)
  }
  
  plotConfiguration$labels$y2label <- tlf::getLabelWithUnit(
    displayDimension(axesProperties$y2$dimension), 
    axesProperties$y2$unit
    )
  plotConfiguration$background$y2Grid$color <- axesProperties$y2$grid$color
  plotConfiguration$background$y2Grid$linetype <- axesProperties$y2$grid$linetype

  plotConfiguration$y2Axis$scale <- axesProperties$y2$scale
  plotConfiguration$y2Axis$limits <- c(axesProperties$y2$min, axesProperties$y2$max)
  plotConfiguration$y2Axis$ticks <- axesProperties$y2$ticks
  plotConfiguration$y2Axis$ticklabels <- axesProperties$y2$ticklabels

  return(plotConfiguration)
}
