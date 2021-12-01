#' @title plotQualificationComparisonTimeProfile
#' @description Plot comparison time profile for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @param settings `ConfigurationPlan` object
#' @return list with `plots` and `tables`
#' @import tlf
#' @import ospsuite
#' @keywords internal
plotQualificationComparisonTimeProfile <- function(configurationPlan,
                                                   logFolder = getwd(),
                                                   settings) {
  timeProfileResults <- list()
  for (timeProfilePlan in configurationPlan$plots$ComparisonTimeProfilePlots) {
    # Create a unique ID for the plot name as <Plot index>-<Project>-<Simulation>
    plotID <- paste("comparison-time-profile", timeProfilePlan$Title, length(timeProfileResults) + 1, sep = "-")

    # Get axes properties (with scale, limits and display units)
    axesProperties <- getAxesProperties(timeProfilePlan$Axes) %||% settings$axes
    if (isOfLength(axesProperties, 0)) {
      # TODO Centralize messaging of configurtion plan errors and warnings
      logWorkflow(
        message = paste0(
          "In Comparison Time Profile Plots,\n",
          "No axes settings defined for plot: '", timeProfilePlan$Title, "'"
        ),
        pathFolder = logFolder,
        logTypes = LogTypes$Error
      )
      next
    }

    simulationDuration <- ospsuite::toUnit(
      quantityOrDimension = "Time",
      values = as.numeric(timeProfilePlan$SimulationDuration),
      targetUnit = axesProperties$x$unit,
      sourceUnit = timeProfilePlan$TimeUnit
    )

    plotConfiguration <- getPlotConfigurationFromPlan(timeProfilePlan$Plot)
    timeProfilePlot <- tlf::initializePlot(plotConfiguration)
    for (outputMapping in timeProfilePlan$OutputMappings) {
      timeProfilePlot <- addOutputToComparisonTimeProfile(
        outputMapping,
        simulationDuration,
        axesProperties,
        timeProfilePlot,
        configurationPlan,
        logFolder
      )
    }
    # Set axes based on Axes properties
    timeProfilePlot <- updatePlotAxes(timeProfilePlot, axesProperties)
    # Save results
    timeProfileResults[[plotID]] <- saveTaskResults(
      id = plotID,
      sectionId = timeProfilePlan$SectionId,
      plot = timeProfilePlot,
      plotCaption = timeProfilePlan$Title
    )
  }
  return(timeProfileResults)
}

#' @title addOutputToComparisonTimeProfile
#' @description Add plot layers for an output mapping from comparison time profile plot
#' @param outputMapping list of mapping elements from `OutputMappings` field in configuration plan
#' @param simulationDuration Duration of simulation in X axis unit
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param plotObject ggplot object
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @return A ggplot object
#' @keywords internal
addOutputToComparisonTimeProfile <- function(outputMapping, simulationDuration, axesProperties, plotObject, configurationPlan, logFolder) {
  # Get simulation output
  simulationFile <- configurationPlan$getSimulationPath(
    project = outputMapping$Project,
    simulation = outputMapping$Simulation
  )
  simulationResultsFile <- configurationPlan$getSimulationResultsPath(
    project = outputMapping$Project,
    simulation = outputMapping$Simulation
  )
  simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
  simulationResults <- ospsuite::importResultsFromCSV(simulation, simulationResultsFile)
  # Get and convert output path values into display unit
  simulationQuantity <- ospsuite::getQuantity(outputMapping$Output, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = simulationQuantity)
  molWeight <- simulation$molWeightFor(outputMapping$Output)

  # timeOffset and simulationDuration needs to be in same unit as x Axis
  timeOffset <- ospsuite::toUnit(
    quantityOrDimension = "Time",
    values = as.numeric(outputMapping$StartTime %||% 0),
    targetUnit = axesProperties$x$unit,
    sourceUnit = outputMapping$TimeUnit %||% axesProperties$x$unit
  )
  simulatedTime <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesProperties$x$unit
  )
  simulatedTime <- simulatedTime - timeOffset
  selectedTimeValues <- simulatedTime >= 0 & simulatedTime <= simulationDuration
  simulatedTime <- simulatedTime[selectedTimeValues]

  logWorkflow(
    paste0(
      "In comparison time profile, '", sum(selectedTimeValues), "' values selected between ",
      timeOffset, " and ", timeOffset + simulationDuration, " ", axesProperties$x$unit,
      " in Project '", outputMapping$Project, "' - Simulation '", outputMapping$Simulation, "'"
    ),
    logFolder,
    LogTypes$Debug
  )

  simulatedValues <- ospsuite::toUnit(
    simulationQuantity,
    simulationPathResults$data[, outputMapping$Output],
    axesProperties$y$unit,
    molWeight = molWeight
  )
  simulatedValues <- simulatedValues[selectedTimeValues]

  # Add simulated values to plot
  plotObject <- tlf::addLine(
    x = simulatedTime,
    y = simulatedValues,
    caption = prettyCaption(paste(outputMapping$Caption, "Simulated Data")),
    linetype = tlfLinetype(outputMapping$LineStyle),
    color = outputMapping$Color,
    size = outputMapping$Size,
    plotObject = plotObject
  )

  # Get observed output
  observedResults <- getObservedDataFromConfigurationPlan(outputMapping$ObservedData, configurationPlan, logFolder)
  observedTime <- ospsuite::toUnit(
    quantityOrDimension = "Time",
    values = as.numeric(observedResults$data[, 1]),
    targetUnit = axesProperties$x$unit,
    sourceUnit = observedResults$metaData$time$unit
  )
  observedTime <- observedTime - timeOffset
  selectedObservedTimeValues <- observedTime >= 0 & observedTime <= simulationDuration
  observedTime <- observedTime[selectedObservedTimeValues]

  logWorkflow(
    paste0(
      "In comparison time profile, '", sum(selectedObservedTimeValues), "' values selected between ",
      timeOffset, " and ", timeOffset + simulationDuration, " ", axesProperties$x$unit,
      " in Observed Dataset '", outputMapping$ObservedData, "'"
    ),
    logFolder,
    LogTypes$Debug
  )

  observedValues <- ospsuite::toUnit(
    quantityOrDimension = ospsuite::getDimensionForUnit(observedResults$metaData$output$unit),
    values = observedResults$data[, 2],
    targetUnit = axesProperties$y$unit,
    sourceUnit = tolower(observedResults$metaData$output$unit),
    molWeight = molWeight
  )
  observedValues <- observedValues[selectedObservedTimeValues]
  observedError <- NULL
  if (!isOfLength(observedResults$metaData$error, 0)) {
    # No unit means that error is geometric
    observedError$ymin <- observedValues / observedResults$data[, 3]
    observedError$ymax <- observedValues * observedResults$data[, 3]

    if (!isIncluded(observedResults$metaData$error$unit, "")) {
      observedError$ymin <- observedValues - ospsuite::toUnit(
        ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
        observedResults$data[, 3],
        targetUnit = axesProperties$y$unit,
        sourceUnit = observedResults$metaData$error$unit,
        molWeight = molWeight
      )

      observedError$ymax <- observedValues + ospsuite::toUnit(
        ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
        observedResults$data[, 3],
        targetUnit = axesProperties$y$unit,
        sourceUnit = observedResults$metaData$error$unit,
        molWeight = molWeight
      )
    }
    # Caution: error NA values cause ymin and ymax NA values which breaks the plot,
    # they need to be replaced by y (no error bar)
    observedError$ymin[is.na(observedError$ymin)] <- observedValues[is.na(observedError$ymin)]
    observedError$ymax[is.na(observedError$ymax)] <- observedValues[is.na(observedError$ymax)]
    # In case of log scale, ymin<0 are replaced by y so upper branch is still plotted
    if(isIncluded(axesProperties$y$scale, tlf::Scaling$log)){
      observedError$ymin[observedError$ymin<=0] <- observedValues[observedError$ymin<=0]
    }
    
    # Add error bars for observed data
    plotObject <- tlf::addErrorbar(
      x = observedTime,
      ymin = observedError$ymin[selectedObservedTimeValues],
      ymax = observedError$ymax[selectedObservedTimeValues],
      caption = prettyCaption(paste(outputMapping$Caption, "Observed Data")),
      color = outputMapping$Color,
      size = outputMapping$Size,
      plotObject = plotObject
    )
  }

  # Add observed values to plot
  plotObject <- tlf::addScatter(
    x = observedTime,
    y = observedValues,
    caption = prettyCaption(paste(outputMapping$Caption, "Observed Data")),
    shape = tlfShape(outputMapping$Symbol),
    color = outputMapping$Color,
    size = outputMapping$Size,
    plotObject = plotObject
  )

  return(plotObject)
}
