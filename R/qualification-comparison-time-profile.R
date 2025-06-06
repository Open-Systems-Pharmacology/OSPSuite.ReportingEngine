#' @title plotQualificationComparisonTimeProfile
#' @description Plot comparison time profile for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param settings `ConfigurationPlan` object
#' @return list with `plots` and `tables`
#' @import tlf
#' @import ospsuite
#' @import ospsuite.utils
#' @keywords internal
plotQualificationComparisonTimeProfile <- function(configurationPlan, settings) {
  timeProfileResults <- list()
  for (timeProfilePlan in configurationPlan$plots$ComparisonTimeProfilePlots) {
    qualificationCatch(
      {
        # Create a unique ID for the plot name as <Plot index>-<Project>-<Simulation>
        plotID <- defaultFileNames$resultID("comparison_time_profile", timeProfilePlan$Title, length(timeProfileResults) + 1)

        # Get axes properties (with scale, limits and display units)
        axesProperties <- getAxesProperties(timeProfilePlan$Axes) %||% settings$axes
        if (isEmpty(axesProperties)) {
          logError(messages$warningNoAxesSettings(
            timeProfilePlan$Title,
            plotType = "Comparison Time Profile Plots"
          ))
          next
        }

        simulationDuration <- ospsuite::toUnit(
          quantityOrDimension = "Time",
          values = as.numeric(timeProfilePlan$SimulationDuration),
          targetUnit = axesProperties$x$unit,
          sourceUnit = timeProfilePlan$TimeUnit
        )
        axesProperties$x <- c(
          axesProperties$x,
          getTimeTicksFromUnit(axesProperties$x$unit, simulationDuration)
        )

        plotConfiguration <- getPlotConfigurationFromPlan(timeProfilePlan[["PlotSettings"]])
        timeProfilePlot <- tlf::initializePlot(plotConfiguration)
        for (outputMapping in timeProfilePlan$OutputMappings) {
          timeProfilePlot <- addOutputToComparisonTimeProfile(
            outputMapping,
            simulationDuration,
            axesProperties,
            timeProfilePlot,
            configurationPlan
          )
        }
        # Set axes based on Axes properties
        timeProfilePlot <- updatePlotAxes(timeProfilePlot, axesProperties)
        # Save results
        timeProfileResults[[plotID]] <- saveTaskResults(
          id = plotID,
          sectionId = timeProfilePlan$SectionReference %||% timeProfilePlan$SectionId,
          plot = timeProfilePlot,
          plotCaption = timeProfilePlan$Title
        )
      },
      configurationPlanField = timeProfilePlan
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
#' @return A ggplot object
#' @import ospsuite.utils
#' @keywords internal
addOutputToComparisonTimeProfile <- function(outputMapping, simulationDuration, axesProperties, plotObject, configurationPlan) {
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

  logDebug(paste0(
    "In Comparison Time Profile Plots, Project '",
    outputMapping$Project, "' - Simulation '", outputMapping$Simulation, "'\n",
    messages$dataIncludedInTimeRange(
      sum(selectedTimeValues),
      timeOffset + c(0, simulationDuration),
      axesProperties$x$unit,
      "simulated"
    )
  ))

  simulatedValues <- ospsuite::toUnit(
    simulationQuantity,
    simulationPathResults$data[, outputMapping$Output],
    axesProperties$y$unit,
    molWeight = molWeight
  )
  simulatedValues <- simulatedValues[selectedTimeValues]
  # If issues with log scale due to all zeros or negative values
  # Warn and do not plot the data
  logScaleIssue <- all(simulatedValues <= 0, isIncluded(axesProperties$y$scale, "log"))
  if (logScaleIssue) {
    warning(messages$warningLogScaleIssue(outputMapping$Output), call. = FALSE)
  }

  if (!logScaleIssue) {
    # Add simulated values to plot
    plotObject <- tlf::addLine(
      x = simulatedTime,
      y = simulatedValues,
      caption = prettyCaption(paste(outputMapping$Caption, "Simulated Data"), plotObject),
      linetype = tlfLinetype(outputMapping$LineStyle),
      color = outputMapping$Color,
      size = outputMapping$Size,
      plotObject = plotObject
    )
  }

  # Loop on each observed dataset in OutputMappings
  for (observedDataSet in outputMapping$ObservedData) {
    # Get data and meta data of observed results
    observedResults <- getObservedDataFromConfigurationPlan(observedDataSet, configurationPlan)
    observedTime <- ospsuite::toUnit(
      quantityOrDimension = "Time",
      values = as.numeric(observedResults$data[, 1]),
      targetUnit = axesProperties$x$unit,
      sourceUnit = observedResults$metaData$time$unit
    )
    observedTime <- observedTime - timeOffset
    selectedObservedTimeValues <- observedTime >= 0 & observedTime <= simulationDuration
    observedTime <- observedTime[selectedObservedTimeValues]
    logDebug(paste0(
      "In Comparison Time Profile Plots, Observed Dataset '", observedDataSet, "'\n",
      messages$dataIncludedInTimeRange(
        sum(selectedObservedTimeValues),
        timeOffset + c(0, simulationDuration),
        axesProperties$x$unit,
        "observed"
      )
    ))

    observedValues <- ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(observedResults$metaData$output$unit),
      values = observedResults$data[, 2],
      targetUnit = axesProperties$y$unit,
      sourceUnit = tolower(observedResults$metaData$output$unit),
      molWeight = molWeight
    )
    observedValues <- observedValues[selectedObservedTimeValues]
    observedResults$data <- observedResults$data[selectedObservedTimeValues, ]

    # Add observed errorbars
    if (!isEmpty(observedResults$metaData$error)) {
      observedError <- getObservedErrorValues(observedValues, observedResults, axesProperties, molWeight = molWeight)

      plotObject <- tlf::addErrorbar(
        x = observedTime,
        ymin = observedError$ymin,
        ymax = observedError$ymax,
        caption = prettyCaption(paste(outputMapping$Caption, "Observed Data"), plotObject),
        color = outputMapping$Color,
        size = outputMapping$Size,
        plotObject = plotObject
      )
    }
    # Add observed points
    plotObject <- tlf::addScatter(
      x = observedTime,
      y = observedValues,
      caption = prettyCaption(paste(outputMapping$Caption, "Observed Data"), plotObject),
      shape = tlfShape(outputMapping$Symbol),
      color = outputMapping$Color,
      size = outputMapping$Size,
      plotObject = plotObject
    )
  }
  return(plotObject)
}
