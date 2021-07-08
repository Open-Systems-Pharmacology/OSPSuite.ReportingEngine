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

    simulation <- loadSimulation(simulationFile)
    simulationResults <- importResultsFromCSV(simulation, simulationResultsFile)

    # Get mol weight for observed data as first molecule path
    moleculePath <- utils::head(ospsuite::getAllMoleculePathsIn(simulation), 1)
    molWeightForObservedData <- simulation$molWeightFor(moleculePath)

    # Get axes properties (with scale, limits and display units)
    axesProperties <- getAxesPropertiesForTimeProfiles(timeProfilePlan$Plot$Axes)

    timeProfilePlot <- tlf::initializePlot()
    for (curve in timeProfilePlan$Plot$Curves) {
      # TODO handle Observed data and Y2 axis
      curveOutput <- getCurvePropertiesForTimeProfiles(curve, simulation, simulationResults, molWeightForObservedData, axesProperties, configurationPlan, logFolder)
      # Update mol weight obtained from latest path in case next curve is observed data
      molWeightForObservedData <- curveOutput$molWeight
      if (is.null(curveOutput)) {
        next
      }
      if (!isOfLength(curveOutput$uncertainty, 0)) {
        timeProfilePlot <- addErrorbar(
          x = curveOutput$x,
          ymin = curveOutput$uncertainty$ymin,
          ymax = curveOutput$uncertainty$ymax,
          caption = curveOutput$caption,
          color = curveOutput$color,
          size = curveOutput$size,
          plotObject = timeProfilePlot
        )
      }
      timeProfilePlot <- addLine(
        x = curveOutput$x,
        y = curveOutput$y,
        caption = curveOutput$caption,
        color = curveOutput$color,
        linetype = curveOutput$linetype,
        size = curveOutput$size,
        shape = curveOutput$shape,
        plotObject = timeProfilePlot
      )
    }
    # Set axes based on Axes properties
    timeProfilePlot <- updatePlotAxes(timeProfilePlot, axesProperties)

    # Save results
    timeProfileResults[[plotID]] <- saveTaskResults(
      id = plotID,
      sectionId = timeProfilePlan$SectionId,
      plot = timeProfilePlot,
      plotCaption = timeProfilePlan$Plot$Name
    )
  }
  return(timeProfileResults)
}
