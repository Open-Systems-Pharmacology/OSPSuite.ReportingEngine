# Template for user-defined functions
# For simplicity, the documentation follows the usual structure of R packages documentation

# The first template, plotApplicationProfile, plot the time profile of an application


#' @title plotApplicationProfile
#' @description Simulate and plot the profile of the application
#' @param structureSet `SimulationStructure` object used by `Workflow` object.
#' Contains knowledge of where intermediate simulations and their results are stored
#' @param logFolder folder where the log files are saved
#' @param settings optional list of settings that can be modified
#' @return list of `plots`, `tables` and `captions` to be saved and printed in the report
plotApplicationProfile <- function(structureSet,
                                   logFolder,
                                   settings) {
  # Load the simulation
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)

  # Get the application path and simulate its values within the simulation
  applications <- ospsuite::getContainer("Applications", simulation)
  appliedMoleculePaths <- ospsuite::getAllMoleculePathsIn(applications)
  appliedMoleculePaths <- appliedMoleculePaths[1]
  applicationQuantity <- ospsuite::getQuantity(appliedMoleculePaths, simulation)
  ospsuite::clearOutputs(simulation)
  addOutputs(quantitiesOrPaths = applicationQuantity, simulation = simulation)
  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutput <- ospsuite::getOutputValues(
    simulationResults = simulationResults,
    quantitiesOrPaths = applicationQuantity
  )

  # Create a plot of profile
  # Tip: The plotCOnfiguration can be set as settings argument
  # so it can be updated from the task directly
  applicationPlot <- tlf::addLine(
    x = simulationResultsOutput$data[, "Time"],
    y = simulationResultsOutput$data[, appliedMoleculePaths],
    color = "firebrick",
    size = 2,
    plotConfiguration = settings$plotConfiguration
  )

  # Update plot aspect
  applicationPlot <- tlf::setPlotLabels(
    plotObject = applicationPlot,
    ylabel = paste0(
      "Application Profile [",
      simulationResultsOutput$metaData[[appliedMoleculePaths]]$unit, "]"
    )
  )
  applicationPlot <- tlf::setLegendPosition(plotObject = applicationPlot, position = tlf::LegendPositions$none)

  return(list(
    plots = list(application = applicationPlot),
    captions = list(application = paste0("Application profile for '", appliedMoleculePaths, "'"))
  ))
}



#' @title plotAUCRatios
#' @description Get results of PK Parameters across populations and plot AUC ratios as a function of Age
#' @param structureSets list of `SimulationStructure` objects used by `Workflow` object.
#' Contains knowledge of where intermediate simulations and their results are stored
#' @param logFolder folder where the log files are saved
#' @param settings optional list of settings that can be modified
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @param xParameters optional list of parameters to be plotted along x axis (required for plotPKParameters and plotDemograpy tasks)
#' @param yParameters optional list of parameters to be plotted along y axis (required for plotPKParameters and plotDemograpy tasks)
#' @return list of `plots`, `tables` and `captions` to be saved and printed in the report
plotAUCRatios <- function(structureSets,
                          logFolder,
                          settings,
                          workflowType = PopulationWorkflowTypes$parallelComparison,
                          xParameters,
                          yParameters) {
  # Recover the PK parameters from calculatePKParameters task
  pkParametersAcrossPopulations <- getPkParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData

  # Get AUC values from Output object
  outputForAUC <- Output$new(
    path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    displayName = "Venous Blood",
    pkParameters = "AUC_inf"
  )

  pkParameterFromOutput <- getPopulationPkAnalysesFromOuptut(
    pkParametersDataAcrossPopulations,
    pkParametersMetaDataAcrossPopulations,
    outputForAUC,
    outputForAUC$pkParameters[[1]]
  )

  pkParameterData <- pkParameterFromOutput$data
  pkParameterMetaData <- pkParameterFromOutput$metaData

  # Calculate an AUC ratio
  pkParameterData$RatioAUC <- pkParameterData$Value / median(pkParameterData$Value[pkParameterData$simulationSetName == "Adults"])
  pkParameterMetaData$RatioAUC <- list(dimension = "AUC Ratio [fraction of median adult]", unit = "")

  # Plot the AUC ratios
  pkRatioMapping <- tlf::PKRatioDataMapping$new(
    x = StandardPath$Age,
    y = "RatioAUC",
    color = "simulationSetName"
  )

  pkRatioPlot <- tlf::plotPKRatio(
    data = pkParameterData,
    metaData = pkParameterMetaData,
    dataMapping = pkRatioMapping,
    plotConfiguration = NULL
  )

  pkRatioTable <- tlf::getPKRatioMeasure(data = pkParameterData, dataMapping = pkRatioMapping)
  pkRatioTable$Measure <- c("All", "Within 1.5-fold", "Within 2-fold")

  return(list(
    plots = list("pkratio-AUC" = pkRatioPlot),
    tables = list("pkratio-AUC" = pkRatioTable),
    captions = list("pkratio-AUC" = "AUC Ratios across populations")
  ))
}
