#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @param logFolder folder where the logs are saved
#' @return pkAnalysis object
#' @export
#' @import ospsuite
calculatePKParameters <- function(structureSet,
                                  settings = NULL,
                                  logFolder = getwd()) {
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation = simulation,
    filePaths = structureSet$simulationResultFileNames
  )

  logWorkflow(
    message = paste0("Simulation results '", structureSet$simulationResultFileNames, "' successfully loaded"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  pkAnalyses <- calculatePKAnalyses(results = simulationResults)
  logWorkflow(
    message = "Calculation of PK parameters complete",
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(pkAnalyses)
}

#' @title plotMeanPKParameters
#' @description Plot PK parameters table
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @return data.frame with calculated PK parameters for the simulation set
#' @export
#' @import ospsuite
plotMeanPKParameters <- function(structureSet,
                                 logFolder = getwd(),
                                 settings = NULL) {
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  # pkParametersTable <- pkParametersTable[, c("QuantityPath", "Parameters", "Value", "Unit")]

  return(list(
    plots = NULL,
    tables = list(pkAnalysis = pkParametersTable)
  ))
}


#' @title plotPopulationPKParameters
#' @description Plot PK parameters box plots and tables
#' @param structureSets `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @return data.frame with calculated PK parameters for the simulation set
#' @export
#' @import ospsuite
plotPopulationPKParameters <- function(structureSets,
                                       logFolder = getwd(),
                                       settings = NULL,
                                       workflowType = PopulationWorkflowTypes$parallelComparison) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)

  pkParametersTableAcrossPopulations <- NULL
  pkRatioTableAcrossPopulations <- NULL
  
  for (index in seq_along(popW$simulationStructures))
  {
    structureSet <- popW$simulationStructures[[index]]

    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
      structureSet$pkAnalysisResultsFileNames,
      simulation
    )

    pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)

    pkParametersTable$Population <- structureSet$simulationSet$populationName

    pkParametersTableAcrossPopulations <- rbind.data.frame(
      pkParametersTableAcrossPopulations,
      pkParametersTable
    )
  }

  pkParametersPlots <- NULL
  pkParametersTables <- NULL
  
  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "Population",
    y = "Value"
  )

  for (parameter in levels(pkParametersTableAcrossPopulations$Parameter)) {
    pkParameterData <- pkParametersTableAcrossPopulations[pkParametersTableAcrossPopulations$Parameter %in% parameter, ]
    pkParameterData <- pkParameterData[!is.na(pkParameterData$Value), ]
    pkParameterMetaData <- list("Value" = list(
      dimension = parameter,
      unit = pkParameterData$Unit[1]
    ))
    pkParametersPlots[[parameter]] <- tlf::plotBoxWhisker(
      data = pkParameterData,
      metaData = pkParameterMetaData,
      dataMapping = pkParametersMapping,
      plotConfiguration = settings$plotConfiguration
    ) +
      ggplot2::labs(title = NULL, subtitle = NULL)

    pkParametersTables[[parameter]] <- tlf::getBoxWhiskerMeasure(
      data = pkParameterData,
      dataMapping = pkParametersMapping
    )
  }
  
  return(list(
    plots = pkParametersPlots,
    tables = pkParametersTables
  ))
}
