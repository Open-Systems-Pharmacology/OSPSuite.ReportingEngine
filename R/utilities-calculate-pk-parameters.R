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
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  #pkParametersTable <- pkParametersTable[, c("QuantityPath", "Parameters", "Value", "Unit")]

  return(list(plots = NULL,
              tables = list(pkAnalysis = pkParametersTable)))
}
