#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @return pkAnalyses object
#' @export
#' @import ospsuite
calculatePKParameters <- function(structureSet) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation = simulation,
    filePaths = structureSet$simulationResultFileNames
  )
  pkAnalyses <- calculatePKAnalyses(results = simulationResults)
  return(pkAnalyses)
}

#' @title plotMeanPKParameters
#' @description Plot PK parameters table
#' @param structureSet `SimulationStructure` R6 class object
#' @param plotConfigurations list of configurations for the output table/plot
#' @return data.frame with calculated PK parameters for the simulation set
#' @export
#' @import ospsuite
plotMeanPKParameters <- function(structureSet,
                                 plotConfigurations) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  pkParametersTable <- pkParametersTable[, c("QuantityPath", "Display", "Value", "Unit")]
  names(pkParametersTable) <- c("Path", "Parameter", "Value", "Unit")

  return(pkParametersTable)
}
