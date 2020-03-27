#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param simulationFilePath path to pkml model file
#' @param simulationResultFilePaths path to simulation CSV results files
#' @param pkParametersToEvaluate vector of PK parameters to evaluate
#' @param userDefinedPKFunctions vector of userDefinedPKFunction objects
#' @param pkParameterResultsFilePath path to PK analysis result file
#' @return pkParameterResultsFilePath, paths to pk results file CSV
#' @export
#' @import ospsuite
calculatePKParameters <- function(simulationFilePath,
                                  simulationResultFilePaths,
                                  pkParametersToEvaluate = NULL,
                                  userDefinedPKFunctions = NULL,
                                  pkParameterResultsFilePath) {
  sim <- ospsuite::loadSimulation(simulationFilePath)
  res <- ospsuite::importResultsFromCSV(simulation = sim, filePaths = simulationResultFilePaths)
  pkAnalyses <- calculatePKAnalyses(results = res)
  exportPKAnalysesToCSV(pkAnalyses = pkAnalyses, filePath = pkParameterResultsFilePath)
  return(pkParameterResultsFilePath)
}

#' @title calculateMeanPKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @return pkAnalyses object
#' @export
#' @import ospsuite
calculateMeanPKParameters <- function(structureSet) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )

  pkAnalyses <- ospsuite::calculatePKAnalyses(simulationResults)

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
