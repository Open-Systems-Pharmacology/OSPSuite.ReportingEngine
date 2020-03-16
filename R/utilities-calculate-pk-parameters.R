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
  sim <- loadSimulation(simulationFilePath)
  res <- importResultsFromCSV(simulation = sim, filePaths = simulationResultFilePaths)
  pkAnalyses <- calculatePKAnalyses(results = res)
  exportPKAnalysesToCSV(pkAnalyses = pkAnalyses, filePath = pkParameterResultsFilePath)
  return(pkParameterResultsFilePath)
}
