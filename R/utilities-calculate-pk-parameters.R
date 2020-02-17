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

#' @title UserDefinedPKFunction
#' @docType class
#' @description  UserDefinedPKFunction R6 class
#' @export
UserDefinedPKFunction <- R6::R6Class(
  "UserDefinedPKFunction",
  public = list(
    pKParameterName = NULL,
    pKFunction = NULL,
    pKParameterUnit = NULL,

    initialize = function(pKParameterName, pKFunction, pKParameterUnit = NULL) {
      self$pKParameterName <- pKParameterName
      self$pKFunction <- pKFunction
      self$pKParameterUnit <- pKParameterUnit
    }
  )
)

#' @title AllPKParameters
#' @description  list of usual PK parameters from OSP Suite
#' @export
AllPKParameters <- enum(c(
  "C_max",
  "C_max_norm",
  "t_max",
  "C_tEnd",
  "AUC",
  "AUC_norm",
  "AUC_inf",
  "AUC_inf_norm",
  "MRT",
  "Thalf",
  "CL",
  "Vss",
  "Vd"
))
