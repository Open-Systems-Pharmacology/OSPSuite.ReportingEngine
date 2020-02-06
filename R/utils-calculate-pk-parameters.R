#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @return generatedResultFileNames
#' @export
#' @import ospsuite
calculatePKParameters <- function(simulationFilePath,
                                  simulationResultFilePaths,
                                  pkParametersToEvaluate = NULL,
                                  userDefinedPKFunctions = NULL,
                                  pkParameterResultsFilePath){
res <- importResultsFromCSV(simulation = loadSimulation(simulationFilePath),filePaths = simulationResultFilePaths)
pkAnalyses <- calculatePKAnalyses(results = res)
exportPKAnalysesToCSV(pkAnalyses = pkAnalyses,filePath = pkParameterResultsFilePath)
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
