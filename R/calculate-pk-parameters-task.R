#' @title CalculatePKParametersTask
#' @description  R6 class for CalculatePKParametersTask settings
#' @field simulationFilePath simulation folder
#' @field simulationResultFilePaths simulation files
#' @field pkParametersToEvaluate list of PK parameters to evaluate
#' @field userDefinedPKFunctions list of user defined functions to calculate PK parameters
#' @field pkParameterResultsFilePath files where PK parameters are saved
#' @field generatedResultFileNames name of files where PK parameters are saved
CalculatePKParametersTask <- R6::R6Class(
  "CalculatePKParametersTask",
  inherit = Task,
  public = list(
    simulationFilePath = NULL,
    simulationResultFilePaths = NULL,
    pkParametersToEvaluate = NULL,
    userDefinedPKFunctions = NULL,
    pkParameterResultsFilePath = NULL,
    generatedResultFileNames = NULL,

    #' @description
    #' Create a `CalculatePKParametersTask` object
    #' @param simulationFilePath simulation folder
    #' @param simulationResultFilePaths simulation files
    #' @param pkParametersToEvaluate list of PK parameters to evaluate
    #' @param userDefinedPKFunctions list of user defined functions to calculate PK parameters
    #' @param pkParameterResultsFilePath files where PK parameters are saved
    #' @param generatedResultFileNames name of files where PK parameters are saved
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `CalculatePKParametersTask` object
    initialize = function(...) {
      super$initialize(...)
    }
  )
)
