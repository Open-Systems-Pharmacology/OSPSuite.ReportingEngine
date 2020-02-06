#' @title CalculatePKParametersTask
#' @docType class
#' @description  Calculating PK parameters task settings for Reporting Engine
#' @field resultsFileName string with no file extension.  Name of CSV file to which simulation results will be saved.
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize Task settings}
#' \item{activate()}{Set Task as active in workflow}
#' \item{inactivate()}{Set Task as inactive in workflow}
#' \item{print()}{Show task settings}
#' }
#' @format NULL
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
    initialize = function(simulationFilePath,
                          simulationResultFilePaths,
                          pkParametersToEvaluate = NULL,
                          userDefinedPKFunctions = NULL,
                          pkParameterResultsFilePath = file.path(getwd(),"pkParameters.csv"),
                          ...) {
      super$initialize(...)
      self$simulationFilePath <- simulationFilePath
      self$simulationResultFilePaths <- simulationResultFilePaths
      self$pkParametersToEvaluate <- pkParametersToEvaluate
      self$userDefinedPKFunctions <- userDefinedPKFunctions
      self$pkParameterResultsFilePath <- pkParameterResultsFilePath
    }
  )
)
