#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
#' @field  variationRange variation range for sensitivity analysis
#' @field numberOfCores number of cores for parallel computation
#' @field generatedResultFileNames name of files where PK parameters are saved
SensitivityAnalysisTask <- R6::R6Class(
  "SensitivityAnalysisTask",
  inherit = Task,
  public = list(
    variationRange = NULL,
    numberOfCores = NULL,
    quantileVec = NULL,
    variableParameterPaths = NULL,

    #' @description
    #' Create a `SensitivityAnalysisTask` object
    #' @param variationRange variation range for sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    #' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(
                              variationRange = NULL,
                              numberOfCores = NULL,
                              quantileVec = NULL,
                              variableParameterPaths = NULL,
                              ...) {
      super$initialize(...)

      if (!is.null(variationRange)) {
        validateIsNumeric(variationRange)
        validateIsOfLength(variationRange, nbElements = 1)
        self$variationRange <- variationRange
      }

      if (!is.null(numberOfCores)) {
        validateIsInteger(numberOfCores)
        self$numberOfCores <- numberOfCores
      }

      if (!is.null(quantileVec)) {
        validateIsNumeric(quantileVec)
        self$quantileVec <- quantileVec
      }

      if (!is.null(variableParameterPaths)) {
        validateIsString(variableParameterPaths)
        self$variableParameterPaths <- variableParameterPaths
      }
    }
  )
)
