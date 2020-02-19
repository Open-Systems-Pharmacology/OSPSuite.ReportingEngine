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

    #' @description
    #' Create a `SensitivityAnalysisTask` object
    #' @param variationRange variation range for sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    #' @param generatedResultFileNames name of files where PK parameters are saved
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(
      variationRange = NULL,
      numberOfCores = NULL,
      quantileVec = NULL,
      ...) {
      super$initialize(...)
      self$variationRange <- variationRange
      self$numberOfCores <- numberOfCores
      self$quantileVec <- quantileVec
    }
  )
)
