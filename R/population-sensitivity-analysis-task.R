#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
#' @field variationRange variation range for sensitivity analysis
#' @field numberOfCores number of cores for parallel computation
#' @field quantileVec vector of quantiles to be calculated
#' @field variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
#' @field pkParameterSelection list of selected PK parameters for sensitivity analysis
PopulationSensitivityAnalysisTask <- R6::R6Class(
  "PopulationSensitivityAnalysisTask",
  inherit = SensitivityAnalysisTask,
  public = list(
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `SensitivityAnalysisTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings instance of SensitivityAnalysisSettings class
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                          settings = NULL,
                          ...) {
      super$initialize(...)
      if(is.null(settings)){
        self$settings <- SensitivityAnalysisSettings$new()
      } else{
        validateIsOfType(object = settings,SensitivityAnalysisSettings)
        self$settings <- settings
      }
      self$getTaskResults <- getTaskResults
    }

  )
)
