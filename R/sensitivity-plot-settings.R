#' @title SensitivityPlotSettings
#' @description  R6 class for sensitivity analysis plot settings
#' @export
SensitivityPlotSettings <- R6::R6Class(
  "SensitivityPlotSettings",
  public = list(

    #' @description
    #' Create a `SensitivityPlotSettings` object
    #' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    #' @return A new `SensitivityAnalysisSettings` object
    initialize = function(totalSensitivityThreshold = NULL,
                          variableParameterPaths = NULL) {
      self$totalSensitivityThreshold <- getDefaultTotalSensitivityThreshold(totalSensitivityThreshold = totalSensitivityThreshold,
                                                                            variableParameterPaths = variableParameterPaths)
      }

  ),

  active = list(
    #' @field totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    totalSensitivityThreshold = function(value) {
      if (missing(value)) {
        private$.totalSensitivityThreshold
      } else {
        if (!is.null(value)) {
          validateIsInRange('totalSensitivityThreshold',value,0,1)
          private$.totalSensitivityThreshold <- value
        }
      }
    }
  ),

  private = list(
    .totalSensitivityThreshold = NULL
  )
)
