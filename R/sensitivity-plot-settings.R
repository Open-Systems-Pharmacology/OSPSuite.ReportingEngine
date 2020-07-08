#' @title SensitivityPlotSettings
#' @description  R6 class for sensitivity analysis plot settings
#' @export
SensitivityPlotSettings <- R6::R6Class(
  "SensitivityPlotSettings",
  public = list(
    #' @description
    #' Create a `SensitivityPlotSettings` object
    #' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    #' @param variableParameterPaths paths that were varied in the sensitivity analysis.  If supplied totalSensitivityThreshold = 1, else 0.9.
    #' @param maximalParametersPerSensitivityPlot maximalParametersPerSensitivityPlot is the maximal number of parameters to display in a sensitivity plot
    #' @param  plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
    #' @return A new `SensitivityPlotSettings` object
    initialize = function(totalSensitivityThreshold = NULL,
                          variableParameterPaths = NULL,
                          maximalParametersPerSensitivityPlot = 50,
                          plotConfiguration = NULL) {
      self$totalSensitivityThreshold <- getDefaultTotalSensitivityThreshold(
        totalSensitivityThreshold = totalSensitivityThreshold,
        variableParameterPaths = variableParameterPaths
      )

      self$maximalParametersPerSensitivityPlot <- maximalParametersPerSensitivityPlot

      self$plotConfiguration <- plotConfiguration
    }
  ),

  active = list(
    #' @field totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    totalSensitivityThreshold = function(value) {
      if (missing(value)) {
        private$.totalSensitivityThreshold
      } else {
        if (!is.null(value)) {
          validateIsInRange("totalSensitivityThreshold", value, 0, 1)
          private$.totalSensitivityThreshold <- value
        }
      }
    },

    #' @field maximalParametersPerSensitivityPlot is the maximal number of parameters to display in a sensitivity plot
    maximalParametersPerSensitivityPlot = function(value) {
      if (missing(value)) {
        private$.maximalParametersPerSensitivityPlot
      } else {
        if (!is.null(value)) {
          validateIsInRange("maximalParametersPerSensitivityPlot", value, 0, Inf)
          private$.maximalParametersPerSensitivityPlot <- value
        }
      }
    },


    #' @field  plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
    plotConfiguration = function(value) {
      if (missing(value)) {
        private$.plotConfiguration
      } else {
        validateIsOfType(object = value, type = tlf::PlotConfiguration, nullAllowed = TRUE)
        private$.plotConfiguration <- value %||% tlf::PlotConfiguration$new()
      }
    }
  ),

  private = list(
    .totalSensitivityThreshold = NULL,
    .maximalParametersPerSensitivityPlot = NULL,
    .plotConfiguration = NULL
  )
)
