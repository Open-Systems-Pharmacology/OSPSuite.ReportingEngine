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
    #' @param  xAxisFontSize for sensitivity plot
    #' @param  yAxisFontSize for sensitivity plot
    #' @return A new `SensitivityPlotSettings` object
    initialize = function(totalSensitivityThreshold = NULL,
                          variableParameterPaths = NULL,
                          maximalParametersPerSensitivityPlot = 50,
                          plotConfiguration = NULL,
                          xAxisFontSize = 6,
                          yAxisFontSize = 6) {
      self$totalSensitivityThreshold <- getDefaultTotalSensitivityThreshold(
        totalSensitivityThreshold = totalSensitivityThreshold,
        variableParameterPaths = variableParameterPaths
      )

      self$maximalParametersPerSensitivityPlot <- maximalParametersPerSensitivityPlot

      self$plotConfiguration <- plotConfiguration

      self$xAxisFontSize <- xAxisFontSize
      self$yAxisFontSize <- yAxisFontSize
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
    }, # ,


    #' @field xAxisFontSize for sensitivity plot
    xAxisFontSize = function(value) {
      if (missing(value)) {
        private$.plotConfiguration$labels$xlabel$font$size
      } else {
        validateIsNumeric(object = value, nullAllowed = FALSE)
        validateIsPositive(object = value, nullAllowed = FALSE)
        private$.plotConfiguration$labels$xlabel$font$size <- value
      }
    },

    #' @field yAxisFontSize for sensitivity plot
    yAxisFontSize = function(value) {
      if (missing(value)) {
        private$.plotConfiguration$labels$ylabel$font$size
      } else {
        validateIsNumeric(object = value, nullAllowed = FALSE)
        validateIsPositive(object = value, nullAllowed = FALSE)
        private$.plotConfiguration$labels$ylabel$font$size <- value
      }
    }
  ),

  private = list(
    .totalSensitivityThreshold = NULL,
    .maximalParametersPerSensitivityPlot = NULL,
    .plotConfiguration = NULL
  )
)
