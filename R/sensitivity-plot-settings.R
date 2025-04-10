#' @title SensitivityPlotSettings
#' @description  R6 class for sensitivity analysis plot settings
#' @export
#' @importFrom ospsuite.utils %||%
SensitivityPlotSettings <- R6::R6Class(
  "SensitivityPlotSettings",
  public = list(
    #' @description
    #' Create a `SensitivityPlotSettings` object
    #' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    #' @param variableParameterPaths paths that were varied in the sensitivity analysis.  If supplied totalSensitivityThreshold = 1, else 0.9.
    #' @param maximalParametersPerSensitivityPlot maximalParametersPerSensitivityPlot is the maximal number of parameters to display in a sensitivity plot
    #' @param plotConfiguration `PlotConfiguration` object from `tlf` library
    #' @param xAxisFontSize Font size of x-axis labels for sensitivity plot
    #' @param yAxisFontSize Font size of y-axis labels for sensitivity plot
    #' @param xLabel Label of x-axis for sensitivity plot
    #' @param yLabel Label of y-axis for sensitivity plot
    #' @param maxLinesPerParameter maxLinesPerParameter maximum number of lines allowed per displayed parameters
    #' @param maxWidthPerParameter maximum number of characters allowed per lines of displayed parameters
    #' @param colorPalette Name of a color palette to be used by `ggplot2::scale_fill_brewer()` for sensitivity plot
    #' @return A new `SensitivityPlotSettings` object
    initialize = function(totalSensitivityThreshold = NULL,
                          variableParameterPaths = NULL,
                          maximalParametersPerSensitivityPlot = NULL,
                          plotConfiguration = NULL,
                          xAxisFontSize = 6,
                          yAxisFontSize = 6,
                          maxLinesPerParameter = NULL,
                          maxWidthPerParameter = NULL,
                          xLabel = "Sensitivity",
                          yLabel = NULL,
                          colorPalette = "Spectral") {
      validateIsInteger(maximalParametersPerSensitivityPlot, nullAllowed = TRUE)
      validateIsInteger(maxLinesPerParameter, nullAllowed = TRUE)
      validateIsInteger(maxWidthPerParameter, nullAllowed = TRUE)
      validateIsNumeric(xAxisFontSize, nullAllowed = TRUE)
      validateIsNumeric(yAxisFontSize, nullAllowed = TRUE)

      private$.totalSensitivityThreshold <- getDefaultTotalSensitivityThreshold(
        totalSensitivityThreshold = totalSensitivityThreshold,
        variableParameterPaths = variableParameterPaths
      )

      private$.maximalParametersPerSensitivityPlot <- maximalParametersPerSensitivityPlot %||% reEnv$maximalParametersPerSensitivityPlot
      private$.plotConfiguration <- plotConfiguration
      private$.xAxisFontSize <- xAxisFontSize
      private$.yAxisFontSize <- yAxisFontSize
      private$.maxLinesPerParameter <- maxLinesPerParameter %||% reEnv$maxLinesPerParameter
      # Default line breaks will now be limited to 1/3 of plot width
      defaultPlotConfiguration <- tlf::TornadoPlotConfiguration$new(bar = FALSE)
      defaultPlotConfiguration$yAxis$font$size <- yAxisFontSize
      private$.maxWidthPerParameter <- maxWidthPerParameter %||% getLineBreakWidth(element = "yticklabels", plotConfiguration %||% defaultPlotConfiguration)
      private$.xLabel <- xLabel
      private$.yLabel <- yLabel
      private$.colorPalette <- colorPalette
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
          validateIsInteger(value)
          private$.maximalParametersPerSensitivityPlot <- value
        }
      }
    },


    #' @field  plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
    plotConfiguration = function(value) {
      if (missing(value)) {
        private$.plotConfiguration
      } else {
        validateIsOfType(value, "PlotConfiguration", nullAllowed = TRUE)
        private$.plotConfiguration <- value
      }
    },

    #' @field xAxisFontSize for sensitivity plot.
    #' This Value will overwrite values defined by plot configuration object
    xAxisFontSize = function(value) {
      if (missing(value)) {
        private$.xAxisFontSize
      } else {
        validateIsNumeric(object = value, nullAllowed = FALSE)
        validateIsPositive(object = value, nullAllowed = FALSE)
        private$.xAxisFontSize <- value
      }
    },

    #' @field yAxisFontSize for sensitivity plot.
    #' This Value will overwrite values defined by plot configuration object
    yAxisFontSize = function(value) {
      if (missing(value)) {
        private$.yAxisFontSize
      } else {
        validateIsNumeric(object = value, nullAllowed = FALSE)
        validateIsPositive(object = value, nullAllowed = FALSE)
        private$.yAxisFontSize <- value
      }
    },

    #' @field xLabel for sensitivity plot.
    #' This Value will overwrite values defined by plot configuration object
    xLabel = function(value) {
      if (missing(value)) {
        private$.xLabel
      } else {
        validateIsString(value, nullAllowed = TRUE)
        private$.xLabel <- value
      }
    },

    #' @field yLabel for sensitivity plot.
    #' This Value will overwrite values defined by plot configuration object
    yLabel = function(value) {
      if (missing(value)) {
        private$.yLabel
      } else {
        validateIsString(value, nullAllowed = TRUE)
        private$.yLabel <- value
      }
    },

    #' @field colorPalette for sensitivity plot.
    #' This Value will overwrite values defined by plot configuration object
    colorPalette = function(value) {
      if (missing(value)) {
        private$.colorPalette
      } else {
        validateIsOfType(value, c("character", "numeric"), nullAllowed = TRUE)
        private$.colorPalette <- value
      }
    },

    #' @field maxLinesPerParameter maximum number of lines allowed per displayed parameters
    maxLinesPerParameter = function(value) {
      if (missing(value)) {
        private$.maxLinesPerParameter
      } else {
        validateIsInteger(value, nullAllowed = TRUE)
        private$.maxLinesPerParameter <- value %||% private$.maxLinesPerParameter
      }
    },

    #' @field maxWidthPerParameter maximum number of characters allowed per lines of displayed parameters
    maxWidthPerParameter = function(value) {
      if (missing(value)) {
        private$.maxWidthPerParameter
      } else {
        validateIsInteger(value, nullAllowed = TRUE)
        private$.maxWidthPerParameter <- value %||% private$.maxWidthPerParameter
      }
    }
  ),
  private = list(
    .totalSensitivityThreshold = NULL,
    .maximalParametersPerSensitivityPlot = NULL,
    .plotConfiguration = NULL,
    .xAxisFontSize = NULL,
    .yAxisFontSize = NULL,
    .maxLinesPerParameter = NULL,
    .maxWidthPerParameter = NULL,
    .xLabel = NULL,
    .yLabel = NULL,
    .colorPalette = NULL
  )
)
