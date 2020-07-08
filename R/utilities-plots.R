# Currently, need to load a RData because, jsonlite need to be in Namespace/Description otherwise
# The code for loading the json is consequently left commented
# reThemeProperties <- jsonlite::fromJSON('./data/RE-theme.json')
load("./data/reThemeProperties.RData")
reTheme <- tlf::Theme$new(
  themesProperties = reThemeProperties,
  labelBaseSize = 8
)
reTheme$titleFont$size <- 10
reTheme$subtitleFont$size <- 9
tlf::useTheme(reTheme)

# Set default legend position to outside top
reDefaultLegendPosition <- tlf::LegendPositions$outsideTop
tlf::setDefaultLegendPosition(reDefaultLegendPosition)


# Plot format
# TO DO: set default formats according to task or according to legend size
ExportPlotConfigurationClass <- R6::R6Class(
  "ExportPlotConfiguration",
  public = list(format = "png", width = 4 * 5, height = 3 * 5, units = "cm")
)

#' @title ExportPlotConfiguration
#' @description Properties of exported plots
#' @export
ExportPlotConfiguration <- ExportPlotConfigurationClass$new()

#' @title setPlotFormat
#' @description Set plot format
#' @param format file format of the exported plots
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height`
#' @return demographyPlots list of ggplot objects
#' @export
setPlotFormat <- function(format, width = NULL, height = NULL, units = NULL) {
  formatInputs <- c("format", "width", "height", "units")
  setConfigurationExpression <- parse(text = paste0(
    "ExportPlotConfiguration$", formatInputs, " <- ",
    formatInputs, " %||% ExportPlotConfiguration$", formatInputs
  ))
  eval(setConfigurationExpression)
}

#' @title AggregationConfiguration
#' @description Aggregation default properties  (which functions and their captions)
#' @export
AggregationConfiguration <- list(
  functions = list(
    middle = median,
    ymin = function(x) {
      as.numeric(quantile(x, probs = 0.05))
    },
    ymax = function(x) {
      as.numeric(quantile(x, probs = 0.95))
    }
  ),
  names = list(
    middle = "median",
    range = "[5-95th] percentiles"
  )
)
