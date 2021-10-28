# Environment that holds various global variables and settings for ospsuite.reportingengine package,
# It is not exported and should not be directly manipulated by other packages.
reEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
reEnv$packageName <- "ospsuite.reportingengine"

# Default format values for numerics
reEnv$formatNumericsDigits <- 2
reEnv$formatNumericsScientific <- NA

# Default binning properties
reEnv$defaultBins <- 11
reEnv$binUsingQuantiles <- TRUE
reEnv$defaultStairstep <- TRUE

reEnv$defaultWatermarkMessage <- "preliminary analysis"
reEnv$defaultSimulationSetDescriptor <- ""

reEnv$maximalParametersPerSensitivityPlot <- 25
reEnv$maxWidthPerParameter <- 25
reEnv$maxLinesPerParameter <- 3

# Default plot properties
reEnv$theme$background$legendPosition <- tlf::LegendPositions$outsideTop
# reEnv$defaultLegendPosition <- tlf::LegendPositions$outsideTop

reEnv$defaultPlotFormat <- list(
  format = "png",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

reEnv$workflowWatermarkMessage <- "preliminary analysis"

# Default values for qualification pk ratio
reEnv$pkRatio <- list()
reEnv$pkRatio$dictionary <- list(
  id = "ID",
  study = "Study",
  parameterColumn = "Avg",
  unitColumn = "AvgUnit",
  prefixObserved = "Observed",
  prefixSimulated = "Predicted",
  prefixRatio = "Pred/Obs",
  suffixRatio = "Ratio"
)

#' @title setWatermarkConfiguration
#' @description Set default watermark configuration for current theme
#' @param watermark character or \code{Label} class object from `tlf` package
#' @export
#' @import tlf
#' @examples 
#' setWatermarkConfiguration("Confidential")
#' setWatermarkConfiguration(Label$new(text = "test", color = "blue"))
setWatermarkConfiguration <- function(watermark = NULL) {
  tlf::setDefaultWatermark(watermark)
}

#' @title setDefaultPlotFormat
#' @description Set default plot format to be exported by reporting engine
#' @param format file format of the exported plots. E.g. "png" or "pdf"
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height` included in "in", "cm", "mm", or "px"
#' @param dpi Plot resolution in dots per inch. Caution, font sizes depend on resolution.
#' @export
#' @examples 
#' setDefaultPlotFormat(format = "pdf")
#' setDefaultPlotFormat(width = 16, height = 9, units = "cm", dpi = 300)
setDefaultPlotFormat <- function(format = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL) {
  validateIsNumeric(width, nullAllowed = TRUE)
  validateIsNumeric(height, nullAllowed = TRUE)
  validateIsNumeric(dpi, nullAllowed = TRUE)
  validateIsIncluded(units, c("in", "cm", "mm", "px"), nullAllowed = TRUE)
  reEnv$defaultPlotFormat$format <- format %||% reEnv$defaultPlotFormat$format
  reEnv$defaultPlotFormat$width <- width %||% reEnv$defaultPlotFormat$width
  reEnv$defaultPlotFormat$height <- height %||% reEnv$defaultPlotFormat$height
  reEnv$defaultPlotFormat$dpi <- dpi %||% reEnv$defaultPlotFormat$dpi
  # ggplot2 version 3.3.0 does not include pixels yet
  # Convert width and height back into inches in case of units as pixels
  if (isIncluded(units, "px")) {
    units <- "in"
    unitConversionFactor <- grDevices::dev.size("in") / grDevices::dev.size("px")
    reEnv$defaultPlotFormat$width <- reEnv$defaultPlotFormat$width * unitConversionFactor[1]
    reEnv$defaultPlotFormat$height <- reEnv$defaultPlotFormat$height * unitConversionFactor[2]
  }
  reEnv$defaultPlotFormat$units <- units %||% reEnv$defaultPlotFormat$units
  return(invisible())
}

#' @title setPlotFormat
#' @description Set default plot format to be exported by reporting engine
#' @inheritParams setDefaultPlotFormat
#' @export
setPlotFormat <- function(format = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL) {
  setDefaultPlotFormat(format, width, height, units, dpi)
}

#' @title ApplicationRanges
#' @description
#' Keys of reported ranges when simulation includes multiple applications
#' @export
#' @import ospsuite
#' @examples
#' ApplicationRanges$total
#' ApplicationRanges$firstApplication
#' ApplicationRanges$lastApplication
ApplicationRanges <- ospsuite::enum(c("total", "firstApplication", "lastApplication"))

#' @title setDefaultNumericFormat
#' @description Set default format for numeric values output in reports
#' @param digits Number of significant digits
#' @param scientific Logical defining if numeric format uses a scientific expression
#' @export
#' @examples 
#' setDefaultNumericFormat(digits = 2, scientific = TRUE)
setDefaultNumericFormat <- function(digits = NULL, scientific = NULL) {
  validateIsInteger(digits, nullAllowed = TRUE)
  validateIsLogical(scientific, nullAllowed = TRUE)

  reEnv$formatNumericsDigits <- digits %||% reEnv$formatNumericsDigits
  reEnv$formatNumericsScientific <- scientific %||% reEnv$formatNumericsScientific
  return(invisible())
}

# Default theme
reEnv$theme <- tlf::loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf"))

#' @title setDefaultTheme
#' @param theme `Theme` object from `tlf` package
#' @export
setDefaultTheme <- function(theme) {
  validateIsOfType(theme, "Theme")
  reEnv$theme <- theme
  tlf::useTheme(reEnv$theme)
}

#' @title setDefaultThemeFromJson
#' @param jsonFile path to json file where theme properties are stored
#' @export
setDefaultThemeFromJson <- function(jsonFile) {
  validateIsString(jsonFile)
  validateIsFileExtension(jsonFile, "json")
  newTheme <- tlf::loadThemeFromJson(jsonFile)
  setDefaultTheme(newTheme)
}

#' @title setDefaultBins
#' @param bins number or edges of bins
#' @export
#' @examples 
#' setDefaultBins(10)
setDefaultBins <- function(bins) {
  validateIsNumeric(bins)
  reEnv$defaultBins <- bins
}

#' @title setDefaultStairstep
#' @param stairstep logical defining if stairstep should be plotted by default when aggregation is performed
#' @export
#' @examples 
#' setDefaultStairstep(TRUE)
setDefaultStairstep <- function(stairstep) {
  validateIsLogical(stairstep)
  reEnv$defaultStairstep <- stairstep
}
