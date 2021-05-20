# Environment that holds various global variables and settings for ospsuite.reportingengine package,
# It is not exported and should not be directly manipulated by other packages.
reEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
reEnv$packageName <- "ospsuite.reportingengine"

# Default format values for numerics
reEnv$formatNumericsDigits <- 5
reEnv$formatNumericsSmall <- 2
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

#' @title ApplicationRanges
#' @description
#' Keys of reported ranges when simulation includes multiple applications
#' @export
#' @import ospsuite
ApplicationRanges <- ospsuite::enum(c("total", "firstApplication", "lastApplication"))

#' @title setDefaultNumericFormat
#' @description Set default format for numeric values output in reports
#' @param digits Number of significant digits
#' @param nsmall Number of decimal digits
#' (Note: `nsmall` needs to be consistent with `digits` for being correctly used)
#' @param scientific Logical defining if numeric format uses a scientific expression
#' @export
setDefaultNumericFormat <- function(digits = NULL,
                                    nsmall = NULL,
                                    scientific = NULL) {
  validateIsInteger(digits, nullAllowed = TRUE)
  validateIsInteger(nsmall, nullAllowed = TRUE)
  validateIsLogical(scientific, nullAllowed = TRUE)

  reEnv$formatNumericsDigits <- digits %||% reEnv$formatNumericsDigits
  reEnv$formatNumericsSmall <- nsmall %||% reEnv$formatNumericsSmall
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
setDefaultBins <- function(bins) {
  validateIsNumeric(bins)
  reEnv$defaultBins <- bins
}

#' @title setDefaultStairstep
#' @param stairstep logical defining if stairstep should be plotted by default when aggregation is performed
#' @export
setDefaultStairstep <- function(stairstep) {
  validateIsLogical(stairstep)
  reEnv$defaultStairstep <- stairstep
}
