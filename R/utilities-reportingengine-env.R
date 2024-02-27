#' @title displayDebugLogs
#' @description `Debug` logs are displayed on R/RStudio console
displayDebugLogs <- function() {
  reEnv$defaultLogPrint$Debug <- TRUE
}

#' @title setDefaultPlotFormat
#' @description Set default plot format to be exported by reporting engine
#' @param format file format of the exported plots. E.g. "png" or "pdf"
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height` included in "in", "cm", "mm", or "px"
#' @param dpi Plot resolution in dots per inch. Caution, font sizes depend on resolution.
#' @export
#' @import ospsuite.utils
#' @examples
#' setDefaultPlotFormat(format = "pdf")
#' setDefaultPlotFormat(width = 16, height = 9, units = "cm", dpi = 300)
#' 
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
  # In such case, Convert width and height back into inches in case of units as pixels
  requireConversion <- all(
    packageVersion("ggplot2")<"3.4",
    isIncluded(units, "px")
  )
  if (requireConversion) {
    units <- "in"
    unitConversionFactor <- grDevices::dev.size("in") / grDevices::dev.size("px")
    reEnv$defaultPlotFormat$width <- reEnv$defaultPlotFormat$width * unitConversionFactor[1]
    reEnv$defaultPlotFormat$height <- reEnv$defaultPlotFormat$height * unitConversionFactor[2]
  }
  reEnv$defaultPlotFormat$units <- units %||% reEnv$defaultPlotFormat$units
  tlf::setDefaultExportParameters(
    format = reEnv$defaultPlotFormat$format,
    width = reEnv$defaultPlotFormat$width,
    height = reEnv$defaultPlotFormat$height,
    units = reEnv$defaultPlotFormat$units,
    dpi = reEnv$defaultPlotFormat$dpi
  )
  return(invisible())
}

#' @title setPlotFormat
#' @description Set default plot format to be exported by reporting engine
#' @inheritParams setDefaultPlotFormat
#' @export
setPlotFormat <- function(format = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL) {
  setDefaultPlotFormat(format, width, height, units, dpi)
}

#' @title setDefaultNumericFormat
#' @description Set default format for numeric values output in reports
#' @param digits Number of significant digits
#' @param scientific Logical defining if numeric format uses a scientific expression
#' @export
#' @import ospsuite.utils
#' @examples
#' setDefaultNumericFormat(digits = 2, scientific = TRUE)
setDefaultNumericFormat <- function(digits = NULL, scientific = NULL) {
  validateIsInteger(digits, nullAllowed = TRUE)
  validateIsLogical(scientific, nullAllowed = TRUE)
  
  reEnv$formatNumericsDigits <- digits %||% reEnv$formatNumericsDigits
  reEnv$formatNumericsScientific <- scientific %||% reEnv$formatNumericsScientific
  return(invisible())
}

#' @title setDefaultTheme
#' @description Set the default plot settings for a workflow
#' @param theme `Theme` object from `tlf` package
#' If `NULL`, the current theme is re-initialized to the reporting engine default
#' @export
#' @import ospsuite.utils
setDefaultTheme <- function(theme = NULL) {
  validateIsOfType(theme, "Theme", nullAllowed = TRUE)
  reEnv$theme <- theme %||% getDefaultRETheme()
  tlf::useTheme(reEnv$theme)
  return(invisible())
}

#' @title setDefaultThemeFromJson
#' @description Set the default plot settings for a workflow from a json file
#' @param jsonFile path to json file that includes `Theme` properties to be loaded
#' @export
setDefaultThemeFromJson <- function(jsonFile) {
  newTheme <- tlf::loadThemeFromJson(jsonFile)
  setDefaultTheme(newTheme)
  return(invisible())
}

#' @title setDefaultBins
#' @param bins number or edges of bins
#' @export
#' @examples
#' setDefaultBins(10)
setDefaultBins <- function(bins) {
  validateIsNumeric(bins)
  reEnv$defaultBins <- bins
  return(invisible())
}

#' @title setDefaultStairstep
#' @param stairstep logical defining if stairstep should be plotted by default when aggregation is performed
#' @export
#' @examples
#' setDefaultStairstep(TRUE)
setDefaultStairstep <- function(stairstep) {
  validateIsLogical(stairstep)
  reEnv$defaultStairstep <- stairstep
  return(invisible())
}

#' @title setDefaultAutoAxisLimitMargin
#' @param margin numeric value between 0 and 1
#' defining the margin of automated axis limits
#' @export
#' @examples
#' setDefaultAutoAxisLimitMargin(0.1)
setDefaultAutoAxisLimitMargin <- function(margin) {
  validateIsNumeric(margin)
  reEnv$autoAxisLimitMargin <- margin
  return(invisible())
}

#' @title getDefaultMCRandomSeed
#' @description Get the default random seed when performing Monte Carlo sampling
#' @return Random seed number
#' @export
#' @examples
#' getDefaultMCRandomSeed()
#'
getDefaultMCRandomSeed <- function() {
  return(reEnv$defaultMCRandomSeed)
}

#' @title setDefaultMCRandomSeed
#' @description Set the default random seed when performing Monte Carlo sampling
#' @param seed Random seed number as an integer
#' @export
#' @examples
#' setDefaultMCRandomSeed(123456)
#'
setDefaultMCRandomSeed <- function(seed) {
  validateIsInteger(seed)
  reEnv$defaultMCRandomSeed <- seed
  return(invisible())
}

#' @title getDefaultMCRepetitions
#' @description Get the default number of repetitions when performing Monte Carlo sampling
#' @return Random seed number
#' @export
#' @examples
#' getDefaultMCRepetitions()
#'
getDefaultMCRepetitions <- function() {
  return(reEnv$defaultMCRepetitions)
}

#' @title setDefaultMCRepetitions
#' @description Set the default number of repetitions when performing Monte Carlo sampling
#' @param n Number of repetitions
#' @export
#' @examples
#' setDefaultMCRepetitions(1e4)
#'
setDefaultMCRepetitions <- function(n) {
  validateIsInteger(n)
  reEnv$defaultMCRepetitions <- n
  return(invisible())
}

#' @title setDefaultTimeProfileStatistics
#' @description Set default statistics used in population time profiles and residuals plots
#' @param statisticsType Name of statistics type as defined in enum `StatisticsTypes`
#' @param y Function or function name for middle values statistics
#' @param ymin Function or function name for min values statistics
#' @param ymax Function or function name for max values statistics
#' @param yCaption Legend caption for middle values statistics
#' @param rangeCaption Legend caption for range values statistics
#' @export
#' @examples
#' \dontrun{
#' # Set the default statistics as geometric mean
#' setDefaultTimeProfileStatistics(statisticsType = StatisticsTypes$`Geometric mean`)
#'
#' # Set the default legend caption displayed for range
#' setDefaultTimeProfileStatistics(rangeCaption = "90% population range")
#' }
#'
setDefaultTimeProfileStatistics <- function(statisticsType = NULL,
                                            y = NULL,
                                            ymin = NULL,
                                            ymax = NULL,
                                            yCaption = NULL,
                                            rangeCaption = NULL) {
  validateIsIncluded(statisticsType, StatisticsTypes, nullAllowed = TRUE)
  # Allow user to enter the function directly
  validateIsOfType(y, c("character", "closure"), nullAllowed = TRUE)
  validateIsOfType(ymin, c("character", "closure"), nullAllowed = TRUE)
  validateIsOfType(ymax, c("character", "closure"), nullAllowed = TRUE)
  
  if (!isEmpty(statisticsType)) {
    reEnv$defaultTimeProfileStatistics <- getStatisticsFromType(statisticsType)
  }
  # Assign variables to reEnv only if defined
  eval(parseVariableToObject(
    objectName = "reEnv$defaultTimeProfileStatistics",
    variableName = c("y", "ymin", "ymax", "yCaption", "rangeCaption"),
    keepIfNull = TRUE
  ))
  
  return(invisible())
}

#' @title getRESettings
#' @description
#' Get properties from name of the default/global settings stored in `reEnv`.
#' @param settingName setting name as defined in enum `reSettingsNames`
#' @export
getRESettings <- function(settingName) {
  validateEnumValue(settingName, enum = reSettingsNames, nullAllowed = FALSE)
  obj <- reEnv[[settingName]]
  
  return(list(
    Name = settingName,
    Class = class(obj),
    Value = obj
  ))
}

#' @title saveRESettings
#' Save the current reporting engine global settings in a `.RData` file
#' @param file `.RData` file containing the settings
#' @export
saveRESettings <- function(file) {
  validateIsFileExtension(file, "RData")
  newEnv <- reEnv
  save("newEnv", file = file)
}

#' @title loadRESettings
#' @description
#' Load reporting engine global settings from a file
#' @param file `.RData` file containing the settings
#' @export
loadRESettings <- function(file) {
  validateIsFileExtension(file, "RData")
  load(file = file)
  for(fieldNames in names(newEnv)){
    reEnv[[fieldNames]] <- newEnv[[fieldNames]]
  }
  return(invisible())
}

#' @title resetRESettingsToDefault
#' @description
#' Reset the global settings stored in `reEnv` to default values defined by the package.
#' @export
resetRESettingsToDefault <- function() {
  loadRESettings(system.file("extdata", "re-env.RData", package = "ospsuite.reportingengine"))
  # Set the default tlf theme to RE Default values
  setDefaultTheme()
  return(invisible())
}