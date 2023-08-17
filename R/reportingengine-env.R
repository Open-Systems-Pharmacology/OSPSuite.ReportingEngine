# Environment that holds various global variables and settings for ospsuite.reportingengine package,
# It is not exported and should not be directly manipulated by other packages.
reEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
reEnv$packageName <- "ospsuite.reportingengine"

# Create global logging system
reEnv$log <- Logging$new()
reEnv$defaultLogPrint <- list(
  Error = TRUE,
  Info = TRUE,
  Debug = FALSE
)

#' @title displayDebugLogs
#' @description `Debug` logs are displayed on R/RStudio console
displayDebugLogs <- function() {
  reEnv$defaultLogPrint$Debug <- TRUE
}

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

reEnv$maxWidthPerLegendCaption <- 50
reEnv$maxLinesPerLegendCaption <- 2
# If plot limits are left undefined, an auto margin is added
reEnv$autoAxisLimitMargin <- 0.05

reEnv$blankLinesBetweenArtifacts <- 2

# Default plot properties
reEnv$theme$background$legendPosition <- tlf::LegendPositions$outsideTop
# reEnv$defaultLegendPosition <- tlf::LegendPositions$outsideTop

reEnv$defaultPlotFormat <- list(
  format = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

# Scale factor for font size when exporting plot as png
reEnv$fontScaleFactor <- 2

reEnv$workflowWatermarkMessage <- "preliminary analysis"
reEnv$residualsHistogramLabel <- "Relative frequency"
reEnv$demographyHistogramLabel <- "Percentage of population [%]"
reEnv$residualsQQLabel <- "Quantiles of residuals"
reEnv$referenceColor <- "grey50"
reEnv$referenceFill <- "grey50"

# Default values for pk parameter measure header
reEnv$defaultPKParametersHeader <- c(
  "N",
  paste0(c(5, 25, 50, 75, 95), "<sup>th</sup> pctl"),
  "Mean", "SD", "Geo Mean", "Geo SD"
)

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

reEnv$ddiRatioListColumnMappings <- list(
  id = "ID",
  studyId = "Study ID",
  mechanism = "Mechanism",
  perpetrator = "Perpetrator",
  routePerpetrator = "Route Perpetrator",
  victim = "Victim",
  routeVictim = "Route Victim",
  dose = "Dose",
  doseUnit = "Dose Unit",
  description = "Description"
)


reEnv$ddiRatioSubsetsDictionary <- list(
  "Reversible_Inhibition" = "Reversible Inhibition",
  "Mechanism_based_Inactivation" = "Mechanism-based Inactivation"
)


# Default value for a scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.
reEnv$defaultMaxSimulationsPerCore <- 2

# Default values for Monte Carlo sampling when calculating PK Ratios
reEnv$defaultMCRandomSeed <- 123456
reEnv$defaultMCRepetitions <- 1e4

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
#' @importFrom ospsuite.utils %||%
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

#' @title ApplicationRanges
#' @description
#' Keys of reported ranges when simulation includes multiple applications
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists available Application Ranges
#' ApplicationRanges
#'
ApplicationRanges <- enum(c("total", "firstApplication", "lastApplication"))

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

#' @title getDefaultRETheme
#' @description Get default plot settings for RE package
#' @return A `Theme` object from `tlf` package
#' @export
getDefaultRETheme <- function() {
  # Get reporting engine theme from its json file properties
  reThemeFile <- system.file("extdata", "re-theme.json", package = "ospsuite.reportingengine")
  if (!isIncluded(reThemeFile, "")) {
    return(tlf::loadThemeFromJson(reThemeFile))
  }
  # If not found, e.g. before the package is built, use a tlf template theme
  # TODO use themes instead of extdata in later versions of tlf
  return(tlf::loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))
}

# Initialize a theme for reporting engine
# This theme is updated every time a new Workflow object is loaded
reEnv$theme <- getDefaultRETheme()

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
}

#' @title setDefaultThemeFromJson
#' @description Set the default plot settings for a workflow from a json file
#' @param jsonFile path to json file that includes `Theme` properties to be loaded
#' @export
setDefaultThemeFromJson <- function(jsonFile) {
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

#' @title setDefaultAutoAxisLimitMargin
#' @param margin numeric value between 0 and 1
#' defining the margin of automated axis limits
#' @export
#' @examples
#' setDefaultAutoAxisLimitMargin(0.1)
setDefaultAutoAxisLimitMargin <- function(margin) {
  validateIsNumeric(margin)
  reEnv$autoAxisLimitMargin <- margin
}

#' @title StatisticsTypes
#' @description List of available statistic types summarizing data for time profile plots
#' \itemize{
#' \item `"2.5th-97.5th Percentiles"` summarizes data using median, 2.5th and 97.5th percentiles
#' \item `"5th-95th Percentiles"` summarizes data using median, 5th and 95th percentiles
#' \item `"10th-90th Percentiles"` summarizes data using median, 10th and 90th percentiles
#' \item `"Geometric mean"` summarizes data using geometric mean and mean */ geometric standard deviation
#' \item `"Arithmetic mean"` summarizes data using arithmetic mean and mean +/- standard deviation
#' }
#'
#' @export
#' @family enum helpers
StatisticsTypes <- enum(c(
  "2.5th-97.5th Percentiles",
  "5th-95th Percentiles",
  "10th-90th Percentiles",
  "Geometric mean",
  "Arithmetic mean"
))


#' @title getStatisticsFromType
#' @description Get statistics
#' @param statisticsType Statistics summarizing time profile simulated data
#' as defined by helper enum `StatisticsType`
#' @return A list including `y`, `ymin` and `ymax` summary statistics as well as their `caption`
#' @export
#' @examples
#' \dontrun{
#' getStatisticsFromType(StatisticsTypes$`Arithmetic mean`)
#' }
#'
getStatisticsFromType <- function(statisticsType) {
  validateIsIncluded(statisticsType, StatisticsTypes)
  if (isIncluded(statisticsType, StatisticsTypes$`2.5th-97.5th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile2.5%`,
      ymax = tlf::tlfStatFunctions$`Percentile97.5%`,
      yCaption = "median",
      # The unicode characters below are superscript th
      rangeCaption = "[2.5\u1d57\u02b0-97.5\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`5th-95th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile5%`,
      ymax = tlf::tlfStatFunctions$`Percentile95%`,
      yCaption = "median",
      rangeCaption = "[5\u1d57\u02b0-95\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`10th-90th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile10%`,
      ymax = tlf::tlfStatFunctions$`Percentile90%`,
      yCaption = "median",
      rangeCaption = "[10\u1d57\u02b0-90\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`Arithmetic mean`)) {
    return(list(
      y = tlf::tlfStatFunctions$mean,
      ymin = tlf::tlfStatFunctions$`mean-sd`,
      ymax = tlf::tlfStatFunctions$`mean+sd`,
      yCaption = "arithmetic mean",
      # The unicode character below is +/- symbol
      rangeCaption = "mean \u00b1 SD range"
    ))
  }
  # TODO : define geometric mean in tlf !
  return(list(
    y = "geomean",
    ymin = "geomeanDividedBySD",
    ymax = "geomeanMultipliedBySD",
    yCaption = "geometric mean",
    # The unicode character below is supposed to be */ symbol
    rangeCaption = "mean */ geometric SD range"
  ))
}

reEnv$defaultTimeProfileStatistics <- getStatisticsFromType(StatisticsTypes$`5th-95th Percentiles`)

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
