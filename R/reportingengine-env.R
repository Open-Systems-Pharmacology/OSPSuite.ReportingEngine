#' @title reEnv
#' @description
#' Environment that holds various global variables and settings for the RE library.
#' It is not exported and should not be directly manipulated by other packages.
#' @field packageName Name of the package.
#' This will be used to retrieve information on the package at run time.
#' @field log `Logging` object defining how to record log messages
#' @field defaultLogPrint List of logical values indicating whether to print log messages on console
#' @field formatNumericsDigits Number of digits to display for numeric values in reports
#' @field formatNumericsScientific Logical value indicating whether to use scientific notation for numeric values in reports
#' @field defaultBins Default number of bins to use in histograms
#' @field binUsingQuantiles
#' Logical value indicating whether splitting method for bins
#' `TRUE` uses same number of points in bins through quantiles
#' `FALSE` uses same interval ranges
#' @field defaultStairstep
#' Logical value indicating whether to use `stairstep` lines in range plots
#' @field defaultSimulationSetDescriptor
#' Default descriptor for simulation sets to include in reports
#' @field maximalParametersPerSensitivityPlot
#' Maximum number of parameters to include in a sensitivity plot
#' @field maxWidthPerParameter
#' Maximum number of characters allowed before adding line breaks
#' in sensitivity plot parameter captions.
#' @field maxLinesPerParameter
#' Maximum number of lines allowed when line breaks need to be included
#' in sensitivity plot parameter captions.
#' @field maxWidthPerLegendCaption
#' Maximum number of characters allowed before adding line breaks in legend captions.
#' @field maxLinesPerLegendCaption
#' Maximum number of lines allowed when line breaks need to be included in legend captions.
#' @field autoAxisLimitMargin
#' For DDI Ratio plots,
#' value of the margin to add to the limits of the axis when left undefined
#' @field blankLinesBetweenArtifacts
#' Default number of line breaks to include in reports after inclusion of artifact
#' @field fontScaleFactor
#' Arbitrary scale factor for font size when exporting plot as png to render prettier captions
#' @field workflowWatermarkMessage Default watermark message to print when system is not validated
#' @field residualsHistogramLabel Default ylabel in goodness of fit residuals histogram plots
#' @field demographyHistogramLabel Default ylabel in demography histogram plots
#' @field residualsQQLabel Default ylabel in goodness of fit QQ-plots
#' @field referenceColor Color of reference simulationSet in plots for Population Workflows
#' @field referenceFill Fill color of reference simulationSet in plots for Population Workflows
#' @field defaultErrorbarCapSize Default size of the error bar caps in the plots
#' @field defaultSimulationNumberOfCores default numberOfCores for simulation
#' @field defaultSensitivityAnalysisNumberOfCores default numberOfCores for sensitivity analysis
#' @field defaultVariationRange default parameter variation range for sensitivity analysis
#' @field defaultQuantileVec default quantiles for population sensitivity analysis
#' @field defaultMaxSimulationsPerCore
#' Default value for a scale factor used in a parallel simulation.
#' The product of this scale factor and the number of allowable cores (`allowedCores`)
#' sets the maximum number of simulations that may be run on one core.
#' @field defaultMCRandomSeed
#' Default Random Seed for Monte Carlo sampling when calculating PK Ratios in population workflows
#' @field defaultMCRepetitions
#' Default number of repetitions for Monte Carlo sampling when calculating PK Ratios in population workflows
#' @field defaultPKParametersHeader Default header for PK parameters summary tables in reports
#' @field pkRatioDictionary Dictionary of column names for PK Ratio tables
#' @field ddiRatioListColumnMappings Dictionary of column names for DDI Ratio tables
#' @field ddiRatioSubsetsDictionary  Dictionary of sub-sections names for DDI Ratios
#' @field theme A `Theme` object from the `{tlf}` package defining default properties of the figures
#' @field defaultPlotFormat List of default format when exporting plots
#' @field defaultTimeProfileStatistics
#' A list of functions and labels defining the summary statistics and captions for population time profiles
#' @keywords internal
reEnv <- new.env(parent = emptyenv())
reEnv$packageName <- "ospsuite.reportingengine"
reEnv$log <- Logging$new()
reEnv$defaultLogPrint <- list(
  Error = TRUE,
  Info = TRUE,
  Debug = FALSE
)
reEnv$formatNumericsDigits <- 2
reEnv$formatNumericsScientific <- NA
reEnv$defaultBins <- 11
reEnv$binUsingQuantiles <- TRUE
reEnv$defaultStairstep <- TRUE
reEnv$defaultSimulationSetDescriptor <- ""
reEnv$maximalParametersPerSensitivityPlot <- 25
reEnv$maxWidthPerParameter <- 25
reEnv$maxLinesPerParameter <- 3
reEnv$maxWidthPerLegendCaption <- 50
reEnv$maxLinesPerLegendCaption <- 2
reEnv$autoAxisLimitMargin <- 0.05
reEnv$blankLinesBetweenArtifacts <- 2
reEnv$fontScaleFactor <- 5
reEnv$workflowWatermarkMessage <- "preliminary analysis"
reEnv$residualsHistogramLabel <- "Relative frequency"
reEnv$demographyHistogramLabel <- "Percentage of population [%]"
reEnv$residualsQQLabelX <- "Standard normal quantiles"
reEnv$residualsQQLabelY <- "Quantiles of residuals"
reEnv$referenceColor <- "grey50"
reEnv$referenceFill <- "grey50"
reEnv$defaultErrorbarCapSize <- 3
reEnv$defaultMaxSimulationsPerCore <- 2
reEnv$defaultVariationRange <- 0.1
reEnv$defaultSensitivityAnalysisNumberOfCores <- 1
reEnv$defaultQuantileVec <- c(0.05, 0.5, 0.95)
reEnv$defaultSimulationNumberOfCores <- 1
reEnv$defaultMCRandomSeed <- 123456
reEnv$defaultMCRepetitions <- 1e4
reEnv$defaultPKParametersHeader <- c(
  "N",
  paste0(c(5, 25, 50, 75, 95), "<sup>th</sup> pctl"),
  "Mean", "SD", "Geo Mean", "Geo SD"
)
reEnv$pkRatioDictionary <- list(
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
reEnv$defaultPlotFormat <- list(
  format = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

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
  reThemeFile <- system.file("themes", "template-theme.json", package = "tlf")
  if (!isIncluded(reThemeFile, "")) {
    return(tlf::loadThemeFromJson(reThemeFile))
  }
  # Use extdata, in case the old tlf version is installed and used
  return(tlf::loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))
}
reEnv$theme <- getDefaultRETheme()

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

#' @title reSettingsNames
#' @description
#' Names of the default/global settings stored in `reEnv`.
#' Can be used with `getRESettings()`
#' @import ospsuite.utils
#' @export
#' @family enum helpers
reSettingsNames <- enum(names(reEnv))

# Code chunk to be run only if reEnv is updated
# Saves a sync version of reEnv as an re-env.RData file
# That users can load to get default reporting engine settings
if (FALSE) {
  newEnv <- reEnv
  save("newEnv", file = file.path(system.file(package = "ospsuite.reportingengine"), "extdata", "re-env.RData"))
  rm(list = "newEnv")
}
