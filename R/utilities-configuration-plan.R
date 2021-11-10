# Helper functions to translate configuration plan values/fields to tlf/opsuite.reportingengine

#' @title ConfigurationPlots
#' @description Enum defining possible plots defined in configuration plan
#' @keywords internal
ConfigurationPlots <- ospsuite::enum(c("TimeProfile", "GOFMergedPlots", "ComparisonTimeProfilePlots", "PKRatioPlots", "DDIRatioPlots"))

#' @title ConfigurationPlotSettings
#' @description Enum defining possible plot settings fields defined in configuration plan
#' @keywords internal
ConfigurationPlotSettings <- ospsuite::enum(c("ChartWidth", "ChartHeight", "Fonts"))

#' @title ConfigurationFontsFields
#' @description Enum defining possible fonts fields defined in configuration plan
#' Note that some fields might not be used/converted by the current `opsuite.reportingengine` or `tlf` version (e.g. FontFamilyName)
#' @keywords internal
ConfigurationFontsFields <- c("AxisSize", "LegendSize", "OriginSize", "FontFamilyName", "WatermarkSize")

#' @title ConfigurationScales
#' @description List defining scale following tlf nomenclature from a Scaling field of configuration plan
#' @import tlf
#' @keywords internal
ConfigurationScales <- list(
  linear = tlf::Scaling$lin,
  log = tlf::Scaling$log
)

#' @title ConfigurationShapes
#' @description List defining shape following tlf nomenclature from a Symbol field of configuration plan
#' @import tlf
#' @keywords internal
ConfigurationShapes <- list(
  none = tlf::Shapes$blank,
  circle = tlf::Shapes$circle,
  dot = tlf::Shapes$dot,
  square = tlf::Shapes$square,
  diamond = tlf::Shapes$diamond,
  asterisk = tlf::Shapes$star,
  cross = tlf::Shapes$cross,
  point = tlf::Shapes$dot
)

#' @title ConfigurationLinetypes
#' @description List defining linetype following tlf nomenclature from a LineStyle field of configuration plan
#' @import tlf
#' @keywords internal
ConfigurationLinetypes <- list(
  none = tlf::Linetypes$blank,
  solid = tlf::Linetypes$solid,
  dash = tlf::Linetypes$dashed
)

#' @title getAxesProperties
#' @description Get identified axes properties from global field `AxesSettings` of configuration plan or
#' from plot specific field `Axes`. The identified properties are directly compatible with `tlf` package nomenclature
#' @param axesSettings list of axes properties defined in `Axes` field of configuration plan
#' @return A list of properties for axes identified for `x`, `y` and `y2` axes.
#' The identified properties are directly compatible with `tlf` package nomenclature
#' @keywords internal
getAxesProperties <- function(axesSettings) {
  # Hanlde when properties are left undefined globally or locally
  if (isOfLength(axesSettings, 0)) {
    return(NULL)
  }
  # Get axes types for identification of X, Y and Y2 axes
  axisTypes <- sapply(axesSettings, function(axis) {
    axis$Type
  })

  xAxisIndex <- which(axisTypes %in% "X")
  yAxisIndex <- which(axisTypes %in% "Y")
  y2AxisIndex <- which(axisTypes %in% "Y2")

  # X and Y axes are mandatory, while Y2 is not
  validateIsOfLength(xAxisIndex, 1)
  validateIsOfLength(yAxisIndex, 1)
  xAxis <- axesSettings[[xAxisIndex]]
  yAxis <- axesSettings[[yAxisIndex]]
  xAxis <- list(
    dimension = xAxis$Dimension, unit = xAxis$Unit,
    min = xAxis$Min, max = xAxis$Max, scale = tlfScale(xAxis$Scaling)
  )
  yAxis <- list(
    dimension = yAxis$Dimension, unit = yAxis$Unit,
    min = yAxis$Min, max = yAxis$Max, scale = tlfScale(yAxis$Scaling)
  )

  y2Axis <- NULL
  if (isOfLength(y2AxisIndex, 1)) {
    y2Axis <- axesSettings[[y2AxisIndex]]
    y2Axis <- list(
      dimension = y2Axis$Dimension, unit = y2Axis$Unit,
      min = y2Axis$Min, max = y2Axis$Max, scale = tlfScale(y2Axis$Scaling)
    )
  }
  return(list(x = xAxis, y = yAxis, y2 = y2Axis))
}

#' @title updatePlotAxes
#' @description Update the axes of a plot object based on the identified axes properties
#' @param plotObject A ggplot object
#' @param axesProperties list of axes properties obtained from `getAxesForTimeProfiles`
#' @return A ggplot object
#' @import tlf
#' @keywords internal
updatePlotAxes <- function(plotObject, axesProperties) {
  plotObject <- tlf::setPlotLabels(plotObject,
    xlabel = tlf::getLabelWithUnit(displayDimension(axesProperties$x$dimension), axesProperties$x$unit),
    ylabel = tlf::getLabelWithUnit(displayDimension(axesProperties$y$dimension), axesProperties$y$unit)
  )

  try({
    plotObject <- tlf::setXAxis(plotObject, scale = axesProperties$x$scale, limits = c(axesProperties$x$min, axesProperties$x$max))
  })
  try({
    plotObject <- tlf::setYAxis(plotObject, scale = axesProperties$y$scale, limits = c(axesProperties$y$min, axesProperties$y$max))
  })
  plotObject <- tlf::setLegendPosition(plotObject, position = tlf::LegendPositions$outsideTop)
  return(plotObject)
}

#' @title tlfLinetype
#' @description Translate value of `LineStyle` property from configuration plan into tlf `linetype`
#' @param configurationLinetype Value of the configuration plan plot property
#' @return `linetype` values that follows `tlf` package nomenclature
#' @import tlf
#' @keywords internal
tlfLinetype <- function(configurationLinetype) {
  # Unknown or NULL value will translate as NULL
  # which will lead to use default behaviour
  if (isOfLength(configurationLinetype, 0)) {
    return()
  }
  # tolower is used to ensure that there is no issue with caps from field values
  ConfigurationLinetypes[[tolower(configurationLinetype)]]
}

#' @title tlfShape
#' @description Translate value of `Symbol` property from configuration plan into tlf `shape`
#' @param configurationShape Value of the configuration plan plot property
#' @return `shape` values that follows `tlf` package nomenclature
#' @import tlf
#' @keywords internal
tlfShape <- function(configurationShape) {
  # Unknown or NULL value will translate as NULL
  # which will lead to use default behaviour
  if (isOfLength(configurationShape, 0)) {
    return()
  }
  # tolower is used to ensure that there is no issue with caps from field values
  ConfigurationShapes[[tolower(configurationShape)]]
}

#' @title tlfScale
#' @description Translate value of `Scaling` property from configuration plan into tlf `scale`
#' @param configurationScale Value of the configuration plan plot property
#' @return `scale` values that follows `tlf` package nomenclature
#' @import tlf
#' @keywords internal
tlfScale <- function(configurationScale) {
  # Unknown or NULL value will translate as NULL
  # which will lead to use default behaviour
  if (isOfLength(configurationScale, 0)) {
    return()
  }
  # tolower is used to ensure that there is no issue with caps from field values
  ConfigurationScales[[tolower(configurationScale)]]
}

#' @title getCompoundNameFromPath
#' @description
#' Get the compound name from a configuration plan quantity path
#' @param path A quantitiy path from the configuration plan
#' For instance, "S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)"
#' or "Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc"
#' @return A string corresponding to the compound name of a configuration plan quantity path
#' @import ospsuite
#' @examples
#' \dontrun{
#' getCompoundNameFromPath("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)")
#' # > "Midazolam"
#' getCompoundNameFromPath("Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc")
#' # > "Rifampin"
#' }
#' @keywords internal
getCompoundNameFromPath <- function(path) {
  pathArray <- ospsuite::toPathArray(path)
  # As shown in the doc examples, pathArray is assumed to include the compound name as before the last value
  compoundName <- utils::head(utils::tail(pathArray, 2), 1)
  return(compoundName)
}

#' @title getMolWeightForCompound
#' @description
#' Get molecular weight in base unit for a requested compound in a simulation
#' @param compoundName Name of a compound e.g. "Midazolam"
#' @param simulation A `Simulation` object
#' @return molecular weight in base unit or NA if not found
#' @export
getMolWeightForCompound <- function(compoundName, simulation) {
  # In the current version, getAllMoleculePathsIn is faster and lighter than getAllMoleculesMatching
  # since only a path name for the molecule is necessary
  allMoleculePaths <- ospsuite::getAllMoleculePathsIn(simulation)
  pathForCompoundName <- utils::head(allMoleculePaths[grepl(compoundName, allMoleculePaths)], 1)
  # When compound is not found in simulation, return NA
  if (isOfLength(pathForCompoundName, 0)) {
    return(NA)
  }
  return(simulation$molWeightFor(pathForCompoundName))
}

getPlotConfigurationFromPlan <- function(plotProperties) {
  plotConfiguration <- tlf::PlotConfiguration$new()
  # Get properties from FontAndSize
  fonts <- plotProperties$FontAndSize$Fonts
  plotConfiguration$labels$title$font$size <- fonts$TitleSize %||% plotConfiguration$labels$title$font$size
  plotConfiguration$labels$subtitle$font$size <- fonts$DescriptionSize %||% plotConfiguration$labels$subtitle$font$size
  plotConfiguration$labels$xlabel$font$size <- fonts$AxisSize %||% plotConfiguration$labels$xlabel$font$size
  plotConfiguration$labels$ylabel$font$size <- fonts$AxisSize %||% plotConfiguration$labels$ylabel$font$size
  plotConfiguration$xAxis$font$size <- fonts$AxisSize %||% plotConfiguration$xAxis$font$size
  plotConfiguration$yAxis$font$size <- fonts$AxisSize %||% plotConfiguration$yAxis$font$size
  plotConfiguration$legend$font$size <- fonts$LegendSize %||% plotConfiguration$legend$font$size
  plotConfiguration$background$watermark$font$size <- fonts$WatermarkSize %||% plotConfiguration$background$watermark$font$size

  # Get size of exported plot
  plotConfiguration$export$units <- reEnv$defaultPlotFormat$units
  plotConfiguration$export$width <- reEnv$defaultPlotFormat$width
  plotConfiguration$export$height <- reEnv$defaultPlotFormat$height
  # If chart size is defined, it is in pixel and updated accordingly
  unitConversionFactor <- grDevices::dev.size("in") / grDevices::dev.size("px")
  if (!isOfLength(plotProperties$FontAndSize$ChartWidth, 0)) {
    plotConfiguration$export$units <- "in"
    plotConfiguration$export$width <- plotProperties$FontAndSize$ChartWidth * unitConversionFactor[1]
  }
  if (!isOfLength(plotProperties$FontAndSize$ChartHeight, 0)) {
    plotConfiguration$export$units <- "in"
    plotConfiguration$export$height <- plotProperties$FontAndSize$ChartHeight * unitConversionFactor[2]
  }
  return(plotConfiguration)
}
