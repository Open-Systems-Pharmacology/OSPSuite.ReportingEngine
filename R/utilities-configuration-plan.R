# Helper functions to translate configuration plan values/fields to tlf/opsuite.reportingengine

#' @title ConfigurationPlots
#' @description Enum defining possible plots defined in configuration plan
ConfigurationPlots <- ospsuite::enum(c("TimeProfile", "GOFMergedPlots", "ComparisonTimeProfilePlots", "PKRatioPlots", "DDIRatioPlots"))

#' @title ConfigurationPlotSettings
#' @description Enum defining possible plot settings fields defined in configuration plan
ConfigurationPlotSettings <- ospsuite::enum(c("ChartWidth", "ChartHeight", "Fonts"))

#' @title ConfigurationFontsFields
#' @description Enum defining possible fonts fields defined in configuration plan
#' Note that some fields might not be used/converted by the current `opsuite.reportingengine` or `tlf` version (e.g. FontFamilyName)
ConfigurationFontsFields <- c("AxisSize", "LegendSize", "OriginSize", "FontFamilyName", "WatermarkSize")
# TODO the value of watermark font size might need a scaling

#' @title ConfigurationScales
#' @description List defining scale following tlf nomenclature from a Scaling field of configuration plan
#' @import tlf
ConfigurationScales <- list(
  linear = tlf::Scaling$lin,
  log = tlf::Scaling$log
)

#' @title ConfigurationShapes
#' @description List defining shape following tlf nomenclature from a Symbol field of configuration plan
#' @import tlf
ConfigurationShapes <- list(
  none = tlf::Shapes$blank,
  circle = 1,
  dot = tlf::Shapes$dot,
  square = tlf::Shapes$square,
  diamond = 5,
  asterisk = 8,
  cross = 4,
  point = 16
)

#' @title ConfigurationLinetypes
#' @description List defining linetype following tlf nomenclature from a LineStyle field of configuration plan
#' @import tlf
ConfigurationLinetypes <- list(
  none = tlf::Linetypes$blank,
  solid = tlf::Linetypes$solid,
  dash = tlf::Linetypes$dashed
)

#' @title getAxesPropertiesForTimeProfiles
#' @description Identify and convert properties from `Axes` field of configuration plan
#' @param configurationPlanAxes list of axes properties defined in `Axes` field of configuration plan
#' @return A list of properties for identified `x` and `y` axes that follows `tlf` package nomenclature
getAxesPropertiesForTimeProfiles <- function(configurationPlanAxes) {
  # Identify X and Y Axes
  # TODO handle Y2 axis
  xAxis <- configurationPlanAxes[sapply(configurationPlanAxes, function(axis) {
    axis$Type == "X"
  })][[1]]
  yAxis <- configurationPlanAxes[sapply(configurationPlanAxes, function(axis) {
    axis$Type == "Y"
  })][[1]]

  xAxis <- list(
    dimension = xAxis$Dimension, unit = xAxis$Unit,
    min = xAxis$Min, max = xAxis$Max, scale = tlfScale(xAxis$Scaling)
  )
  yAxis <- list(
    dimension = yAxis$Dimension, unit = yAxis$Unit,
    min = yAxis$Min, max = yAxis$Max, scale = tlfScale(yAxis$Scaling)
  )
  return(list(x = xAxis, y = yAxis))
}

#' @title updatePlotAxes
#' @description Update the axes of a plot object based on the identified axes properties
#' @param plotObject A ggplot object
#' @param axesProperties list of axes properties obtained from `getAxesForTimeProfiles`
#' @return A ggplot object
#' @import tlf
updatePlotAxes <- function(plotObject, axesProperties) {
  plotObject <- tlf::setPlotLabels(plotObject,
    xlabel = tlf::getLabelWithUnit(axesProperties$x$dimension, axesProperties$x$unit),
    ylabel = tlf::getLabelWithUnit(axesProperties$y$dimension, axesProperties$y$unit)
  )

  try(plotObject <- tlf::setXAxis(plotObject, scale = axesProperties$x$scale, limits = c(axesProperties$x$min, axesProperties$x$max)))
  try(plotObject <- tlf::setYAxis(plotObject, scale = axesProperties$y$scale, limits = c(axesProperties$y$min, axesProperties$y$max)))
  plotObject <- tlf::setLegendPosition(plotObject, position = tlf::LegendPositions$outsideTop)
  return(plotObject)
}

#' @title getCurvePropertiesForTimeProfiles
#' @description Identify and convert properties from `Curves` field of configuration plan
#' @param configurationPlanCurve list of properties defined in `Curves` field of configuration plan
#' @param simulation A `Simulation` object from `ospsuite` package
#' that includes required information to identify and convert the data requested from `configurationPlanCurve` properties
#' @param simulationResults A `SimulationResults` object from `ospsuite` package
#' that includes the data requested from `configurationPlanCurve` properties
#' @param molWeight molecular weight for observed data
#' @param axesProperties list of axes properties obtained from `getAxesForTimeProfiles`
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find observed data
#' @param logFolder folder where the logs are saved
#' @return A list of data and properties to be plotted and that follows `tlf` package nomenclature
#' @import tlf
getCurvePropertiesForTimeProfiles <- function(configurationPlanCurve,
                                              simulation,
                                              simulationResults,
                                              molWeight,
                                              axesProperties,
                                              configurationPlan,
                                              logFolder) {
  # Check if curve is on first or seconf Y axis
  curveOnSecondAxis <- isTRUE(configurationPlanCurve$CurveOptions$yAxisType == "Y2")
  # TODO handle curve on Y2
  if (curveOnSecondAxis) {
    return()
  }

  pathArray <- ospsuite::toPathArray(configurationPlanCurve$Y)

  # Observed Data
  if (pathArray[2] %in% "ObservedData") {
    observedResults <- getObservedDataFromConfigurationPlan(pathArray[1], configurationPlan, logFolder)

    time <- ospsuite::toUnit(
      quantityOrDimension = "Time",
      values = as.numeric(observedResults$data[, 1]),
      targetUnit = axesProperties$x$unit,
      sourceUnit = observedResults$metaData$time$unit
    )
    outputValues <- ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(observedResults$metaData$output$unit),
      values = observedResults$data[, 2],
      targetUnit = axesProperties$y$unit,
      sourceUnit = observedResults$metaData$output$unit,
      molWeight = molWeight
    )

    outputError <- NULL
    if (!isOfLength(observedResults$metaData$error, 0)) {
      # No unit means that error is geometric
      outputError$ymin <- outputValues / observedResults$data[, 3]
      outputError$ymax <- outputValues * observedResults$data[, 3]

      if (!isIncluded(observedResults$metaData$error$unit, "")) {
        outputError$ymin <- outputValues - ospsuite::toUnit(
          ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
          observedResults$data[, 3],
          axesProperties$y$unit,
          sourceUnit = observedResults$metaData$error$unit,
          molWeight = molWeight
        )
        outputError$ymax <- outputValues + ospsuite::toUnit(
          ospsuite::getDimensionForUnit(observedResults$metaData$error$unit),
          observedResults$data[, 3],
          axesProperties$y$unit,
          sourceUnit = observedResults$metaData$error$unit,
          molWeight = molWeight
        )
      }
    }

    outputCurve <- list(
      x = time,
      y = outputValues,
      error = outputError,
      caption = configurationPlanCurve$Name,
      color = configurationPlanCurve$CurveOptions$Color,
      linetype = tlfLinetype(configurationPlanCurve$CurveOptions$LineStyle),
      shape = tlfShape(configurationPlanCurve$CurveOptions$Symbol),
      size = configurationPlanCurve$CurveOptions$Size,
      id = configurationPlanCurve$CurveOptions$LegendIndex,
      secondAxis = curveOnSecondAxis,
      molWeight = molWeight
    )
    return(outputCurve)
  }
  # Remove simulation name from path
  outputPath <- ospsuite::toPathString(pathArray[-1])
  # Get and convert output path values into display unit
  simulationQuantity <- ospsuite::getQuantity(outputPath, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = outputPath)
  molWeight <- simulation$molWeightFor(outputPath)

  time <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesProperties$x$unit
  )

  outputValues <- ospsuite::toUnit(simulationQuantity,
    simulationPathResults$data[, outputPath],
    axesProperties$y$unit,
    molWeight = molWeight
  )

  # If CurveOptions or one of its field is not defined,
  # the corresponding result will be NULL and default properties will be applied instead
  # If defined, they'll overwrite the default
  outputCurve <- list(
    x = time,
    y = outputValues,
    caption = configurationPlanCurve$Name,
    color = configurationPlanCurve$CurveOptions$Color,
    linetype = tlfLinetype(configurationPlanCurve$CurveOptions$LineStyle),
    shape = tlfShape(configurationPlanCurve$CurveOptions$Symbol),
    size = configurationPlanCurve$CurveOptions$Size,
    id = configurationPlanCurve$CurveOptions$LegendIndex,
    secondAxis = curveOnSecondAxis,
    molWeight = molWeight
  )

  return(outputCurve)
}

#' @title tlfLinetype
#' @description Translate value of `LineStyle` property from configuration plan into tlf `linetype`
#' @param configurationLinetype Value of the configuration plan plot property
#' @return `linetype` values that follows `tlf` package nomenclature
#' @import tlf
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
tlfScale <- function(configurationScale) {
  # Unknown or NULL value will translate as NULL
  # which will lead to use default behaviour
  if (isOfLength(configurationScale, 0)) {
    return()
  }
  # tolower is used to ensure that there is no issue with caps from field values
  ConfigurationScales[[tolower(configurationScale)]]
}
