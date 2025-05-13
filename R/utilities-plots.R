# Set default legend position to outside top
reDefaultLegendPosition <- tlf::LegendPositions$outsideTop
tlf::setDefaultLegendPosition(reDefaultLegendPosition)

#' @title AggregationConfiguration
#' @description To be deprecated:
#' Aggregation default properties  (which functions and their captions).
#' @field functions list of `middle`, `ymin` and `ymax` functions for aggregation
#' @field names list of legend captions for `middle` and `range` from aggregation
#' @field bins default number of bins in plots
#' @field binUsingQuantiles logical to choose a binning based on the quantiles rather than on a constant interval width
#' @export
AggregationConfiguration <- list(
  functions = list(
    middle = function(x) {
      median(x, na.rm = TRUE)
    },
    ymin = function(x) {
      as.numeric(quantile(x, probs = 0.05, na.rm = TRUE))
    },
    ymax = function(x) {
      as.numeric(quantile(x, probs = 0.95, na.rm = TRUE))
    }
  ),
  names = list(
    middle = "median",
    range = "[5-95\u1d57\u02b0] percentiles"
  ),
  bins = 11,
  binUsingQuantiles = TRUE
)

displayDimension <- function(dimension) {
  if (isIncluded(dimension, c(ospsuite::ospDimensions$`Concentration (mass)`, ospsuite::ospDimensions$`Concentration (molar)`))) {
    return("Concentration")
  }
  return(dimension)
}

#' @title autoAxesLimits
#' @description Defines auto axis limits
#' @param x Values from which the limits are calculated
#' @param scale Name of the scale of the axis
#' Use helper enum `Scaling` from `tlf` package to find scales.
#' @return A list of units for goodness of fit results
#' @keywords internal
autoAxesLimits <- function(x, scale = tlf::Scaling$lin) {
  # Filter negative data if scale is log using using which in case of NAs
  if (isIncluded(scale, "log")) {
    x <- x[which(x > 0)]
    if (isEmpty(x)) {
      return()
    }
  }
  minX <- min(x, na.rm = TRUE)
  maxX <- max(x, na.rm = TRUE)
  minX[minX < 0] <- (1 + reEnv$autoAxisLimitMargin) * minX
  minX[minX > 0] <- (1 - reEnv$autoAxisLimitMargin) * minX
  maxX[maxX < 0] <- (1 - reEnv$autoAxisLimitMargin) * maxX
  maxX[maxX < 0] <- (1 + reEnv$autoAxisLimitMargin) * maxX
  if (!isIncluded(scale, "log")) {
    return(c(minX, maxX))
  }
  # For log plots,
  # wider range for pretty axes limits
  if ((log10(maxX) - log10(minX)) > 1) {
    return(c(minX, maxX))
  }
  return(c(minX / 2, maxX * 2))
}

#' @title autoAxesTicksFromLimits
#' @description Defines auto axis ticks from limits for log scale plots.
#' For wide range, log tick labels display every factor of 10
#' For small range, log tick labels displayed at 1,2,5 of every factor of 10
#' @param limits Min and max values of axis range
#' @return Numeric values of ticks to display
#' @keywords internal
autoAxesTicksFromLimits <- function(limits) {
  minLogRange <- log10(min(limits))
  maxLogRange <- log10(max(limits))
  logTicks <- seq(floor(minLogRange), ceiling(maxLogRange))
  logRange <- maxLogRange - minLogRange
  # If range is wide enough, use one tick every factor 10
  if (logRange > 1) {
    return(10^logTicks)
  }
  return(rep(c(1, 2, 5), length(logTicks)) * 10^rep(logTicks, each = 3))
}

#' @title getTimeTicksFromUnit
#' @description Defines auto time ticks from time unit and time values
#' @param unit A time unit as defined in `ospsuite::ospUnits$Time`
#' @param timeValues Numeric values used by the data
#' @param maxTicks Maximum number of ticks allowed
#' @return List of `ticks` and their `ticklabels`
#' @keywords internal
getTimeTicksFromUnit <- function(unit, timeValues = NULL, maxTicks = 10) {
  if (isEmpty(timeValues)) {
    return()
  }
  minTime <- floor(min(0, as.numeric(timeValues), na.rm = TRUE))
  maxTime <- ceiling(max(as.numeric(timeValues), na.rm = TRUE))

  # For undefined ticking of units, assume major tick every 10 units (eg. 10 seconds)
  majorTickStep <- 10
  # For undefined ticking of units, assume minor tick every 1 unit (eg. 1 seconds)
  minorTickStep <- 1

  if (isIncluded(unit, ospsuite::ospUnits$Time$h)) {
    # Major ticks every 6 hours
    majorTickStep <- 6
  }
  if (isIncluded(unit, ospsuite::ospUnits$Time$`day(s)`)) {
    # Major ticks every 7 days
    majorTickStep <- 7
  }
  if (isIncluded(unit, ospsuite::ospUnits$Time$`week(s)`)) {
    # Major ticks every 4 weeks
    majorTickStep <- 4
  }
  if (isIncluded(unit, ospsuite::ospUnits$Time$`month(s)`)) {
    # Major ticks every 6 months
    majorTickStep <- 6
  }

  # Increase tick step to get ticks below max number of ticks
  # To make it prettier, factor will be an integer
  numberOfTicks <- floor((maxTime - minTime) / majorTickStep) + 1
  tickScaleFactor <- ceiling(numberOfTicks / maxTicks)

  minorTicks <- seq(minTime, maxTime, tickScaleFactor * minorTickStep)
  majorTicks <- seq(minTime, maxTime, tickScaleFactor * majorTickStep)
  # In case there are not enough major ticks due to short simulation time
  if (length(majorTicks) <= 3) {
    majorTicks <- minorTicks
  }
  ticklabels <- as.character(minorTicks)
  ticklabels[!(minorTicks %in% majorTicks)] <- ""

  timeTicks <- list(
    ticks = minorTicks,
    ticklabels = ticklabels
  )
  return(timeTicks)
}

#' @title updatePlotConfigurationTimeTicks
#' @description Update time ticks based on selected time unit in `PlotConfiguration` objects
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return A `PlotConfiguration` object
#' @keywords internal
updatePlotConfigurationTimeTicks <- function(data, metaData, dataMapping, plotConfiguration) {
  timeValues <- data[, dataMapping$x]
  timeUnit <- metaData[[dataMapping$x]]$unit
  timeTicks <- getTimeTicksFromUnit(timeUnit, timeValues)

  plotConfiguration$xAxis$ticks <- timeTicks$ticks
  plotConfiguration$xAxis$ticklabels <- timeTicks$ticklabels

  return(plotConfiguration)
}

#' @title getPlotConfigurationFromPlan
#' @description Get the appropriate `PlotConfiguration` object with scaled dimensions for exporting it
#' @param plotProperties Plot properties from configuration plan
#' @param plotType Name of plot type to call the appropriate `PlotConfiguration` object.
#' E.g. for pk ratio plots, use "PKRatio" to create a `PKRatioPlotConfiguration` object
#' @param legendPosition Legend position in order to add scale factor in the final plot dimensions
#' that accounts for possible shrinking of the plot panel due to the addition of the legend
#' @return A `PlotConfiguration` object
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getPlotConfigurationFromPlan <- function(plotProperties, plotType = NULL, legendPosition = NULL) {
  # Define the appropriate configuration from plotType
  # by creating expression: "tlf::<plotType>PlotCOnfiguration$new()"
  plotConfiguration <- eval(parse(text = paste0("tlf::", plotType, "PlotConfiguration$new()")))

  # Set properties from FontAndSize field
  fonts <- plotProperties$FontAndSize$Fonts
  # plotConfiguration initial font and size properties were defined from current theme
  # their scaling was not perform then to have it performed only at this level
  plotConfiguration$labels$title$font$size <- fonts$TitleSize %||% plotConfiguration$labels$title$font$size
  plotConfiguration$labels$subtitle$font$size <- fonts$DescriptionSize %||% plotConfiguration$labels$subtitle$font$size
  plotConfiguration$labels$xlabel$font$size <- fonts$AxisSize %||% plotConfiguration$labels$xlabel$font$size
  plotConfiguration$labels$ylabel$font$size <- fonts$AxisSize %||% plotConfiguration$labels$ylabel$font$size
  plotConfiguration$xAxis$font$size <- fonts$AxisSize %||% plotConfiguration$xAxis$font$size
  plotConfiguration$yAxis$font$size <- fonts$AxisSize %||% plotConfiguration$yAxis$font$size
  plotConfiguration$legend$font$size <- fonts$LegendSize %||% plotConfiguration$legend$font$size
  plotConfiguration$background$watermark$font$size <- fonts$WatermarkSize %||% plotConfiguration$background$watermark$font$size
  # Fix issue #1333: use same font between axes
  if(isOfType(plotConfiguration, "TimeProfilePlotConfiguration")){
    plotConfiguration$y2Axis$font$size <- fonts$AxisSize %||% plotConfiguration$y2Axis$font$size
    plotConfiguration$labels$y2label$font$size <- fonts$AxisSize %||% plotConfiguration$labels$y2label$font$size
  }
  
  # Set legend position
  validateIsIncluded(values = legendPosition, parentValues = tlf::LegendPositions, nullAllowed = TRUE)
  plotConfiguration$legend$position <- legendPosition %||% reEnv$theme$background$legendPosition

  # Quadratic dimensions for ObsVsPred, DDIRatio plot type
  # Note that other plots be could included in default quadratic plots
  defaultWidth <- reEnv$defaultPlotFormat$width
  defaultHeight <- reEnv$defaultPlotFormat$height
  if (isIncluded(plotType, c("ObsVsPred", "DDIRatio"))) {
    defaultWidth <- mean(c(defaultWidth, defaultHeight))
    defaultHeight <- defaultWidth
  }
  width <- ifNotNull(
    plotProperties$FontAndSize$ChartWidth,
    plotProperties$FontAndSize$ChartWidth / reEnv$defaultPlotFormat$dpi,
    defaultWidth
  )
  height <- ifNotNull(
    plotProperties$FontAndSize$ChartHeight,
    plotProperties$FontAndSize$ChartHeight / reEnv$defaultPlotFormat$dpi,
    defaultHeight
  )
  # Get dimensions of exported based on legend position and default/specific plot properties
  plotConfiguration$export$units <- reEnv$defaultPlotFormat$units
  plotConfiguration$export$width <- reEnv$fontScaleFactor * width
  plotConfiguration$export$height <- reEnv$fontScaleFactor * height
  return(plotConfiguration)
}

#' @title addLineBreakToCaption
#' @description Add line breaks to get prettier captions
#' @param captions Array of character strings to render
#' @param maxLines Maximum number of lines directly setting the maximum number of line breaks allowed.
#' @param width Maximum number of characters per line desired.
#' Due to `maxLines`, the returned width can be wider than `width`.
#' @return A character vector of wrapped strings with line breaks at sensible places.
#' @export
#' @examples
#' # Use cat to display result of line break character
#' cat(addLineBreakToCaption("this-is-a-long-sentence-with-dashes", maxLines = 2, width = 25))
#'
#' cat(addLineBreakToCaption("this is a sentence with spaces", maxLines = 2, width = 25))
#'
#' cat(addLineBreakToCaption(
#'   "this_is_a_long_sentence_without_preferential_splits",
#'   maxLines = 2, width = 25
#' ))
#'
#' cat(addLineBreakToCaption("this too short to split", maxLines = 3, width = 40))
#'
#' cat(addLineBreakToCaption("this forces the sentence to use one line", maxLines = 1, width = 5))
#'
addLineBreakToCaption <- function(captions, maxLines = reEnv$maxLinesPerLegendCaption, width = reEnv$maxWidthPerLegendCaption) {
  # Get number of characters for each caption
  totalWidths <- nchar(captions)
  # Check which captions need line breaks to split
  captionsToSplit <- totalWidths > width
  if (sum(captionsToSplit) == 0) {
    return(captions)
  }
  # Check how many line breaks are required
  numberOfSplits <- floor(totalWidths / width)
  numberOfLines <- numberOfSplits + 1

  # Splits cannot create more lines than max lines
  numberOfSplits[numberOfLines > maxLines] <- maxLines - 1
  numberOfLines <- numberOfSplits + 1
  #
  for (captionIndex in seq_along(captions)) {
    if (numberOfSplits[captionIndex] == 0) {
      next
    }
    # dashes and spaces provides preferential sites for line breaks
    dashSplits <- as.numeric(gregexpr(pattern = "-", captions[captionIndex])[[1]])
    spaceSplits <- as.numeric(gregexpr(pattern = " ", captions[captionIndex])[[1]])
    possibleSplits <- sort(c(dashSplits[dashSplits > 0], spaceSplits[spaceSplits > 0]))
    # Recalculate width accounting for maxLines
    splitWidth <- totalWidths[captionIndex] / numberOfLines[captionIndex]
    # Get split positions (actualSplits is a vector)
    actualSplits <- getSplitPositions(possibleSplits, splitWidth, numberOfSplits[captionIndex])
    splitFirst <- c(1, actualSplits + 1)
    splitLast <- c(actualSplits, totalWidths[captionIndex])
    # Update captions with sensible line breaks
    captions[captionIndex] <- paste0(
      substring(captions[captionIndex], first = splitFirst, last = splitLast),
      collapse = "\n"
    )
  }
  return(captions)
}

#' @title getSplitPositions
#' @description Algorithm that gets positions where splitting a character string for sensible line breaks
#' @param possibleSplits Positions where a space or a dash was found
#' @param splitWidth Maximum number of characters desired per lines
#' @param numberOfSplits Maximum number of line breaks to use
#' @return Position where to insert a line break character
#' @keywords internal
getSplitPositions <- function(possibleSplits, splitWidth, numberOfSplits) {
  # Optimal splits are at equal width
  optimalSplits <- floor(cumsum(rep(splitWidth, numberOfSplits)))
  for (splitIndex in seq_along(optimalSplits)) {
    if (isOfLength(possibleSplits, 0)) {
      return(optimalSplits)
    }
    positionDifference <- min(abs(possibleSplits - optimalSplits[splitIndex]))
    # If closest possible split too far, use optimal split
    if (positionDifference > splitWidth) {
      next
    }
    # If available use the available split and remove it from other loops
    closestAvailableSplitIndex <- which.min(abs(possibleSplits - optimalSplits[splitIndex]))
    optimalSplits[splitIndex] <- possibleSplits[closestAvailableSplitIndex]
    possibleSplits <- possibleSplits[-closestAvailableSplitIndex]
  }
  return(optimalSplits)
}

#' @title getLineBreakWidth
#' @description Calculate the maximum number of characters before breaking lines.
#' This aims at preventing as much as possible legends shrinking the plot and legends not fully displayed
#' @param element The name of element to which the line break should be added.
#' If applied to the legend, use `"legend"`.
#' If applied to a plot label use e.g. `"ylabel"` or `"title"`.
#' If applied to tick labels use `"yticks"` or `"yticklabels"`.
#' @param plotConfiguration A `PlotConfiguration` object from the `tlf` package
#' @return An integer as max character width before using line breaks
#' @keywords internal
getLineBreakWidth <- function(element = "legend", plotConfiguration) {
  # Use inches as unit of formula for plotWidth
  plotWidth <- plotConfiguration$export$width / switch(plotConfiguration$export$units,
    "in" = 1,
    "cm" = 2.54,
    "mm" = 25.4,
    1
  )
  # Initialize a default fontsize to have a more robust
  fontSize <- 10
  if (isIncluded(element, "legend")) {
    fontSize <- plotConfiguration$legend$font$size
    # Use only a third of the plot width when legend is on the side
    if (isIncluded(plotConfiguration$legend$position, tlf::LegendPositions[c("outsideLeft", "outsideRight")])) {
      fontSize <- plotConfiguration$legend$font$size * 3
    }
  }
  # When applied to elements other than legend
  if (isIncluded(element, names(plotConfiguration$labels))) {
    fontSize <- plotConfiguration$labels[[element]]$font$size
  }
  # Use only a third of the plot width when using tick labels
  if (isIncluded(element, c("yticks", "yticklabels", "yAxis"))) {
    fontSize <- plotConfiguration$yAxis$font$size * 3
  }

  # Return max number of characters for using line break
  return(round(120 * plotWidth / fontSize))
}


#' @title prettyCaption
#' @description Get prettied captions with line breaks to prevent cropping of long captions
#' @param captions Array of character strings to render
#' @param element The name of element to which the line break should be added.
#' If applied to the legend, use `"legend"`.
#' If applied to a plot label use e.g. `"ylabel"` or `"title"`.
#' If applied to tick labels use `"yticks"` or `"yticklabels"`.
#' @param plotObject A `ggplot` object
#' @return A character vector of wrapped strings with line breaks at sensible places.
#' @export
prettyCaption <- function(captions, plotObject, element = "legend") {
  maxWidth <- getLineBreakWidth(element, plotObject$plotConfiguration)
  return(addLineBreakToCaption(captions, width = maxWidth))
}

#' @title updateWatermarkDimensions
#' @description Update Watermark dimensions
#' @param plotObject A `ggplot` object
#' @return A `ggplot` object
#' @keywords internal
updateWatermarkDimensions <- function(plotObject) {
  # No need to update if no displayed watermark
  if (isEmpty(plotObject$plotConfiguration$background$watermark$text)) {
    return(plotObject)
  }
  # Watermark size in inches to compare with plot dimensions
  # Font size is in point = 1/72 inches
  watermarkSize <- nchar(plotObject$plotConfiguration$background$watermark$text) *
    plotObject$plotConfiguration$background$watermark$font$size / 72
  watermarkWidth <- abs(watermarkSize * cos(plotObject$plotConfiguration$background$watermark$font$angle * pi / 180))
  watermarkHeight <- abs(watermarkSize * sin(plotObject$plotConfiguration$background$watermark$font$angle * pi / 180))

  # Plot dimensions in inches to compare with watermark dimensions
  unitScaling <- switch(plotObject$plotConfiguration$export$units,
    "in" = 1,
    "cm" = 2.54,
    "mm" = 25.4,
    1
  )
  plotWidth <- plotObject$plotConfiguration$export$width / unitScaling
  plotHeight <- plotObject$plotConfiguration$export$height / unitScaling

  # Comparison and scaling of watermark
  watermarkScaling <- max(
    watermarkWidth / plotWidth,
    watermarkHeight / plotHeight
  )

  if (watermarkScaling <= 1) {
    return(plotObject)
  }
  plotObject <- tlf::setWatermark(
    plotObject = plotObject,
    size = plotObject$plotConfiguration$background$watermark$font$size / watermarkScaling
  )
  return(plotObject)
}

#' @title getLegendDimensions
#' @description
#' Get legend dimensions
#' @param plotObject A `ggplot` object
#' @return
#' A list of `width` and `height` if legend is found,
#' otherwise `NULL`
#' @keywords internal
#' @importFrom grid convertUnit
#' @importFrom cowplot get_plot_component
getLegendDimensions <- function(plotObject) {
  # Duplicate plot object to apply modifications preventing crash
  legendPlotObject <- plotObject
  for (elementName in names(legendPlotObject$theme)) {
    if (isOfType(legendPlotObject$theme[[elementName]], "element_text")) {
      legendPlotObject$theme[[elementName]]$family <- ""
    }
  }
  # See https://wilkelab.org/cowplot/reference/set_null_device.html for more details
  cowplot::set_null_device("png")
  # Get grob from plot = list of plot properties
  allLegendGrobs <- cowplot::get_plot_component(
    legendPlotObject,
    pattern = "guide-box",
    return_all = TRUE
  )
  # Look for legend grob that stores the dimensions of the legend
  legendGrobIndex <- head(which(sapply(allLegendGrobs, function(grob) grob$name) %in% "guide-box"), 1)
  # If no legend, index is empty
  if (isEmpty(legendGrobIndex)) {
    return(NULL)
  }
  legendGrob <- allLegendGrobs[[legendGrobIndex]]
  # grid package is already required and installed by ggplot2
  legendWidth <- as.numeric(grid::convertUnit(
    max(legendGrob$widths),
    plotObject$plotConfiguration$export$units
  ))
  legendHeight <- as.numeric(grid::convertUnit(
    max(legendGrob$heights),
    plotObject$plotConfiguration$export$units
  ))
  return(list(width = legendWidth, height = legendHeight))
}

#' @title updatePlotDimensions
#' @description Update plot dimensions based on size and position of legend
#' @param plotObject A `ggplot` object
#' @return A `ggplot` object
#' @keywords internal
updatePlotDimensions <- function(plotObject) {
  legendDimensions <- getLegendDimensions(plotObject)
  # If no legend, index is empty
  if (isEmpty(legendDimensions)) {
    return(plotObject)
  }
  # If not empty,
  # - add nothing if legend within
  if (grepl(pattern = "inside", x = plotObject$plotConfiguration$legend$position)) {
    # Add small margin of 20 pts on right side of plot to prevent axis ticklabel being cut-off
    plotObject <- plotObject +
      ggplot2::theme(plot.margin = ggplot2::margin(r = 20, b = 10, l = 10))
    return(plotObject)
  }
  legendWidth <- legendDimensions$width
  legendHeight <- legendDimensions$height
  # - add legend height to the final plot dimensions if legend above/below
  isLegendPositionVertical <- any(
    grepl(pattern = "Top", x = plotObject$plotConfiguration$legend$position),
    grepl(pattern = "Bottom", x = plotObject$plotConfiguration$legend$position)
  )
  if (isLegendPositionVertical) {
    # Prevent truncated legend, if legend is too long
    # Get size ratio to keep same aspect ratio
    sizeRatio <- plotObject$plotConfiguration$export$height / plotObject$plotConfiguration$export$width
    # Update width if top/bottom legend is too wide (add 5% to legend width to ensure all the entry content are displayed)
    plotObject$plotConfiguration$export$width <- max(plotObject$plotConfiguration$export$width, 1.05 * legendWidth)
    # Keep width-height aspect ratio
    plotObject$plotConfiguration$export$height <- sizeRatio * plotObject$plotConfiguration$export$height
    # Add legend height to final plot height to prevent shrinkage of plot area
    plotObject$plotConfiguration$export$height <- plotObject$plotConfiguration$export$height + legendHeight
    # Caution: pieChart currently do not use watermark because of ggplot2::coord_polar
    if (!isOfType(plotObject$plotConfiguration, "PieChartPlotConfiguration")) {
      plotObject <- updateWatermarkDimensions(plotObject)
    }
    # Add small margin of 20 pts on right side of plot to prevent axis ticklabel being cut-off
    plotObject <- plotObject +
      ggplot2::theme(plot.margin = ggplot2::margin(r = 20, b = 10, l = 10))
    return(plotObject)
  }
  # Prevent truncated legend, if legend is too long
  # Get size ratio to keep same aspect ratio
  sizeRatio <- plotObject$plotConfiguration$export$width / plotObject$plotConfiguration$export$height
  # Update height if side legend is too long (add 5% to legend height to ensure all the entries are displayed)
  plotObject$plotConfiguration$export$height <- max(plotObject$plotConfiguration$export$height, 1.05 * legendHeight)
  # Keep width-height aspect ratio
  plotObject$plotConfiguration$export$width <- sizeRatio * plotObject$plotConfiguration$export$width
  # Add legend width to final plot width to prevent shrinkage of plot area
  plotObject$plotConfiguration$export$width <- plotObject$plotConfiguration$export$width + legendWidth
  # Caution: pieChart currently do not use watermark because of ggplot2::coord_polar
  if (!isOfType(plotObject$plotConfiguration, "PieChartPlotConfiguration")) {
    plotObject <- updateWatermarkDimensions(plotObject)
  }
  return(plotObject)
}

#' @title setQuadraticDimension
#' @description Set quadratic dimensions if plot configuration is not user-defined
#' @param plotObject A `ggplot` object
#' @param plotConfiguration `PlotConfiguration` object defined in task settings
#' @return A `ggplot` object
#' @keywords internal
setQuadraticDimension <- function(plotObject, plotConfiguration = NULL) {
  # If user defined the dimensions through a PlotConfiguration object, use it as is
  if (!isEmpty(plotConfiguration)) {
    return(plotObject)
  }
  # Otherwise, set quadratic plot
  newDimension <- mean(c(
    plotObject$plotConfiguration$export$width,
    plotObject$plotConfiguration$export$height
  ))
  plotObject$plotConfiguration$export$width <- newDimension
  plotObject$plotConfiguration$export$height <- newDimension
  return(plotObject)
}

#' @title getTimeProfilePlotConfiguration
#' @description Define a `TimeProfilePlotConfiguration` object
#' @param workflowType Workflow type, either `"mean"` or `"population"`
#' @param group A data.frame mapping properties to output groups
#' @param data A data.frame
#' @param metaData List of metaData defining dimensions and units in the data.frame
#' @param dataMapping List mapping x, y and color variables to `data`
#' @param plotConfiguration A user-defined `TimeProfilePlotConfiguration` object
#' @return A `TimeProfilePlotConfiguration` object
#' @keywords internal
getTimeProfilePlotConfiguration <- function(workflowType,
                                            group,
                                            data,
                                            metaData,
                                            observedData = NULL,
                                            dataMapping = NULL,
                                            plotConfiguration = NULL) {
  # If user-defined plot configuration, use as is
  if (!isEmpty(plotConfiguration)) {
    return(plotConfiguration)
  }
  dataMapping <- switch(workflowType,
    "mean" = tlf::TimeProfileDataMapping$new(
      x = dataMapping$x,
      y = dataMapping$y,
      color = dataMapping$group
    ),
    "population" = tlf::TimeProfileDataMapping$new(
      x = dataMapping$x,
      y = dataMapping$y,
      ymin = dataMapping$ymin,
      ymax = dataMapping$ymax,
      group = dataMapping$group
    )
  )
  plotConfiguration <- tlf::TimeProfilePlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  plotConfiguration <- updatePlotConfigurationTimeTicks(data, metaData, dataMapping, plotConfiguration)

  plotConfiguration$lines$color <- getColorFromOutputGroup(
    group = group,
    data = data,
    dataMapping = dataMapping,
    legendVariable = "legend",
    colorVariable = "color"
  )
  plotConfiguration$points$color <- getColorFromOutputGroup(
    group = group,
    data = observedData,
    dataMapping = dataMapping,
    legendVariable = "legend",
    colorVariable = "color"
  )
  plotConfiguration$errorbars$color <- getColorFromOutputGroup(
    group = group,
    data = observedData,
    dataMapping = dataMapping,
    legendVariable = "legend",
    colorVariable = "color"
  )
  plotConfiguration$ribbons$fill <- getColorFromOutputGroup(
    group = group,
    data = data,
    dataMapping = dataMapping,
    legendVariable = "legend",
    colorVariable = "fill"
  )

  return(plotConfiguration)
}

#' @title getGOFPlotConfiguration
#' @description Define a `PlotConfiguration` object
#' @param plotType Plot type for residuals
#' @param group A data.frame mapping properties to output groups
#' @param data A data.frame
#' @param metaData List of metaData defining dimensions and units in the data.frame
#' @param dataMapping List `DataMapping` object
#' @param plotConfiguration A user-defined `PlotConfiguration` object
#' @return A `PlotConfiguration` object
#' @keywords internal
getGOFPlotConfiguration <- function(plotType,
                                    group,
                                    data,
                                    metaData,
                                    dataMapping = NULL,
                                    plotConfiguration = NULL) {
  # If user-defined plot configuration, use as is
  if (!isEmpty(plotConfiguration)) {
    return(plotConfiguration)
  }

  plotConfiguration <- switch(plotType,
    "obsVsPred" = tlf::ObsVsPredPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    ),
    "obsVsPredLog" = tlf::ObsVsPredPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping,
      xScale = tlf::Scaling$log,
      yScale = tlf::Scaling$log
    ),
    "resVsPred" = tlf::ResVsPredPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    ),
    "resVsTime" = tlf::ResVsTimePlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    ),
    "resHisto" = tlf::HistogramPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    ),
    "resQQPlot" = tlf::QQPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    )
  )

  # Set quadratic plot for obs vs pred
  if (isIncluded(plotType, c("obsVsPred", "obsVsPredLog"))) {
    newDimension <- mean(c(
      plotConfiguration$export$width,
      plotConfiguration$export$height
    ))
    plotConfiguration$export$width <- newDimension
    plotConfiguration$export$height <- newDimension

    # Use auto axis limits to get prettier obs vs pred plot
    axisLimits <- autoAxesLimits(
      c(data$Simulated, data$Observed, data$lloq),
      scale = switch(plotType,
        "obsVsPredLog" = tlf::Scaling$log,
        "obsVsPred" = tlf::Scaling$lin
      )
    )
    plotConfiguration$xAxis$axisLimits <- axisLimits %||% plotConfiguration$xAxis$axisLimits
    plotConfiguration$yAxis$axisLimits <- axisLimits %||% plotConfiguration$yAxis$axisLimits

    updateAxisTicks <- all(isIncluded(plotType, "obsVsPredLog"), !isEmpty(axisLimits))
    if (updateAxisTicks) {
      plotConfiguration$xAxis$ticks <- autoAxesTicksFromLimits(axisLimits)
      plotConfiguration$yAxis$ticks <- autoAxesTicksFromLimits(axisLimits)
    }
  }

  # Set time ticks for res vs time
  if (plotType %in% "resVsTime") {
    plotConfiguration <- updatePlotConfigurationTimeTicks(data, metaData, dataMapping, plotConfiguration)
  }
  # Set labels for qq plots and histograms
  if (plotType %in% "resHisto") {
    plotConfiguration$ribbons$fill <- getColorFromOutputGroup(
      group = group,
      data = data,
      dataMapping = dataMapping,
      legendVariable = "residualsLegend",
      colorVariable = "fill"
    ) # group$fill
    plotConfiguration$labels$ylabel$text <- reEnv$residualsHistogramLabel
  }
  if (plotType %in% "resQQPlot") {
    # Set quadratic qqplot
    newDimension <- mean(c(
      plotConfiguration$export$width,
      plotConfiguration$export$height
    ))
    plotConfiguration$export$width <- newDimension
    plotConfiguration$export$height <- newDimension
    plotConfiguration$labels$xlabel$text <- reEnv$residualsQQLabelX
    plotConfiguration$labels$ylabel$text <- reEnv$residualsQQLabelY
  }

  plotConfiguration$points$color <- getColorFromOutputGroup(
    group = group,
    data = data,
    dataMapping = dataMapping,
    legendVariable = "residualsLegend",
    colorVariable = "color"
  )
  plotConfiguration$errorbars$color <- getColorFromOutputGroup(
    group = group,
    data = data,
    dataMapping = dataMapping,
    legendVariable = "residualsLegend",
    colorVariable = "color"
  )
  return(plotConfiguration)
}

#' @title getBoxWhiskerPlotConfiguration
#' @description Define a `PlotConfiguration` object
#' @param plotScale Scale of Y Axis
#' @param data A data.frame
#' @param metaData dimensions and units in the data.frame
#' @param dataMapping `DataMapping` object
#' @param plotConfiguration A user-defined `PlotConfiguration` object
#' @return A `PlotConfiguration` object
#' @keywords internal
getBoxWhiskerPlotConfiguration <- function(plotScale = "log",
                                           colorGrouping = NULL,
                                           data,
                                           metaData,
                                           dataMapping = NULL,
                                           plotConfiguration = NULL) {
  if (!isEmpty(plotConfiguration)) {
    return(plotConfiguration)
  }
  plotConfiguration <- tlf::BoxWhiskerPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  # Remove xlabel
  plotConfiguration$labels$xlabel$text <- NULL
  # Default angle for x-ticklabels is 45 degrees right aligned
  plotConfiguration$xAxis$font$angle <- 45
  plotConfiguration$xAxis$font$align <- tlf::Alignments$right
  # No need for legend for boxplots
  plotConfiguration$legend$position <- tlf::LegendPositions$none
  # Color groups
  if (!isEmpty(colorGrouping)) {
    fillValues <- getColorFromOutputGroup(
      group = colorGrouping,
      data = data,
      dataMapping = dataMapping,
      colorVariable = "fill"
    )
    plotConfiguration$ribbons$fill <- fillValues
  }
  # Default axes use auto scaling
  if (!isIncluded(plotScale, "log")) {
    return(plotConfiguration)
  }
  yValues <- data[, dataMapping$y]
  boxRange <- autoAxesLimits(yValues[yValues > 0], scale = "log")
  boxBreaks <- autoAxesTicksFromLimits(boxRange)

  plotConfiguration$yAxis$scale <- tlf::Scaling$log
  plotConfiguration$yAxis$axisLimits <- boxRange
  plotConfiguration$yAxis$ticks <- boxBreaks
  return(plotConfiguration)
}

#' @title alignXTicks
#' @description
#' Use the `plotConfiguration` of a `plotObject`
#' to check and perform vertical alignment of x-axis tick labels
#' @param plotObject A `ggplot` object
#' @return A `ggplot` object
#' @keywords internal
#' @import ggplot2
alignXTicks <- function(plotObject) {
  # vertical alignment required only for 45 degrees right aligned x-axis labels
  requireAlignment <- all(
    isIncluded(
      plotObject$plotConfiguration$xAxis$font$align,
      tlf::Alignments$right
    ),
    plotObject$plotConfiguration$xAxis$font$angle %in% 45
  )
  if (!requireAlignment) {
    return(plotObject)
  }
  xAxisFont <- plotObject$plotConfiguration$xAxis$font
  plotObject <- plotObject + ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      colour = xAxisFont$color,
      size = xAxisFont$size,
      face = xAxisFont$fontFace,
      family = tlf:::.checkPlotFontFamily(xAxisFont$fontFamily),
      hjust = switch(xAxisFont$align,
        left = 0,
        center = 0.5,
        right = 1
      ),
      vjust = 1
    )
  )
  return(plotObject)
}


#' @title getColorFromOutputGroup
#' @description Get the appropriate colors from an output group
#' @param group A data.frame mapping properties to output groups
#' @param data A data.frame
#' @param dataMapping A `DataMapping` object
#' @param legendVariable Name of legend variable in`group`
#' @param colorVariable Name of color variable in`group`
#' @return A sorted array of color values
#' @keywords internal
getColorFromOutputGroup <- function(group,
                                    data,
                                    dataMapping,
                                    legendVariable = "legend",
                                    colorVariable = "color") {
  # If no output to plot, return a default color
  if (isEmpty(data)) {
    return("black")
  }
  displayedLegendValues <- unique(data[, dataMapping$groupMapping[[colorVariable]]$group])
  # Get legend order and associated right color
  toKeep <- (group[[legendVariable]] %in% displayedLegendValues) &
    !duplicated(group[[legendVariable]])
  legendValues <- factor(
    group[[legendVariable]][toKeep],
    levels = levels(displayedLegendValues) %||% displayedLegendValues
  )
  legendOrder <- order(legendValues)
  colorValues <- group[[colorVariable]][toKeep]
  return(colorValues[legendOrder])
}

#' @title getColorGroupForPKParameterPlot
#' @description Map colors to population names
#' @param output An `Output` object
#' @param referenceSetName Display name of reference simulation set
#' @param simulationSetNames Display names of simulation sets
#' @return A data.frame mapping colors to names
#' @keywords internal
getColorGroupForPKParameterPlot <- function(output,
                                            referenceSetName = NULL,
                                            simulationSetNames) {
  remainingSetNames <- setdiff(simulationSetNames, referenceSetName)
  colorGrouping <- data.frame(
    # Legend is the default variable name used by getColorFromOutputGroup
    legend = c(referenceSetName, remainingSetNames),
    color = c(
      ifNotNull(referenceSetName, reEnv$referenceColor),
      rep(output$color %||% "dodgerblue", length(remainingSetNames))
    ),
    fill = c(
      ifNotNull(referenceSetName, reEnv$referenceFill),
      rep(output$fill %||% "dodgerblue", length(remainingSetNames))
    )
  )
  return(colorGrouping)
}

#' @title updateVpcPlotColor
#' @description Update colors of lines and ranges of VPC plots
#' @param plotObject A ggplot object
#' @param output An `Output` object
#' @param referenceSimulationSetName Display name of reference simulation set
#' @return A ggplot object
#' @keywords internal
updateVpcPlotColor <- function(plotObject, output, referenceSimulationSetName = NULL) {
  legendSim <- paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range)
  legendReference <- paste(legendSim, "of", referenceSimulationSetName)

  plotObject <- plotObject +
    ggplot2::scale_color_manual(
      breaks = c(ifNotNull(referenceSimulationSetName, legendReference), legendSim),
      values = c(
        ifNotNull(referenceSimulationSetName, reEnv$referenceColor),
        output$color %||% "dodgerblue"
      )
    ) +
    ggplot2::scale_fill_manual(
      breaks = c(ifNotNull(referenceSimulationSetName, legendReference), legendSim),
      values = c(
        ifNotNull(referenceSimulationSetName, reEnv$referenceFill),
        output$fill %||% "dodgerblue"
      )
    )
  return(plotObject)
}

#' @title updateAxesMargin
#' @description Update axes properties based on configuration plan settings for side margins
#' @param axesProperties List of axes properties
#' @param sideMarginsEnabled Logical defining if side margins are enabled.
#' @return List of axes properties
#' @keywords internal
updateAxesMargin <- function(axesProperties, sideMarginsEnabled = TRUE) {
  for (properyName in names(axesProperties)) {
    axesProperty <- axesProperties[[properyName]]
    # Check if side margin is enabled or necessary
    noSideMargins <- any(
      sideMarginsEnabled = FALSE,
      isEmpty(axesProperty$min),
      isEmpty(axesProperty$max)
    )
    if (noSideMargins) {
      next
    }
    # Update range for log scale plots
    if (isIncluded(axesProperty$scale, tlf::Scaling$log)) {
      axesProperty$min <- 0.7 * axesProperty$min
      axesProperty$max <- axesProperty$max / 0.7
      axesProperties[[properyName]] <- axesProperty
      next
    }
    axesRange <- axesProperty$max - axesProperty$min
    axesProperty$min <- axesProperty$min - axesRange / 10
    axesProperty$max <- axesProperty$max + axesRange / 10
    axesProperties[[properyName]] <- axesProperty
  }
  return(axesProperties)
}

#' @title getDefaultPropertyFromTheme
#' @description
#' Get default property value from current reEnv theme
#' @param plotName Name of the plot in Theme (e.g. `"plotTimeProfile"`)
#' @param propertyType One of `"points"`, `"lines`, `"ribbons"` or `"errorbars"`
#' @param propertyNames Names of the aesthetic property (e.g. `"color"`)
#' @return Property value
#' @keywords internal
getDefaultPropertiesFromTheme <- function(plotName,
                                          propertyType = "points",
                                          propertyNames = as.character(tlf::AestheticProperties)) {
  # The function to get values from a Theme/PlotConfiguration exists in tlf but it is not exported
  # For this reason, it needs to be called using :::
  tlf:::.getAestheticValuesFromConfiguration(
    plotConfigurationProperty = reEnv$theme$plotConfigurations[[plotName]][[propertyType]],
    propertyNames = propertyNames
  )
}

#' @title getLegendAesOverride
#' @description
#' In time profiles, legends are merged into one unique legend
#' The displayed legend is stored in the `plotObject` within the color guide field `override.aes`.
#' This function simply gets the list from that field for updating the current legend
#' @param plotObject A ggplot object
#' @return A list of aesthetic values
#' @keywords internal
#' @import ggplot2
getLegendAesOverride <- function(plotObject) {
  # ggplot2 version 3.5.0 has made some break changes regarding guides
  if (packageVersion("ggplot2") >= "3.5.0") {
    return(plotObject$guides$guides$colour$params$override.aes)
  }
  return(plotObject$guides$colour$override.aes)
}

#' @title addLLOQLegend
#' @description
#' Add LLOQ displayed legend to the legend of a `plotObject`
#' @param plotObject A ggplot object
#' @param captions Current observed data captions for which lloq legend is needed
#' @param prefix Prefix for legend
#' @return A list of aesthetic values
#' @keywords internal
addLLOQLegend <- function(plotObject, captions, prefix = "LLOQ for") {
  # Since lloq legend should be positioned after the current legend
  # Current legend needs to be reused by the color and shape guides
  # to prevent losing the correct captions and keys
  currentLegend <- getLegendAesOverride(plotObject)
  lloqLegend <- prettyCaption(
    captions = paste(prefix, captions),
    plotObject = plotObject
  )

  # If both observed and simulated data are displayed
  # tlf merge the legends using option override.aes from color guide
  # while removing the legends from linetype and shape
  shapeGuide <- "none"
  if (isEmpty(currentLegend)) {
    # ggplot2 auto-merge shape and color legends, if only observed data are displayed
    # thus both color and shape guides need to be consistent by using same order and title
    shapeGuide <- ggplot2::guide_legend(
      title = plotObject$plotConfiguration$legend$title$text,
      title.theme = plotObject$plotConfiguration$legend$title$createPlotTextBoxFont(),
      order = 1,
      label.theme = plotObject$plotConfiguration$legend$font$createPlotTextBoxFont()
    )
    # For ggplot2 version >= 3.5, themes are not in guides anymore
    if (packageVersion("ggplot2") >= "3.5.0") {
      shapeGuide <- ggplot2::guide_legend(
        title = plotObject$plotConfiguration$legend$title$text,
        order = 1
      )
    }
  }
  colorGuide <- ggplot2::guide_legend(
    title = plotObject$plotConfiguration$legend$title$text,
    title.theme = plotObject$plotConfiguration$legend$title$createPlotTextBoxFont(),
    order = 1,
    override.aes = currentLegend,
    label.theme = plotObject$plotConfiguration$legend$font$createPlotTextBoxFont()
  )
  # For ggplot2 version >= 3.5, themes are not in guides anymore
  if (packageVersion("ggplot2") >= "3.5.0") {
    colorGuide <- ggplot2::guide_legend(
      title = plotObject$plotConfiguration$legend$title$text,
      order = 1,
      override.aes = currentLegend
    )
  }

  # the linetype guide should display the caption for lloq legend
  # corresponding to "LLOQ for <caption of the observed data set>"
  # - order argument renders legend after current legend
  # - title is null to allow pasting this additional legend right below the current
  linetypeGuide <- ggplot2::guide_legend(
    title = NULL,
    order = 2,
    override.aes = list(
      shape = tlf::Shapes$blank,
      linetype = tlf:::tlfEnv$defaultLLOQLinetype,
      fill = NA
    ),
    label.theme = plotObject$plotConfiguration$legend$font$createPlotTextBoxFont()
  )
  # For ggplot2 version >= 3.5, themes are not in guides anymore
  if (packageVersion("ggplot2") >= "3.5.0") {
    linetypeGuide <- ggplot2::guide_legend(
      title = NULL,
      order = 2,
      override.aes = list(
        shape = tlf::Shapes$blank,
        linetype = tlf:::tlfEnv$defaultLLOQLinetype,
        fill = NA
      )
    )
  }

  # Needs to add a dummy linetype aesthetic to get lloq legend displayed
  plotObject <- plotObject +
    ggplot2::geom_blank(
      mapping = ggplot2::aes(linetype = lloqLegend),
      inherit.aes = FALSE
    )
  linetypeScale <- plotObject$scales$get_scales("linetype")

  # Suppress message stating scale was updated
  suppressMessages({
    plotObject <- plotObject +
      # Ensure only lloq legend entries are displayed
      # and prevent current linetypes to be changed or removed
      ggplot2::scale_linetype_manual(
        breaks = head(c(linetypeScale$breaks, lloqLegend), length(lloqLegend)),
        values = as.character(c(
          linetypeScale$palette(1),
          rep(tlf::Linetypes$blank, length(lloqLegend))
        )),
        labels = lloqLegend
      ) +
      ggplot2::guides(
        colour = colorGuide,
        shape = shapeGuide,
        linetype = linetypeGuide
      ) +
      # Ensures
      # - gaps between legends are removed
      # - legends are on top of each other (not side by side)
      # - aligned on legend keys (prettier display)
      ggplot2::theme(
        legend.margin = ggplot2::margin(
          t = -plotObject$plotConfiguration$legend$title$font$size / 2,
          b = -plotObject$plotConfiguration$legend$title$font$size / 2,
          unit = "pt"
        ),
        legend.box = "vertical",
        legend.box.just = "left"
      )
  })

  return(plotObject)
}
