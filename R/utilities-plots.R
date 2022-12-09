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
#' @return List of `ticks` and their `ticklabels`
#' @keywords internal
getTimeTicksFromUnit <- function(unit, timeValues = NULL) {
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

  minorTicks <- seq(minTime, maxTime, minorTickStep)
  majorTicks <- seq(minTime, maxTime, majorTickStep)
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
  plotConfiguration$labels$title$font$size <- reEnv$fontScaleFactor * (fonts$TitleSize %||% plotConfiguration$labels$title$font$size)
  plotConfiguration$labels$subtitle$font$size <- reEnv$fontScaleFactor * (fonts$DescriptionSize %||% plotConfiguration$labels$subtitle$font$size)
  plotConfiguration$labels$xlabel$font$size <- reEnv$fontScaleFactor * (fonts$AxisSize %||% plotConfiguration$labels$xlabel$font$size)
  plotConfiguration$labels$ylabel$font$size <- reEnv$fontScaleFactor * (fonts$AxisSize %||% plotConfiguration$labels$ylabel$font$size)
  plotConfiguration$xAxis$font$size <- reEnv$fontScaleFactor * (fonts$AxisSize %||% plotConfiguration$xAxis$font$size)
  plotConfiguration$yAxis$font$size <- reEnv$fontScaleFactor * (fonts$AxisSize %||% plotConfiguration$yAxis$font$size)
  plotConfiguration$legend$font$size <- reEnv$fontScaleFactor * (fonts$LegendSize %||% plotConfiguration$legend$font$size)
  plotConfiguration$background$watermark$font$size <- reEnv$fontScaleFactor * (fonts$WatermarkSize %||% plotConfiguration$background$watermark$font$size)

  # Set legend position
  validateIsIncluded(values = legendPosition, parentValues = tlf::LegendPositions, nullAllowed = TRUE)
  plotConfiguration$legend$position <- legendPosition %||% reEnv$theme$background$legendPosition

  # Quadratic dimensions for ObsVsPred plot type
  # Note that other plots be could included in default quadratic plots
  defaultWidth <- reEnv$defaultPlotFormat$width
  defaultHeight <- reEnv$defaultPlotFormat$height
  if (isIncluded(plotType, "ObsVsPred")) {
    defaultWidth <- mean(c(defaultWidth, defaultHeight))
    defaultHeight <- defaultWidth
  }
  # If chart size is defined, it is in pixel and updated accordingly
  # Get conversion factor between pixels and inches, dev.size provides an array c(width, height)
  unitConversionFactor <- grDevices::dev.size("in") / grDevices::dev.size("px")
  width <- ifNotNull(
    plotProperties$FontAndSize$ChartWidth,
    plotProperties$FontAndSize$ChartWidth * unitConversionFactor[1],
    defaultWidth
  )
  height <- ifNotNull(
    plotProperties$FontAndSize$ChartHeight,
    plotProperties$FontAndSize$ChartHeight * unitConversionFactor[2],
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
    # TODO: improve the preferental site using input arguments
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
#' @param PlotConfiguration A `PlotConfiguration` object from the `tlf` package
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

#' @title updatePlotDimensions
#' @description Update plot dimensions based on size and position of legend
#' @param plotObject A `ggplot` object
#' @return A `ggplot` object
#' @keywords internal
updatePlotDimensions <- function(plotObject) {
  # Get grob from plot = list of plot properties
  grobObject <- ggplot2::ggplotGrob(plotObject)
  # Look for legend grob that stores the dimensions of the legend
  legendGrobIndex <- which(sapply(grobObject$grobs, function(grob) grob$name) == "guide-box")
  # If no legend, index is empty
  if (isEmpty(legendGrobIndex)) {
    return(plotObject)
  }
  legendGrob <- grobObject$grobs[[legendGrobIndex]]
  # If not empty,
  # - add nothing if legend within
  if (grepl(pattern = "inside", x = plotObject$plotConfiguration$legend$position)) {
    return(plotObject)
  }
  # grid package is already required and installed by ggplot2
  legendWidth <- as.numeric(grid::convertUnit(max(legendGrob$widths), plotObject$plotConfiguration$export$units))
  legendHeight <- as.numeric(grid::convertUnit(max(legendGrob$heights), plotObject$plotConfiguration$export$units))
  # - add legend height to the final plot dimensions if legend above/below
  if (grepl(pattern = "Top", x = plotObject$plotConfiguration$legend$position) |
    grepl(pattern = "Bottom", x = plotObject$plotConfiguration$legend$position)) {
    # Prevent truncated legend, if legend is too long
    # Get size ratio to keep same aspect ratio
    sizeRatio <- plotObject$plotConfiguration$export$height / plotObject$plotConfiguration$export$width
    # Update width if top/bottom legend is too wide (add 5% to legend width to ensure all the entry content are displayed)
    plotObject$plotConfiguration$export$width <- max(plotObject$plotConfiguration$export$width, 1.05 * legendWidth)
    # Keep width-height aspect ratio
    plotObject$plotConfiguration$export$height <- sizeRatio * plotObject$plotConfiguration$export$height
    # Add legend height to final plot height to prevent shrinkage of plot area
    plotObject$plotConfiguration$export$height <- plotObject$plotConfiguration$export$height + legendHeight
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
    "mean" = tlf::XYGDataMapping$new(
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
  if (plotType %in% "obsVsPred") {
    newDimension <- mean(c(
      plotConfiguration$export$width,
      plotConfiguration$export$height
    ))
    plotConfiguration$export$width <- newDimension
    plotConfiguration$export$height <- newDimension
  }
  # Set time ticks for res vs time
  if (plotType %in% "resVsTime") {
    residualTimeTicks <- getTimeTicksFromUnit(
      metaData$Time$unit,
      timeValues = data$Time
    )
    # ticks = residualTimeTicks$ticks,
    # ticklabels = residualTimeTicks$ticklabels
    plotConfiguration <- updatePlotConfigurationTimeTicks(data, metaData, dataMapping, plotConfiguration)
  }
  # Set labels for qq plots and histograms
  if (plotType %in% "resHisto") {
    plotConfiguration$ribbons$fill <- group$fill
    plotConfiguration$labels$ylabel$text <- reEnv$residualsHistogramLabel
  }
  if (plotType %in% "resQQPlot") {
    plotConfiguration$labels$ylabel$text <- reEnv$residualsQQLabel
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
  legendValues <- group[[legendVariable]][toKeep]
  legendOrder <- order(legendValues)
  colorValues <- group[[colorVariable]][toKeep]
  return(colorValues[legendOrder])
}
