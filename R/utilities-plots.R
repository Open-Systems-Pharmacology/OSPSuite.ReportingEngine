# Set default legend position to outside top
reDefaultLegendPosition <- tlf::LegendPositions$outsideTop
tlf::setDefaultLegendPosition(reDefaultLegendPosition)

#' @title AggregationConfiguration
#' @description To be deprecated:
#' Aggregation default properties  (which functions and their captions).
#' @field functions list of `middle`, `ymin` and `ymax` functions for aggregation
#' @field names list of legend captions for `middle` and `range` from aggregation
#' @field bins default number of bins in plots
#' @field binUsingQuantiles logical to choose a binning based on the quantiles rather thanon a constant interval width
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
  ),
  bins = 11,
  binUsingQuantiles = TRUE
)

displayDimension <- function(dimension) {
  if (isIncluded(dimension, c("Concentration (mass)", "Concentration (molar)"))) {
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

  # If chart size is defined, it is in pixel and updated accordingly
  # Get conversion factor between pixels and inches, dev.size provides an array c(width, height)
  unitConversionFactor <- grDevices::dev.size("in") / grDevices::dev.size("px")
  width <- ifNotNull(
    plotProperties$FontAndSize$ChartWidth,
    plotProperties$FontAndSize$ChartWidth * unitConversionFactor[1],
    reEnv$defaultPlotFormat$width
  )
  height <- ifNotNull(
    plotProperties$FontAndSize$ChartHeight,
    plotProperties$FontAndSize$ChartHeight * unitConversionFactor[2],
    reEnv$defaultPlotFormat$height
  )

  legendScaling <- getLegendScalingFactors(legendPosition)

  # Get dimensions of exported based on legend position and default/specific plot properties
  plotConfiguration$export$units <- reEnv$defaultPlotFormat$units
  plotConfiguration$export$width <- reEnv$fontScaleFactor * legendScaling$width * width
  plotConfiguration$export$height <- reEnv$fontScaleFactor * legendScaling$height * height
  return(plotConfiguration)
}

#' @title getLegendScalingFactors
#' @description Get factors to scale plot dimensions accounting for the location of the legend.
#' Initial estimates based on Abdullah's tests
#' TODO: improve this directly from tlf
#' @param legendPosition The name of the legend position as defined by `tlf` enum `LegendPositions`
#' @return A list of scaling values for `width` and `height`
#' @keywords internal
getLegendScalingFactors <- function(legendPosition = tlf::LegendPositions$outsideTop) {
  # Legend on the left/right sides: increase width
  if (isIncluded(legendPosition, c(tlf::LegendPositions$outsideRight, tlf::LegendPositions$outsideLeft))) {
    return(list(width = 4 / 3, height = 1))
  }
  # Legend on the top/bottom sides: increase height
  if (isIncluded(legendPosition, c(
    tlf::LegendPositions$outsideTopLeft,
    tlf::LegendPositions$outsideTop,
    tlf::LegendPositions$outsideTopRight,
    tlf::LegendPositions$outsideBottomLeft,
    tlf::LegendPositions$outsideBottom,
    tlf::LegendPositions$outsideBottomRight
  ))) {
    return(list(width = 1, height = 7 / 6))
  }
  # Otherwise use these default values
  return(list(width = 1, height = 1))
}

#' @title prettyCaption
#' @description Get prettied captions with line breaks to prevent cropping of long captions
#' @param captions Array of character strings to render
#' @param maxLines Maximum number of lines directly setting the maximum number of line breaks allowed.
#' @param width Maximum number of characters per line desired.
#' Due to `maxLines`, the returned width can be wider than `width`.
#' @return A character vector of wrapped strings with line breaks at sensible places.
#' @export
#' @examples
#'
#' cat(prettyCaption("this-is-a-long-sentence-with-dashes", maxLines = 2, width = 25))
#'
#' cat(prettyCaption("this is a sentence with spaces", maxLines = 2, width = 25))
#'
#' cat(prettyCaption("this_is_a_long_sentence_without_preferential_splits", maxLines = 2, width = 25))
#'
#' cat(prettyCaption("this too short to split", maxLines = 3, width = 40))
#'
#' cat(prettyCaption("this forces the sentence to use one line", maxLines = 1, width = 5))
prettyCaption <- function(captions, maxLines = reEnv$maxLinesPerLegendCaption, width = reEnv$maxWidthPerLegendCaption) {
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
#' @param possibleSplits Positions where a space or a dash was flaged
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
