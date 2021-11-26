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

displayDimension <- function(dimension){
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
  minX[minX<0] <- (1+reEnv$autoAxisLimitMargin)*minX
  minX[minX>0] <- (1-reEnv$autoAxisLimitMargin)*minX
  maxX[maxX<0] <- (1-reEnv$autoAxisLimitMargin)*maxX
  maxX[maxX<0] <- (1+reEnv$autoAxisLimitMargin)*maxX
  if(!isIncluded(scale, "log")){
    return(c(minX, maxX))
  }
  # For log plots, 
  # wider range for pretty axes limits
  if ((log10(maxX)-log10(minX)) > 1) {
    return(c(minX, maxX))
  }
  return(c(minX/2, maxX*2))
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
  logTicks <- seq(floor(minLogRange),ceiling(maxLogRange))
  logRange <- maxLogRange - minLogRange
  # If range is wide enough, use one tick every factor 10
  if(logRange > 1){
    return(10^logTicks)
  }
  return(rep(c(1, 2, 5), length(logTicks)) * 10^rep(logTicks, each = 3))
}