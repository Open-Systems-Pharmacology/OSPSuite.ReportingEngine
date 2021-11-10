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
#' @return A list of units for goodness of fit results
#' @keywords internal
autoAxesLimits <- function(x) {
  minX <- min(x, na.rm = TRUE)
  maxX <- max(x, na.rm = TRUE)
  minX[minX<0] <- (1+reEnv$autoAxisLimitMargin)*minX
  minX[minX>0] <- (1-reEnv$autoAxisLimitMargin)*minX
  maxX[maxX<0] <- (1-reEnv$autoAxisLimitMargin)*maxX
  maxX[maxX<0] <- (1+reEnv$autoAxisLimitMargin)*maxX
  return(c(minX, maxX))
}

