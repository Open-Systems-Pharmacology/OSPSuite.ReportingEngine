#' @title repeatableSample
#' @description Perform repeatable random sampling
#' @param x Array to be sampled
#' @param size Number of samples
#' @param n Number of repetitions
#' @param seed Random Seed
#' @keywords internal
repeatableSampling <- function(x, 
                               size,
                               n = getDefaultMCRepetitions(), 
                               seed = getDefaultMCRandomSeed()) {
  old <- .Random.seed
  on.exit({.Random.seed <<- old})
  set.seed(seed)
  # Return a matrix of sampled PK parameters
  return(lapply(
    1:n,
    function(index){sample(x = x, size = size, replace = FALSE)}
  ))
}


#' @title 
#' @description 
#' @param 
#' @export
getPKParameterMeasureFromMCSampling <- function(data, data, dataMapping){
  
  PKParamR_temp <- data.frame(q05=numeric(),q25=numeric(),q50=numeric(),q75=numeric(),q95=numeric(),
                              arithMean=numeric(),arithSD=numeric(),arithCV=numeric())
  sampleSize <- min(c(n1,n2))
  
  # Create a list of Sampled PK Parameters for each MC repetition and calculate their Ratio
  allSampledPKValues <- repeatableSampling(x = data2, size = sampleSize)
  allPKRatioStatistics <- lapply(allSampledPKValues, function(sampledPKValues){
    pkRatios <- sampledPKValues / data1
    # Create named array of PK Ratio statistics
    pkRatioStatistics <- c(
      n = sampleSize,
      mean = mean(pkRatios),
      median = mean(pkRatios),
      sd = stats::sd(pkRatios)
    )
    return(pkRatioStatistics)
    })
  # Get median statistics over all MC repetitions as a data.frame
  medianPKRatioStatistics <- as.data.frame(lapply(as.data.frame(do.call(dplyr::bind_rows, allPKRatioStatistics)), median))
  
  summaryRow <- sapply(as.data.frame(do.call("rbind", lapply(repeatableSampling(1:10, 6), function(x){
    y <- x/1:6
    c(median = median(y), mean = mean(y))}))), median)
  
  
  yy <- lapply(xxRatios, function(x){x/data1
    data.frame()
  })
  # Calculate their median
  yy <- do.call("rbind", yy)
                                 
  for(ii in 1:B){
    s_1 <- sample(x = PKParam_1,size = sampleSize,replace = FALSE) # (sub-)sample of population 1
    s_2 <- sample(x = PKParam_2,size = sampleSize,replace = FALSE) # (sub-)sample of population 2
    PKParamR_temp[ii,"arithMean"] <- mean(s_2/s_1)
    PKParamR_temp[ii,"arithSd"] <- sd(s_2/s_1)
    PKParamR_temp[ii,"arithCV"] <- sd(s_2/s_1)/mean(s_2/s_1)
    PKParamR_temp[ii,c("q05","q25","q50","q75","q95")] <- quantile(s_2/s_1,c(0.05,0.25,0.5,0.75,0.95))
  }
  PKParamR1 <- apply(PKParamR_temp,MARGIN = 2,median)
  PKParamR2 <- data.frame(geoMean=exp(mean(log(PKParam_2))-mean(log(PKParam_1))),
                          geoSd=exp(sqrt((sd(log(PKParam_2)))^2+(sd(log(PKParam_1)))^2)),
                          geoCV=sqrt(exp((sd(log(PKParam_2)))^2+(sd(log(PKParam_1)))^2)-1))
  PKParamR <- data.frame(c(PKParamR1,PKParamR2))
  print(PKParamR)
  
}

#' @title ratioBoxplot
#' @description Plot box-whiskers of ratios as is
#' @param data data.frame of the ratios
#' @param plotConfiguration PlotConfiguration R6 class object
#' @return ggplot object
#' @export
#' @import tlf
#' @import ggplot2
ratioBoxplot <- function(data,
                         plotConfiguration = NULL) {
  # TODO: create new molecule plot for this
  ratioPlot <- tlf::initializePlot(plotConfiguration)
  aestheticValues <- tlf:::.getAestheticValuesFromConfiguration(
    n = 1,
    position = 0,
    plotConfigurationProperty = ratioPlot$plotConfiguration$ribbons,
    propertyNames = c("size", "alpha", "fill")
  )
  
  ratioPlot <- ratioPlot +
    ggplot2::geom_boxplot(
      data = data,
      mapping = ggplot2::aes_string(
        x = "Population",
        ymin = "ymin",
        lower = "lower",
        middle = "middle",
        upper = "upper",
        ymax = "ymax"
      ),
      stat = "identity",
      fill = aestheticValues$fill,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    )
  ratioPlot <- tlf:::.updateAxes(ratioPlot)
  return(ratioPlot)
}