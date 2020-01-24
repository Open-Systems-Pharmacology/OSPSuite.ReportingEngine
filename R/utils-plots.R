#' @title plotDemography
#' @description Plot histograms of demography parameters from a simulated population
#' @param simulation simulation class object
#' @param population population class object
#' @param parameterNames Paths of the demography parameters to plot
#' @param plotConfiguration HistogramPlotConfiguration class object
#' @return demographyPlots list of ggplot objects
#' @export
#' @import tlf
#' @import ospsuite
plotDemography <- function(simulation,
                           population,
                           parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height),
                           plotConfiguration = NULL) {
  demographyParameters <- ospsuite::getAllParametersMatching(parameterNames, simulation)
  # plotHistogram uses data input as data.frame
  demographyValues <- as.data.frame(lapply(
    demographyParameters,
    function(p) {
      toDisplayUnit(p, population$getValues(p))
    }
  ))
  names(demographyValues) <- parameterNames

  # Initialize list of plot objects
  demographyPlot <- list()
  for (parameterName in parameterNames) {
    mapping <- tlf::HistogramDataMapping$new(x = parameterName)
    demographyPlot[[parameterName]] <- tlf::plotHistogram(
      data = demographyValues,
      dataMapping = mapping,
      plotConfiguration = plotConfiguration,
      bins = 5
    )
  }
  return(demographyPlot)
}

#' @title plotGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles, vpc,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param populationSimulation class object from runSimulation(simulation, population)
#' @param population population class object
#' @param observedData list of datasets corresponding to observations
#' @param quantity Paths of the quantity parameters to plot
#' @param plotConfiguration List of PlotConfiguration class objects for each plot
#' @return gofPlots list of ggplot objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotGoodnessOfFit <- function(populationSimulation,
                              population,
                              observedData = NULL, # TO DO: include observedData into the analysis
                              quantity = NULL,
                              plotConfiguration = NULL) {
  resultsPaths <- populationSimulation$allQuantityPaths
  path <- resultsPaths[[1]]

  timeProfileResults <- getOutputValuesTLF(populationSimulation,
    path,
    population = population
  )

  # For this test the default mapping will be last quantity of data
  quantity <- quantity %||% utils::tail(names(timeProfileResults$data), 1)

  # Initialize GofPlots
  gofPlots <- list()

  timeProfileMapping <- tlf::TimeProfileDataMapping$new(
    x = "Time",
    y = quantity
  )
  timeProfilePlot <- tlf::plotTimeProfile(
    data = timeProfileResults$data,
    metaData = timeProfileResults$metaData,
    dataMapping = timeProfileMapping,
    plotConfiguration = plotConfiguration[["timeProfile"]]
  )

  gofPlots[["timeProfile"]] <- timeProfilePlot

  # All the other diagnostic plots need observedData
  if (is.null(observedData)) {
    return(gofPlots)
  }
  # TO DO: perform other gof plots
  # Template example for residuals histogram
  residualsData <- getResiduals(timeProfileResults$data,
    observedData,
    dataMapping = XYDataMapping$new(
      x = "Time",
      y = quantity
    )
  )

  residualsHistogramMapping <- tlf::HistogramDataMapping$new(x = "residuals")

  residualsHistogramPlot <- tlf::plotHistogram(
    data = residualsData,
    dataMapping = residualsHistogramMapping,
    plot = plotConfiguration[["residualsHistogram"]]
  )

  gofPlots[["residualsHistogram"]] <- residualsHistogramPlot

  return(gofPlots)
}


#' @title getResiduals
#' @description Get a data.frame matching simulated data to observed data and compute the residuals
#' @param simulatedData data.frame of simulated data
#' @param observedData data.frame of observed data
#' @param dataMapping XYDataMapping class object mapping what variable is to compare
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
#' @import tlf
#' @import utils
getResiduals <- function(simulatedData,
                         observedData,
                         dataMapping = NULL) {
  dataMapping <- dataMapping %||% XYDataMapping$new(
    x = "Time",
    y = ncol(observedData)
  )
  # Caution: This is only a template for example,
  # Observed data will need to be an actual innput
  residualsData <- data.frame(
    "time" = simulatedData[, dataMapping$x],
    "observed" = observedData[, dataMapping$y],
    "simulated" = simulatedData[, dataMapping$y],
    "residuals" = simulatedData[, dataMapping$y] - simulatedData[, dataMapping$y]
  )

  return(residualsData)
}


#' @title plotPKParameters
#' @description Plot boxplot of the PK parameters
#' @param populationSimulation class object from runSimulation(simulation, population)
#' @param population population class object
#' @param observedData list of datasets corresponding to observations
#' @param parameterNames names of PK parameters to plot
#' @param plotConfiguration List of PlotConfiguration class objects for each plot
#' @return pkParametersPlot ggplot object
#' @export
#' @import tlf
#' @import ospsuite
plotPKParameters <- function() {}

#' @title plotSensitivity
#' @description Plot sensitivity analysis results
#' @param pkSensitivities List of PK senstivities computed by runSensitivityAnalysis
#' @param plotConfiguration List of PlotConfiguration class objects for each plot
#' @return sensitivityPlot ggplot object
#' @export
#' @import tlf
#' @import ospsuite
plotSensitivity <- function(pkSensitivities = NULL,
                            plotConfiguration = NULL) {
  # TO DO: create a sensitivity plot environment in tlf
  sensitivityPlot <- ggplot2::ggplot()
}
