# Currently, need to load a RData because, jsonlite need to be in Namespace/Description otherwise
# The code for loading the json is consequently left commented
# reThemeProperties <- jsonlite::fromJSON('./data/RE-theme.json')
load("./data/reThemeProperties.RData")
reTheme <- tlf::Theme$new(
  themesProperties = reThemeProperties,
  labelBaseSize = 11
)
reTheme$titleFont$size <- 12
reTheme$subtitleFont$size <- 10
tlf::useTheme(reTheme)


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

#' @title getMolWeightForPath
#' @description Get Molecular Weight for quantity path name
#' @param pathName List of PK senstivities computed by runSensitivityAnalysis
#' @param simulation List of PlotConfiguration class objects for each plot
#' @return Molecular weight value for the path
#' @export
#' @import ospsuite
getMolWeightForPath <- function(pathName, simulation) {
  molWeightParams <- ospsuite::getAllParametersMatching("*|Molecular *", simulation)
  compoundNames <- sapply(molWeightParams, function(parameter) {
    ospsuite::toPathArray(parameter$path)[1]
  })
  compoundMatching <- which(as.logical(sapply(compoundNames, function(compoundName) {
    grepl(compoundName, pathName)
  })))

  molWeight <- molWeightParams[[compoundMatching]]$value

  return(molWeight)
}
