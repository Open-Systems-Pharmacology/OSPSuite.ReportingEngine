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


#' @title plotMeanGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param plotConfigurations List of `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list of `residuals` objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotMeanGoodnessOfFit <- function(structureSet,
                                  logFolder = getwd(),
                                  plotConfigurations = NULL) {
  timeProfileData <- NULL
  goodnessOfFitPlots <- NULL
  residuals <- NULL

  # Get the time profile for observed data
  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    observations <- list()
    observations$data <- readObservedDataFile(structureSet$simulationSet$observedDataFile)
    observations$metaData <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)
    observations$filter <- structureSet$simulationSet$dataFilter

    timeColumn <- as.character(observations$metaData[observations$metaData[, "matlabID"] == "time", "nonmenColumn"])
    dvColumn <- as.character(observations$metaData[observations$metaData[, "matlabID"] == "dv", "nonmenColumn"])

    rowFilter <- TRUE
    if (!is.null(observations$filter)) {
      if (!isOfType(observations$filter, "expression")) {
        observations$filter <- parse(text = observations$filter)
      }
      rowFilter <- evalDataFilter(observations$data, observations$filter)

      logWorkflow(
        message = paste0("Number of observations filtered: ", sum(rowFilter)),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )
    }

    # Observed Data needs to be already in the display unit
    timeProfileData <- data.frame(
      "Time" = observations$data[rowFilter, timeColumn],
      "Concentration" = observations$data[rowFilter, dvColumn],
      "Legend" = structureSet$simulationSet$dataReportName
    )
  }

  # Get the time profile for simulated data
  simulation <- loadSimulation(structureSet$simulationSet$simulationFile)
  simulationQuantity <- getQuantity(structureSet$simulationSet$pathID, simulation)

  simulationResult <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )

  simulationPathResults <- ospsuite::getOutputValues(simulationResult,
    quantitiesOrPaths = structureSet$simulationSet$pathID
  )

  molWeight <- getMolWeightForPath(structureSet$simulationSet$pathID, simulation)

  timeProfileData <- rbind.data.frame(
    timeProfileData,
    data.frame(
      "Time" = toUnit("Time", simulationPathResults$data[, "Time"], structureSet$simulationSet$timeUnit),
      "Concentration" = toUnit(
        simulationQuantity,
        simulationPathResults$data[, structureSet$simulationSet$pathID],
        structureSet$simulationSet$pathUnit,
        molWeight = molWeight
      ),
      "Legend" = paste0(structureSet$simulationSet$pathName, " simulated data")
    )
  )

  timeProfileMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = structureSet$simulationSet$pathUnit
    )
  )

  # TO DO: work out on tlf side shape/linetype in mapping
  timeProfileMapping <- tlf::TimeProfileDataMapping$new(
    x = "Time",
    y = "Concentration",
    color = "Legend"
  )

  goodnessOfFitPlots[["timeProfile"]] <- plotMeanTimeProfile(
    data = timeProfileData,
    metaData = timeProfileMetaData,
    dataMapping = timeProfileMapping,
    plotConfiguration = plotConfigurations[["timeProfile"]]
  )

  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    residuals <- getResiduals(
      timeProfileData,
      timeProfileMetaData
    )
    goodnessOfFitPlots[["obsVsPred"]] <- plotMeanObsVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = plotConfigurations[["obsVsPred"]]
    )
    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = plotConfigurations[["resVsTime"]]
    )
    goodnessOfFitPlots[["resVsPred"]] <- plotMeanResVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = plotConfigurations[["resVsPred"]]
    )
  }
  return(list(
    plots = goodnessOfFitPlots,
    tables = timeProfileData,
    residuals = residuals
  ))
}

#' @title plotMeanTimeProfile
#' @description Plot time profile for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `TimeProfileDataMapping` R6 class object from `tlf` library
#' @param plotCOnfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
plotMeanTimeProfile <- function(data,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  timeProfilePlot <- tlf::plotTimeProfile(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  return(timeProfilePlot)
}


#' @title getResiduals
#' @description This function may be reshape to be more generic later on
#' Currently, the input variable data is a data.frame with "Time", "Concentration" and "Legend"
#' The function get the simulated data with the time the closest to the observed data times
#' @param data data.frame of time profile data
#' @param metaData meta data on `data`
#' @param dataMapping XYDataMapping class object mapping what variable is to compare
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
getResiduals <- function(data,
                         metaData = NULL,
                         dataMapping = NULL) {
  dataTypes <- levels(data[, "Legend"])

  # Observed Data
  observedData <- data[data[, "Legend"] == dataTypes[1], ]
  simulatedData <- data[data[, "Legend"] == dataTypes[2], ]

  # Time matrix
  obsTimeMatrix <- matrix(observedData[, "Time"], nrow(simulatedData), nrow(observedData), byrow = TRUE)
  simTimeMatrix <- matrix(simulatedData[, "Time"], nrow(simulatedData), nrow(observedData))

  simFilter <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))

  simulatedData <- simulatedData[simFilter, ]

  residuals <- list()

  # TO DO: check which is the order of residuals
  # Check if residuals should be normalized
  residuals$data <- data.frame(
    "Time" = observedData[, "Time"],
    "Observed" = observedData[, "Concentration"],
    "Simulated" = simulatedData[, "Concentration"],
    "Residuals" = observedData[, "Concentration"] - simulatedData[, "Concentration"]
  )

  # TO DO: integrate method to get residuals metadata
  residuals$metaData <- list()
  residuals$metaData[["Time"]] <- metaData[["Time"]]
  residuals$metaData[["Observed"]] <- metaData[["Concentration"]]
  residuals$metaData[["Simulated"]] <- metaData[["Concentration"]]
  residuals$metaData[["Residuals"]] <- metaData[["Concentration"]]

  return(residuals)
}


#' @title plotMeanObsVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMeanObsVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  obsVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Observed",
    y = "Simulated"
  )

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = obsVsPredDataMapping
  )

  # TO DO: use the new version of tlf to get this plot
  meanObsVsPredPlot <- ggplot2::ggplot()
  meanObsVsPredPlot <- plotConfiguration$setPlotBackground(meanObsVsPredPlot)
  meanObsVsPredPlot <- plotConfiguration$setPlotLabels(meanObsVsPredPlot)

  meanObsVsPredPlot <- meanObsVsPredPlot + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(
      x = obsVsPredDataMapping$x,
      y = obsVsPredDataMapping$y
    )
  ) + ggplot2::geom_abline(
    slope = 1,
    intercept = 0
  )

  return(meanObsVsPredPlot)
}

#' @title plotMeanResVsTime
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMeanResVsTime <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals"
  )

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping
  )

  # TO DO: use the new version of tlf to get this plot
  meanResVsTimePlot <- ggplot2::ggplot()
  meanResVsTimePlot <- plotConfiguration$setPlotBackground(meanResVsTimePlot)
  meanResVsTimePlot <- plotConfiguration$setPlotLabels(meanResVsTimePlot)

  meanResVsTimePlot <- meanResVsTimePlot + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(
      x = resVsTimeDataMapping$x,
      y = resVsTimeDataMapping$y
    )
  ) + ggplot2::geom_hline(yintercept = 0)

  return(meanResVsTimePlot)
}

#' @title plotMeanResVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMeanResVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  resVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Simulated",
    y = "Residuals"
  )

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping
  )

  # TO DO: use the new version of tlf to get this plot
  meanResVsPredPlot <- ggplot2::ggplot()
  meanResVsPredPlot <- plotConfiguration$setPlotBackground(meanResVsPredPlot)
  meanResVsPredPlot <- plotConfiguration$setPlotLabels(meanResVsPredPlot)

  meanResVsPredPlot <- meanResVsPredPlot + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(
      x = resVsPredDataMapping$x,
      y = resVsPredDataMapping$y
    )
  ) + ggplot2::geom_hline(yintercept = 0)

  return(meanResVsPredPlot)
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
