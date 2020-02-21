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
#' @param plotConfigurations List of `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list of `residuals` objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotMeanGoodnessOfFit <- function(structureSet,
                                  plotConfigurations = NULL) {
  timeProfileData <- NULL
  goodnessOfFitPlots <- NULL
  residuals <- NULL

  # Get the time profile for observed data
  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    observations <- list()
    observations$data <- read.csv(structureSet$simulationSet$observedDataFile)
    observations$metaData <- read.csv(structureSet$simulationSet$observedMetaDataFile)

    timeColumn <- as.character(observations$metaData[observations$metaData[, "matlabID"] == "time", "nonmenColumn"])
    dvColumn <- as.character(observations$metaData[observations$metaData[, "matlabID"] == "dv", "nonmenColumn"])

    # TO DO: dataFilter
    # rowFilter <- which(observations$data[dataFilter$condition, dataFilter$variableName])
    rowFilter <- which(observations$data[, dvColumn] >= 0)

    timeProfileData <- data.frame(
      "Time" = observations$data[rowFilter, timeColumn],
      "Concentration" = observations$data[rowFilter, dvColumn],
      "Legend" = paste0(structureSet$simulationSet$pathName, " observed data")
    )
  }

  # Get the time profile for simulated data
  simulation <- loadSimulation(structureSet$simulationSet$simulationFile)
  simulationQuantity <- getQuantity(structureSet$simulationSet$pathID, simulation)

  simulationResult <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )

  simulationPathResults <- getOutputValuesTLF(simulationResult,
    quantitiesOrPaths = structureSet$simulationSet$pathID
  )

  # TO DO: integrate toUnit for correct time display unit
  timeProfileData <- rbind.data.frame(
    timeProfileData,
    data.frame(
      "Time" = simulationPathResults$data[, "Time"] / 60,
      "Concentration" = toUnit(
        simulationQuantity,
        simulationPathResults$data[, structureSet$simulationSet$pathID],
        structureSet$simulationSet$pathUnit
      ),
      "Legend" = paste0(structureSet$simulationSet$pathName, " simulated data")
    )
  )

  # TO DO: integrate method to get metaData
  timeProfileMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = "h"
    ),
    "Concentration" = list(
      dimension = simulationQuantity$dimension,
      unit = structureSet$simulationSet$pathUnit
    )
  )

  # TO DO: work out shape/linetype in mapping
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
    timeProfile = timeProfileData,
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
#' @description Get a data.frame matching simulated data to observed data and compute the residuals
#' @param data data.frame of time profile data
#' @param metaData meta data on `data`
#' @param dataMapping XYDataMapping class object mapping what variable is to compare
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
getResiduals <- function(data,
                         metaData = NULL,
                         dataMapping = NULL) {
  # TO DO:
  # Compute closest time between obs and sim to get residuals
  # and get the residualData as the following template data.frame

  residuals <- list()
  residuals$data <- data.frame(
    "time" = seq(1, 72),
    "observed" = sample(seq(1, 500), size = 72, replace = TRUE),
    "simulated" = sample(seq(1, 500), size = 72, replace = TRUE)
  )

  # TO DO: check which is the order of residuals
  # Check if residuals should be normalized
  residuals$data$residuals <- residuals$data$observed - residuals$data$simulated

  # TO DO: integrate method to get residuals metadata
  residuals$metaData <- list(
    "time" = list(
      dimension = "Time",
      unit = "h"
    ),
    "observed" = list(
      dimension = "observed data",
      unit = "ng/l"
    ),
    "simulated" = list(
      dimension = "simulated data",
      unit = "ng/l"
    ),
    "residuals" = list(
      dimension = "residuals",
      unit = "ng/l"
    )
  )

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
    x = "observed",
    y = "simulated"
  )
  # TO DO: remove Testing Data Watermark when plot is fixed
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = obsVsPredDataMapping,
    watermark = "Testing Data !"
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
    x = "time",
    y = "residuals"
  )
  # TO DO: remove Testing Data Watermark when plot is fixed
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping,
    watermark = "Testing Data !"
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
    x = "simulated",
    y = "residuals"
  )
  # TO DO: remove Testing Data Watermark when plot is fixed
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping,
    watermark = "Testing Data !"
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
