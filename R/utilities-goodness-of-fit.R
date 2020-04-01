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
  observedData <- NULL
  goodnessOfFitPlots <- NULL
  residuals <- NULL
  
  # Get the time profile for observed data
  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    observations <- list()
    observations$data <- readObservedDataFile(structureSet$simulationSet$observedDataFile)
    observations$metaData <- readObservedDataFile(structureSet$simulationSet$observedMetaDataFile)
    observations$filter <- structureSet$simulationSet$dataFilter
    
    # TO DO: Agree on dictionary to replace "matlabID"
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
    observedData <- data.frame(
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
  
  simulatedData <- data.frame(
      "Time" = toUnit("Time", simulationPathResults$data[, "Time"], structureSet$simulationSet$timeUnit),
      "Concentration" = toUnit(
        simulationQuantity,
        simulationPathResults$data[, structureSet$simulationSet$pathID],
        structureSet$simulationSet$pathUnit,
        molWeight = molWeight
      ),
      "Legend" = paste0(structureSet$simulationSet$pathName, " simulated data")
    )
  
  timeProfileData <- rbind.data.frame(
    observedData,
    simulatedData
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
  
  timeProfileMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Concentration",
    color = "Legend"
  )
  
  timeProfilePlot <- plotMeanTimeProfile(simulatedData = simulatedData,
                                         observedData = observedData,
                                         metaData = timeProfileMetaData,
                                         dataMapping = timeProfileMapping,
                                         plotConfiguration = plotConfigurations[["timeProfile"]])
  
  timeProfilePlotLog <- timeProfilePlot + ggplot2::scale_y_continuous(trans = "log10")
  
  goodnessOfFitPlots[["timeProfile"]] <- timeProfilePlot
  goodnessOfFitPlots[["timeProfileLog"]] <- timeProfilePlotLog
  
  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    residuals <- getResiduals(
      timeProfileData,
      timeProfileMetaData
    )
    # Smart plotConfig labels metaData$dimension [metaData$unit]
    residuals$metaData[["Observed"]]$dimension <- paste0("Observed ", structureSet$simulationSet$pathName)
    residuals$metaData[["Simulated"]]$dimension <- paste0("Simulated ", structureSet$simulationSet$pathName)
    
    goodnessOfFitPlots[["obsVsPred"]] <- plotMeanObsVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      caption = structureSet$simulationSet$dataReportName,
      plotConfiguration = plotConfigurations[["obsVsPred"]]
    )
    
    goodnessOfFitPlots[["obsVsPredLog"]] <- goodnessOfFitPlots[["obsVsPred"]] + 
      ggplot2::scale_y_continuous(trans = "log10") + 
      ggplot2::scale_x_continuous(trans = "log10")
    
    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residuals$data,
      metaData = residuals$metaData,
      caption = structureSet$simulationSet$dataReportName,
      plotConfiguration = plotConfigurations[["resVsTime"]]
    )
    
    goodnessOfFitPlots[["resVsPred"]] <- plotMeanResVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      caption = structureSet$simulationSet$dataReportName,
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
#' @param simulatedData data.frame of observed data
#' @param observedData data.frame of simulated data
#' @param metaData meta data on `data`
#' @param dataMapping `TimeProfileDataMapping` R6 class object from `tlf` library
#' @param plotCOnfiguration `TimeProfilePlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanTimeProfile <- function(simulatedData,
                                observedData = NULL,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL) {
  
  timeProfilePlot <- tlf::addLine(data = simulatedData,
                                  metaData = metaData,
                                  dataMapping = dataMapping,
                                  plotConfiguration = plotConfiguration)
  if(!is.null(observedData)){
    timeProfilePlot <- tlf::addScatter(data = observedData,
                                       metaData = metaData,
                                       dataMapping = dataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = timeProfilePlot)
  }
  # Remove guides for unwanted aes_properties
  timeProfilePlot <- timeProfilePlot + 
    guides(color = guide_legend(title = "Legend"), 
           shape = guide_none(), 
           linetype = guide_none(), 
           size = guide_none()) + labs(title = NULL, subtitle = NULL)
  
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
    "Residuals" = log(observedData[, "Concentration"]) - log(simulatedData[, "Concentration"])
  )
  
  # TO DO: integrate method to get residuals metadata
  residuals$metaData <- list()
  residuals$metaData[["Time"]] <- metaData[["Time"]]
  residuals$metaData[["Observed"]] <- metaData[["Concentration"]]
  residuals$metaData[["Simulated"]] <- metaData[["Concentration"]]
  residuals$metaData[["Residuals"]] <- list(unit = "",
                                            dimension = "Residuals\nlog(Observed)-log(Simulated)")
  
  return(residuals)
}


#' @title plotMeanObsVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param caption legend caption for data.frame
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanObsVsPred <- function(data,
                              metaData = NULL,
                              caption = NULL,
                              plotConfiguration = NULL) {
  
  identityMinMax <- c(0.8 * min(cbind(data[, "Observed"], data[, "Simulated"])), 
                    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"])))
  identityLine <- data.frame("Observed" = identityMinMax,
                             "Simulated" = identityMinMax)
  meanObsVsPredPlot <- tlf::addLine(data =  identityLine,
                                    metaData = metaData,
                                    caption = "line of identity",
                                    plotConfiguration = plotConfiguration)
  
  meanObsVsPredPlot <- tlf::addScatter(x = data[, "Observed"],
                                       y = data[, "Simulated"],
                                       metaData = metaData,
                                       caption = paste0("symbols: ", caption),
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanObsVsPredPlot)
  
  # Remove unwanted aes properties
  meanObsVsPredPlot <- meanObsVsPredPlot + ggplot2::guides(color = guide_legend(title = "Legend"), 
                                                  shape = guide_none(), 
                                                  linetype = guide_none(), 
                                                  size = guide_none())  + 
    labs(title = NULL, subtitle = NULL)
  
  return(meanObsVsPredPlot)
}


#' @title plotMeanResVsTime
#' @description Plot Residual vs time for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param caption legend caption for data.frame
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanResVsTime <- function(data,
                              metaData = NULL,
                              caption = NULL,
                              plotConfiguration = NULL) {
  
  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals"
  )
  
  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]))
  
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping
  )
  
  meanResVsTimePlot <- tlf::addLine(y = 0,
                                    caption = "Residuals = 0",
                                    plotConfiguration = plotConfiguration)
  
  meanResVsTimePlot <- tlf::addScatter(data = data,
                                       metaData = metaData,
                                       caption = paste0("symbols: ", caption),
                                       dataMapping = resVsTimeDataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanResVsTimePlot)
  
  # Remove unwanted aes properties
  meanResVsTimePlot <- meanResVsTimePlot + 
    scale_y_continuous(limits = c(-maxRes, maxRes)) + 
    ggplot2::guides(color = guide_legend(title = "Legend"), 
                    shape = guide_none(), 
                    linetype = guide_none(), 
                    size = guide_none())  + 
    labs(title = NULL, subtitle = NULL)
  
  return(meanResVsTimePlot)
}

#' @title plotMeanResVsPred
#' @description Plot observation vs prediction for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param caption legend caption for data.frame
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanResVsPred <- function(data,
                              metaData = NULL,
                              caption = NULL,
                              plotConfiguration = NULL) {
  resVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Simulated",
    y = "Residuals"
  )
  
  maxRes <- 1.2 * max(abs(data[, resVsPredDataMapping$y]))
  
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping
  )
  
  meanResVsPredPlot <- tlf::addLine(y = 0,
                                    caption = "Residuals = 0",
                                    plotConfiguration = plotConfiguration)
  
  meanResVsPredPlot <- tlf::addScatter(data = data,
                                       metaData = metaData,
                                       caption = paste0("symbols: ", caption),
                                       dataMapping = resVsPredDataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanResVsPredPlot)
  
  # Remove unwanted aes properties
  meanResVsPredPlot <- meanResVsPredPlot + 
    scale_y_continuous(limits = c(-maxRes, maxRes)) + 
    ggplot2::guides(color = guide_legend(title = "Legend"), 
                    shape = guide_none(), 
                    linetype = guide_none(), 
                    size = guide_none())  + 
    labs(title = NULL, subtitle = NULL)
  
  return(meanResVsPredPlot)
}