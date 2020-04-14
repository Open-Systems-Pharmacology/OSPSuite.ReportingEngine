#' @title plotMeanGoodnessOfFit
#' @description Plot goodness of fit diagnostics including time profiles,
#' observations vs predictions, residuals plots (residuals vs time, vs predictions, qq-plots and histogram)
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list with `plots`, `tables` and `residuals` objects to be saved
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ggplot2
plotMeanGoodnessOfFit <- function(structureSet,
                                  logFolder = getwd(),
                                  settings = NULL) {
  observedData <- NULL
  simulatedData <- NULL
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
    
    if (is.null(observations$filter)) {
    observedData <- data.frame(
      "Time" = observations$data[, timeColumn],
      "Concentration" = observations$data[, dvColumn],
      "Legend" = structureSet$simulationSet$dataReportName
      )
    }
    
    if (!is.null(observations$filter)) {
      # Ensure that filter is used as an expression
      if (!isOfType(observations$filter, "expression")) {
        observations$filter <- parse(text = observations$filter)
      }
      for (filterIndex in seq_along(observations$filter)){
        rowFilter <- evalDataFilter(observations$data, observations$filter[filterIndex])
        
        logWorkflow(
          message = paste0("Number of observations filtered: ", sum(rowFilter)),
          pathFolder = logFolder,
          logTypes = LogTypes$Debug
        )
        
        # Observed Data needs to be already in the display unit
        # TO DO: ensure in simulationSet that dataReportName is same length as filter
        # orderer = TRUE ensure the same order in the legend, otherwise ggplot use alphabetical order
        observedData <- rbind.data.frame(observedData,
                                         data.frame(
                                           "Time" = observations$data[rowFilter, timeColumn],
                                           "Concentration" = observations$data[rowFilter, dvColumn],
                                           "Legend" = factor(structureSet$simulationSet$dataReportName[filterIndex],
                                                             ordered = TRUE)
                                         )
        )
      }
    }
  }
    
  # Get the time profile for simulated data
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  simulationQuantity <- ospsuite::getAllQuantitiesMatching(structureSet$simulationSet$pathID, simulation)
  
  simulationResult <- ospsuite::importResultsFromCSV(
    simulation,
    structureSet$simulationResultFileNames
  )
  
  simulationPathResults <- ospsuite::getOutputValues(simulationResult,
                                                     quantitiesOrPaths = structureSet$simulationSet$pathID
  )
  
  for (quantityIndex in seq_along(simulationQuantity)){
    # TO DO: replace by osp built in function
    molWeight <- getMolWeightForPath(structureSet$simulationSet$pathID[quantityIndex], simulation)
  
  simulatedData <- rbind.data.frame(simulatedData,
                                   data.frame(
      "Time" = toUnit("Time", simulationPathResults$data[, "Time"], structureSet$simulationSet$timeUnit),
      "Concentration" = toUnit(
        simulationQuantity[[quantityIndex]],
        simulationPathResults$data[, structureSet$simulationSet$pathID[quantityIndex]],
        structureSet$simulationSet$pathUnit[quantityIndex],
        molWeight = molWeight
      ),
      "Legend" = factor(paste0(structureSet$simulationSet$pathName[quantityIndex], " simulated data"),
                        ordered = TRUE)
    )
  )
  }
  
  timeProfileData <- rbind.data.frame(
    observedData,
    simulatedData
  )
  # Reorder factors: observed before simulated
  # This aims at facilitating the standaridzation of the GOF plots:
  # Observed data are split by shapes and colors,
  # So will the residuals with default options
  timeProfileData$Legend <- factor(timeProfileData$Legend,
                                   levels = c(levels(observedData$Legend),
                                              levels(simulatedData$Legend)))
  
  # TO DO: so far only using the first quantity to get the ylabel and its unit
  timeProfileMetaData <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Concentration" = list(
      dimension = simulationQuantity[[1]]$dimension,
      unit = structureSet$simulationSet$pathUnit[1]
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
                                         plotConfiguration = settings$plotConfigurations[["timeProfile"]])
  
  timeProfilePlotLog <- timeProfilePlot + ggplot2::scale_y_continuous(trans = "log10")
  
  goodnessOfFitPlots[["timeProfile"]] <- timeProfilePlot
  goodnessOfFitPlots[["timeProfileLog"]] <- timeProfilePlotLog
  
  if (!is.null(structureSet$simulationSet$observedDataFile)) {
    residuals <- getResiduals(
      observedData,
      simulatedData,
      timeProfileMetaData
    )
    # Smart plotConfig labels metaData$dimension [metaData$unit]
    residuals$metaData[["Observed"]]$dimension <- paste0("Observed ", structureSet$simulationSet$pathName)
    residuals$metaData[["Simulated"]]$dimension <- paste0("Simulated ", structureSet$simulationSet$pathName)
    
    goodnessOfFitPlots[["obsVsPred"]] <- plotMeanObsVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
    )
    
    goodnessOfFitPlots[["obsVsPredLog"]] <- goodnessOfFitPlots[["obsVsPred"]] + 
      ggplot2::scale_y_continuous(trans = "log10") + 
      ggplot2::scale_x_continuous(trans = "log10")
    
    goodnessOfFitPlots[["resVsTime"]] <- plotMeanResVsTime(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = settings$plotConfigurations[["resVsTime"]]
    )
    
    goodnessOfFitPlots[["resVsPred"]] <- plotMeanResVsPred(
      data = residuals$data,
      metaData = residuals$metaData,
      plotConfiguration = settings$plotConfigurations[["resVsPred"]]
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
    dataMapping$groupMapping$shape <- dataMapping$groupMapping$color
    timeProfilePlot <- tlf::addScatter(data = observedData,
                                       metaData = metaData,
                                       dataMapping = dataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = timeProfilePlot)
  }
  # Remove guides for unwanted aes_properties
  timeProfilePlot <- timeProfilePlot + 
    ggplot2::guides(color = ggplot2::guide_legend(title = "Legend"), 
           shape = "none", 
           linetype = "none", 
           size = "none") + 
    ggplot2::labs(title = NULL, subtitle = NULL)
  
  return(timeProfilePlot)
}


#' @title getResiduals
#' @description This function may be reshape to be more generic later on
#' Currently, the input variable data is a data.frame with "Time", "Concentration" and "Legend"
#' The function get the simulated data with the time the closest to the observed data times
#' @param observedData data.frame of time profile observed data
#' @param simulatedData data.frame of time profile simulated data
#' @param metaData meta data on `data`
#' @param dataMapping XYDataMapping class object mapping what variable is to compare
#' @return residualsData data.frame with Time, Observed, Simulated, Residuals
#' @export
getResiduals <- function(observedData,
                         simulatedData,
                         metaData = NULL,
                         dataMapping = NULL) {
  observedDataTypes <- levels(observedData[, "Legend"])
  simulatedDataTypes <- levels(simulatedData[, "Legend"])
  
  residuals <- list()
  
  # Match observed with simulation for each filter
  for (dataIndex in seq_along(observedDataTypes)){
  filterObservedData <- observedData[observedData[, "Legend"] == observedDataTypes[dataIndex], ]
  filterSimulatedData <- simulatedData[simulatedData[, "Legend"] == simulatedDataTypes[dataIndex], ]
  
  # Time matrix
  obsTimeMatrix <- matrix(filterObservedData[, "Time"], nrow(filterSimulatedData), nrow(filterObservedData), byrow = TRUE)
  simTimeMatrix <- matrix(filterSimulatedData[, "Time"], nrow(filterSimulatedData), nrow(filterObservedData))
  
  timeMatchedData <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))
  
  residuals$data <- rbind.data.frame(
    residuals$data,
    data.frame(
      "Time" = filterObservedData[, "Time"],
      "Observed" = filterObservedData[, "Concentration"],
      "Simulated" = filterSimulatedData[timeMatchedData, "Concentration"],
      "Residuals" = log(filterObservedData[, "Concentration"]) - log(filterSimulatedData[timeMatchedData, "Concentration"]),
      "Legend" = observedDataTypes[dataIndex]
    )
  )
}
  
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
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanObsVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  
  identityMinMax <- c(0.8 * min(cbind(data[, "Observed"], data[, "Simulated"])), 
                    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"])))
  identityLine <- data.frame("Observed" = identityMinMax,
                             "Simulated" = identityMinMax)
  
  meanObsVsPredPlot <- tlf::addLine(data =  identityLine,
                                    metaData = metaData,
                                    caption = "Line of identity",
                                    plotConfiguration = plotConfiguration)
  
  meanObsVsPredPlot <- tlf::addScatter(data = data,
                                       metaData = metaData,
                                       dataMapping = tlf::XYGDataMapping$new(x = "Observed", 
                                                                             y = "Simulated", 
                                                                             color = "Legend",
                                                                             shape = "Legend"),
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanObsVsPredPlot)
  
  # Remove unwanted aes properties
  meanObsVsPredPlot <- meanObsVsPredPlot + 
    ggplot2::guides(color = ggplot2::guide_legend(title = "Legend"), 
                    shape = "none", 
                    linetype = "none", 
                    size = "none")  + 
    ggplot2::labs(title = NULL, subtitle = NULL)
  
  return(meanObsVsPredPlot)
}


#' @title plotMeanResVsTime
#' @description Plot Residual vs time for mean model workflow
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
plotMeanResVsTime <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  
  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals",
    color = "Legend",
    shape = "Legend",
  )
  
  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]))
  
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsTimeDataMapping
  )
  
  meanResVsTimePlot <- tlf::addLine(y = 0,
                                    caption = "Line of residuals = 0",
                                    plotConfiguration = plotConfiguration)
  
  meanResVsTimePlot <- tlf::addScatter(data = data,
                                       metaData = metaData,
                                       dataMapping = resVsTimeDataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanResVsTimePlot)
  
  # Remove unwanted aes properties and hide the warnings it provokes
  meanResVsTimePlot <- meanResVsTimePlot + 
    ggplot2::scale_y_continuous(limits = c(-maxRes, maxRes)) + 
    ggplot2::guides(color = ggplot2::guide_legend(title = "Legend"), 
                    shape = "none", 
                    linetype = "none", 
                    size = "none")  + 
    ggplot2::labs(title = NULL, subtitle = NULL)
  
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
plotMeanResVsPred <- function(data,
                              metaData = NULL,
                              plotConfiguration = NULL) {
  resVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Simulated",
    y = "Residuals",
    color = "Legend",
    shape = "Legend"
  )
  
  maxRes <- 1.2 * max(abs(data[, resVsPredDataMapping$y]))
  
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = resVsPredDataMapping
  )
  
  meanResVsPredPlot <- tlf::addLine(y = 0,
                                    caption = "Line of residuals = 0",
                                    plotConfiguration = plotConfiguration)
  
  meanResVsPredPlot <- tlf::addScatter(data = data,
                                       metaData = metaData,
                                       dataMapping = resVsPredDataMapping,
                                       plotConfiguration = plotConfiguration,
                                       plotObject = meanResVsPredPlot)
  
  # Remove unwanted aes properties
  meanResVsPredPlot <- meanResVsPredPlot + 
    ggplot2::scale_y_continuous(limits = c(-maxRes, maxRes)) + 
    ggplot2::guides(color = ggplot2::guide_legend(title = "Legend"), 
                    shape = "none", 
                    linetype = "none", 
                    size = "none")  + 
    ggplot2::labs(title = NULL, subtitle = NULL)
  
  return(meanResVsPredPlot)
}