#' @title getQualificationDDIPlotData
#' @description Build dataframes and metadata for each DDI plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return  plotDDIdata, a list of lists of the form list(dataframe,metadata) specific to each DID plot
getQualificationDDIPlotData <- function(configurationPlan){
  plotDDIdata <- list()
  for (plotNumber in seq_along(configurationPlan$plots$DDIRatioPlots)){
    plotDDIDataFrame <- NULL
    plotDDIMetadata <- list()
    plot <- configurationPlan$plots$DDIRatioPlots[[plotNumber]]

    plotDDIMetadata$title <- plot$Title
    plotDDIMetadata$sectionID <- plot$SectionId
    # Pipes in configuration plan will be deprecated moving forward
    #browser()
    plotDDIMetadata$plotTypes <- plot$PlotTypes %||% ospsuite::toPathArray(plot$PlotType)
    plotDDIMetadata$axesSettings <- lapply(plotDDIMetadata$plotTypes, function(pltType) {
      getAxesSettings(configurationPlan$plots$AxesSettings[[ddiPlotAxesSettings[[pltType]]]])
    })
    names(plotDDIMetadata$axesSettings) <- plotDDIMetadata$plotTypes


    plotDDIMetadata$groups <- list()

    pkParameters <- NULL
    if(!is.null(plot$PKParameter)){
      pkParameters <- toPathArray(plot$PKParameter)
    }

    for (groupNumber in seq_along(plot$Groups)){
      group <- plot$Groups[[groupNumber]]
      plotDDIMetadata$groups[[groupNumber]] <- list()
      plotDDIMetadata$groups[[groupNumber]]$caption <- group$Caption
      plotDDIMetadata$groups[[groupNumber]]$color <- group$Color
      plotDDIMetadata$groups[[groupNumber]]$symbol <- tlfShape(group$Symbol)

      for (ddiRatio in group$DDIRatios){
        outputPath <- ddiRatio$Output
        observedDataSet <- ddiRatio$ObservedData
        observedDataSetFilePath <- configurationPlan$getObservedDataPath(id = observedDataSet)
        observedDataRecordId <- ddiRatio$ObservedDataRecordId
        observedDataFrame <- readObservedDataFile(file = observedDataSetFilePath)
        validateIsIncluded(observedDataRecordId, observedDataFrame$ID)

        ratioList <- list()

        for (pkParameter in  pkParameters){
          ratioList[[pkParameter]] <- list()

          validateIsIncluded(ddiPKRatioColumnName[[pkParameter]], names(observedDataFrame))
          ratioList[[pkParameter]]$observedRatio <- observedDataFrame[[ddiPKRatioColumnName[[pkParameter]]]][observedDataFrame$ID == observedDataRecordId]

          for (simulationType in c("SimulationControl","SimulationDDI")){

            plotComponent <- ddiRatio[[simulationType]]
            projectName <- plotComponent$Project
            simulationName <- plotComponent$Simulation

            startTime <-  ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                               values = plotComponent$StartTime,
                                               unit = plotComponent$TimeUnit)

            endTime <- ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                            values = plotComponent$EndTime,
                                            unit = plotComponent$TimeUnit)

            pkParameterName <- generateDDIPlotPKParameterName(pkParameter,startTime,endTime)

            simulationResultsFile <- configurationPlan$getSimulationResultsPath(project = projectName,
                                                                                simulation = simulationName)

            pkAnalysisResultsPath <- configurationPlan$getPKAnalysisResultsPath(project = projectName,
                                                                                simulation = simulationName)

            simulationFile <- configurationPlan$getSimulationPath(
              project = projectName,
              simulation = simulationName
            )

            simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
            pkAnalysisResults <- ospsuite::importPKAnalysesFromCSV(filePath = pkAnalysisResultsPath,
                                                                   simulation = simulation)

            ratioList[[pkParameter]][[simulationType]] <- pkAnalysisResults$pKParameterFor(quantityPath = outputPath,
                                                                                           pkParameter = pkParameterName)$values

          }

          df <- data.frame(project = projectName,
                           simulation = simulationName,
                           groupNumber = groupNumber,
                           outputPath = outputPath,
                           pkParameter = pkParameter,
                           observedRatio = ratioList[[pkParameter]]$observedRatio,
                           simulatedRatio = ratioList[[pkParameter]][["SimulationDDI"]]/ratioList[[pkParameter]][["SimulationControl"]])

          plotDDIDataFrame <- rbind.data.frame(plotDDIDataFrame,df)
        }
      }
    }

    plotDDIdata[[plotNumber]] <- list(dataframe = plotDDIDataFrame,
                                      metadata = plotDDIMetadata)

  }
  return(plotDDIdata)
}





#' @title buildQualificationDDIPredictedVsObserved
#' @description Build dataframe for observation vs prediction
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @return dataframe for plotting goodness of fit of predictedVsObserved type
buildQualificationDDIPredictedVsObserved <- function(dataframe,
                                                     metadata) {
  axesSettings <- metadata$axesSettings$predictedVsObserved
  axesSettings$X$label <- plotDDIXLabel$predictedVsObserved
  axesSettings$Y$label <- plotDDIYLabel$predictedVsObserved

  xUnit <- axesSettings$X$unit
  xDimension <- axesSettings$X$dimension
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- axesSettings$Y$dimension
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  # function to do obs vs sim
  ddiPlotDataframe <- NULL
  aestheticsList <- list(shape = list(), color = list())

  for (grp in unique(dataframe$groupNumber)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    aestheticsList$color[[grp]] <- metadata$groups[[grp]]$color

    xDataDimension <- ""
    xDataUnit <- ""
    xData <- dataframe[dataframe$group == grp, ]$observedRatio

    if (xScaling == "Log") {
      xData <- log10(xData)
    }
    xData <- replaceInfWithNA(xData)

    yDataDimension <- ""
    yDataUnit <- ""
    yData <- dataframe[dataframe$group == grp, ]$simulatedRatio
    if (yScaling == "Log") {
      yData <- log10(yData)
    }
    yData <- replaceInfWithNA(yData)

    df <- list()
    df[[axesSettings$X$label]] <- xData
    df[[axesSettings$Y$label]] <- yData
    df$Group <- grp
    ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, as.data.frame(df))

  }

  ddiPlotDataframe$Group <- as.factor(ddiPlotDataframe$Group)

  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
}




#' #' @title buildQualificationGOFResidualsVsObserved
#' #' @description Build dataframe for residuals vs observed
#' #' @param dataframe data.frame
#' #' @param metadata meta data on `data`
#' #' @return dataframe for plotting goodness of fit of residuals vs time type
buildQualificationDDIResidualsVsObserved <- function(dataframe,
                                                     metadata) {
  axesSettings <- metadata$axesSettings$residualsVsObserved
  axesSettings$X$label <- plotDDIXLabel$residualsVsObserved
  axesSettings$Y$label <- plotDDIYLabel$residualsVsObserved

  xUnit <- axesSettings$X$unit
  xDimension <- axesSettings$X$dimension
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines


  yUnit <- axesSettings$Y$unit
  yDimension <- axesSettings$Y$dimension
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  # function to do obs vs sim
  ddiPlotDataframe <- NULL
  aestheticsList <- list(shape = list(), color = list())

  for (grp in unique(dataframe$groupNumber)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    aestheticsList$color[[grp]] <- metadata$groups[[grp]]$color

    observedRatio <- dataframe[dataframe$group == grp , ]$observedRatio
    simulatedRatio <- dataframe[dataframe$group == grp , ]$simulatedRatio

    xDataDimension <- ""
    xDataUnit <- ""
    xData <- observedRatio

    if (xScaling == "Log") {
      xData <- log10(xData)
    }
    xData <- replaceInfWithNA(xData)

    yDataDimension <- ""
    yDataUnit <- ""
    if (yScaling == "Log") {
      residualValues <- log10(simulatedRatio) - log10(observedRatio)
    } else {
      residualValues <- (simulatedRatio - observedRatio)/observedRatio
    }
    yData <- residualValues
    yData <- replaceInfWithNA(yData)

    df <- list()
    df[[axesSettings$X$label]] <- xData
    df[[axesSettings$Y$label]] <- yData
    df$Group <- grp
    ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, as.data.frame(df))
  }

  ddiPlotDataframe$Group <- as.factor(ddiPlotDataframe$Group)

  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
}



#' @title generateDDIQualificationDDIPlot
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot DDI plot object for DDI qualification workflow
#' @import tlf
#' @import ggplot2
generateDDIQualificationDDIPlot <- function(data) {
  if (data$axesSettings$X$scaling == "Log") {
    xlabel <- bquote(log[10]*.(paste0(data$axesSettings$X$label)))
  } else {
    xlabel <- paste(data$axesSettings$X$label)
  }

  if (data$axesSettings$Y$scaling == "Log") {
    ylabel <- bquote(log[10]*.(paste0(data$axesSettings$Y$label)))
  } else {
    ylabel <- paste(data$axesSettings$Y$label)
  }

  ddiData <- data$ddiPlotDataframe

  ddiDataMapping <- tlf::DDIRatioDataMapping$new(
    x = data$axesSettings$X$label,
    y = data$axesSettings$Y$label,
    shape = "Group",
    color = "Group"
  )

  ddiPlotConfiguration <- tlf::DDIRatioPlotConfiguration$new(
    data = ddiData,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPredictedVsObservedPlot <- tlf::plotDDIRatio(
    data = ddiData,
    plotConfiguration = ddiPlotConfiguration,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPredictedVsObservedPlot <- qualificationDDIPredictedVsObservedPlot + ggplot2::scale_color_manual(values = data$aestheticsList$color)
  qualificationDDIPredictedVsObservedPlot <- qualificationDDIPredictedVsObservedPlot + ggplot2::scale_shape_manual(values = data$aestheticsList$shape)
  qualificationDDIPredictedVsObservedPlot <- qualificationDDIPredictedVsObservedPlot + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)
  qualificationDDIPredictedVsObservedPlot <- qualificationDDIPredictedVsObservedPlot + ggplot2::guides(color = FALSE)
  return(qualificationDDIPredictedVsObservedPlot)
}

#' @title plotQualificationDDIs
#' @description Plot observation vs prediction for DDI qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @return list of qualification DDI ggplot objects
plotQualificationDDIs <- function(configurationPlan,
                                  logFolder = getwd(),
                                  settings) {
  ddiPlotsData <- getQualificationDDIPlotData(configurationPlan)
  ddiPlotList <- list()
  ddiPlotResults <- list()

  for (plotIndex in seq_along(ddiPlotsData)) {
    ddiPlotList[[plotIndex]] <- list()
    dataframe <- ddiPlotsData[[plotIndex]]$dataframe
    metadata <- ddiPlotsData[[plotIndex]]$metadata
    for (plotType in metadata$plotTypes) {

      for (pkParameter in unique(dataframe$pkParameter)){
        plotID <- paste("DDIRatioPlot", plotIndex, plotType , pkParameter , sep = "-")

        plotDDIData <- buildDDIDataFrameFunctions[[plotType]](dataframe[dataframe$pkParameter == pkParameter,], metadata)
        ddiPlotList[[plotIndex]][[plotType]] <- generateDDIQualificationDDIPlot(plotDDIData)

        ddiPlotResults[[plotID]] <- saveTaskResults(
          id = plotID,
          sectionId = metadata$sectionID,
          plot = ddiPlotList[[plotIndex]][[plotType]],
          plotCaption = paste(metadata$title,pkParameter," - ")
        )
      }
    }
  }
  return(ddiPlotResults)
}

#' Names of fields in configuration plane containing axes settings data for each DDI plot type
ddiPlotAxesSettings <- list(
  "predictedVsObserved" = "DDIRatioPlotsPredictedVsObserved",
  "residualsVsObserved" = "DDIRatioPlotsResidualsVsObserved"
)

#' Names of functions for extracting data for each DDI plot type
buildDDIDataFrameFunctions <- list(
  "predictedVsObserved" = buildQualificationDDIPredictedVsObserved,
  "residualsVsObserved" = buildQualificationDDIResidualsVsObserved
)

#'Labels for DDI plot X-axis
plotDDIXLabel <- list(
  "predictedVsObserved" = "Observed",
  "residualsVsObserved" = "Observed"
)

#'Labels for DDI plot Y-axis
plotDDIYLabel <- list(
  "predictedVsObserved" = "Predicted",
  "residualsVsObserved" = "Residual"
)
