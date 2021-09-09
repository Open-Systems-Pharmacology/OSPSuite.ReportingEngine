#' @title getQualificationDDIPlotData
#' @description Build dataframes and metadata for each DDI plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return  plotDDIDataFrame, a list of lists of the form list(dataframe,metadata) specific to each DID plot
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
      plotDDIMetadata$groups[[groupNumber]]$symbol <- group$Symbol

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
  axesSettings <- metadata$axesSettings[["predictedVsObserved"]]

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
  uniqueGroupPKParameterID <- 0
  for (grp in unique(dataframe$groupNumber)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    aestheticsList$color[[grp]] <- metadata$groups[[grp]]$color

    for (pkParameter in unique(dataframe[dataframe$group == grp, ]$pkParameter )) {

      uniqueGroupPKParameterID <- uniqueGroupPKParameterID + 1

      xDataDimension <- ""
      xDataUnit <- ""
      xData <- dataframe[dataframe$group == grp & dataframe$pkParameter == pkParameter, ]$observedRatio

      if (xScaling == "Log") {
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      yDataDimension <- ""
      yDataUnit <- ""
      yData <- dataframe[dataframe$group == grp & dataframe$pkParameter == pkParameter, ]$simulatedRatio
      if (yScaling == "Log") {
        yData <- log10(yData)
      }
      yData <- replaceInfWithNA(yData)

      df <- data.frame(
        Observed = xData,
        Simulated = yData,
        Group = grp,
        pkParameter = pkParameter,
        uniqueGroupPKParameterID = uniqueGroupPKParameterID
      )

      df$uniqueGroupPKParameterID <- as.factor(df$uniqueGroupPKParameterID)

      ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, df)
    }
  }
  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
}




#' #' @title buildQualificationGOFResidualsVsObserved
#' #' @description Build dataframe for residuals vs observed
#' #' @param dataframe data.frame
#' #' @param metadata meta data on `data`
#' #' @return dataframe for plotting goodness of fit of residuals vs time type
buildQualificationDDIResidualsVsObserved <- function(dataframe,
                                                     metadata) {
  axesSettings <- metadata$axesSettings[["residualsVsObserved"]]

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
  uniqueGroupPKParameterID <- 0
  for (grp in unique(dataframe$groupNumber)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    aestheticsList$color[[grp]] <- metadata$groups[[grp]]$color

    for (pkParameter in unique(dataframe[dataframe$group == grp, ]$pkParameter )) {

      observedRatio <- dataframe[dataframe$group == grp & dataframe$pkParameter == pkParameter, ]$observedRatio
      simulatedRatio <- dataframe[dataframe$group == grp & dataframe$pkParameter == pkParameter, ]$simulatedRatio

      uniqueGroupPKParameterID <- uniqueGroupPKParameterID + 1

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

      df <- data.frame(
        Observed = xData,
        Residual = yData,
        Group = grp,
        pkParameter = pkParameter,
        uniqueGroupPKParameterID = uniqueGroupPKParameterID
      )

      df$uniqueGroupPKParameterID <- as.factor(df$uniqueGroupPKParameterID)

      ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, df)
    }
  }
  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
}



#' @title plotQualificationDDIPredictedVsObserved
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationDDIPredictedVsObserved <- function(data) {
  if (data$axesSettings$X$scaling == "Log") {
    xlabel <- bquote(log[10]*.(paste0("(Observed ",data$axesSettings$X$unit,")")))
  } else {
    xlabel <- paste("Observed",data$axesSettings$X$unit)
  }

  if (data$axesSettings$Y$scaling == "Log") {
    ylabel <- bquote(log[10]*.(paste0("(Predicted ",data$axesSettings$Y$unit,")")))
  } else {
    ylabel <- paste("Predicted",data$axesSettings$Y$unit)
  }

  gofData <- data$gofPlotDataframe

  gofDataMapping <- tlf::ObsVsPredDataMapping$new(
    x = "Observed",
    y = "Simulated",
    shape = "Group",
    color = "uniqueGroupOutputMappingID"
  )

  gofPlotConfiguration <- tlf::ObsVsPredPlotConfiguration$new(
    data = gofData,
    dataMapping = gofDataMapping
  )

  qualificationGOFPredictedVsObservedPlot <- tlf::plotObsVsPred(
    data = gofData,
    dataMapping = gofDataMapping,
    plotConfiguration = gofPlotConfiguration
  )

  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::scale_color_manual(values = data$aestheticsList$color)
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::scale_shape_manual(values = data$aestheticsList$shape)
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::guides(color = FALSE)
  return(qualificationGOFPredictedVsObservedPlot)
}

#'
#' #' @title plotQualificationDDIResidualsVsObserved
#' #' @description Plot DDI residual vs observation for qualification workflow
#' #' @param data data.frame
#' #' @return ggplot object of residuals over time profile for qualification workflow
#' #' @import tlf
#' #' @import ggplot2
#' plotQualificationDDIResidualsVsObserved <- function(data) {
#'   maxRes <- 1.2 * max(abs(data$gofPlotDataframe[, "Residuals"]), na.rm = TRUE)
#'
#'   resVsTimeDataMapping <- tlf::ResVsPredDataMapping$new(
#'     x = "Time",
#'     y = "Residuals",
#'     shape = "Group",
#'     color = "uniqueGroupOutputMappingID"
#'   )
#'
#'   metaData <- NULL
#'
#'   gofPlotConfiguration <- tlf::ResVsPredPlotConfiguration$new(
#'     data = data$gofPlotDataframe,
#'     metaData = metaData,
#'     dataMapping = resVsTimeDataMapping
#'   )
#'
#'   gofPlotConfiguration$labels$ylabel$font$angle <- 90
#'   gofPlotConfiguration$labels$xlabel$text <- paste0("Time (",data$axesSettings$X$unit,")")
#'   gofPlotConfiguration$labels$ylabel$text <- paste0("Residuals")
#'   gofPlotConfiguration$legend$position <- "outsideRight"
#'
#'   qualificationGOFResVsTimePlot <- tlf::plotResVsPred(data = data$gofPlotDataframe,
#'                                                       metaData = metaData,
#'                                                       dataMapping = resVsTimeDataMapping,
#'                                                       plotConfiguration = gofPlotConfiguration)
#'
#'   qualificationGOFResVsTimePlot <- tlf::setYAxis(
#'     plotObject = qualificationGOFResVsTimePlot,
#'     limits = c(-maxRes, maxRes)
#'   )
#'
#'   qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::scale_color_manual(values = data$aestheticsList$color)
#'   qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::scale_shape_manual(values = data$aestheticsList$shape)
#'   qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::guides(color = FALSE)
#'   return(qualificationGOFResVsTimePlot)
#' }
#'
#'
#'
#' @title plotQualificationGOFs
#' @description Plot observation vs prediction for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @return list of qualification GOF ggplot objects
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

        plotDDIDataframe <- buildGOFDataFrameFunctions[[plotType]](dataframe[dataframe$pkParameter == pkParameter,], metadata)
        ddiPlotList[[plotIndex]][[plotType]] <- plotDDIFunctions[[plotType]](plotDDIDataframe)

        ddiPlotResults[[plotID]] <- saveTaskResults(
          id = plotID,
          sectionId = metadata$sectionID,
          plot = ddiPlotList[[plotIndex]][[plotType]],
          plotCaption = metadata$title
        )
      }
    }
  }
  return(ddiPlotResults)
}

#' Names of fields in configuration plane containing axes settings data for each GOF plot type
ddiPlotAxesSettings <- list(
  "predictedVsObserved" = "DDIRatioPlotsPredictedVsObserved",
  "residualsVsObserved" = "DDIRatioPlotsResidualsVsObserved"
)

#' Names of functions for extracting data for each DDI plot type
buildDDIDataFrameFunctions <- list(
  "predictedVsObserved" = buildQualificationDDIPredictedVsObserved,
  "residualsOverTime" = buildQualificationDDIResidualsVsObserved
)

#' Names of functions for plotting DDI plots for each DDI plot type
plotDDIFunctions <- list(
   "predictedVsObserved" = plotQualificationDDIPredictedVsObserved,
   "residualsOverTime" = plotQualificationDDIResidualsVsObserved
)
