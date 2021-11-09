#' @title getQualificationDDIPlotData
#' @description Build dataframes and metadata for each DDI plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return  plotDDIdata, a list of lists of the form list(dataframe,metadata) specific to each DID plot
#' @keywords internal
getQualificationDDIPlotData <- function(configurationPlan) {
  plotDDIdata <- list()
  for (plotNumber in seq_along(configurationPlan$plots$DDIRatioPlots)) {
    plotDDIDataFrame <- NULL
    plotDDIMetadata <- list()
    plot <- configurationPlan$plots$DDIRatioPlots[[plotNumber]]

    plotDDIMetadata$title <- plot$Title
    plotDDIMetadata$sectionID <- plot$SectionId
    plotDDIMetadata$plotSettings <- getPlotSettings(configurationPlan$plots$PlotSettings)
    # Pipes in configuration plan will be deprecated moving forward

    plotDDIMetadata$plotTypes <- plot$PlotTypes %||% ospsuite::toPathArray(plot$PlotType)
    plotDDIMetadata$axesSettings <- lapply(plotDDIMetadata$plotTypes, function(pltType) {
      getAxesSettings(configurationPlan$plots$AxesSettings[[ddiPlotAxesSettings[[pltType]]]])
    })
    names(plotDDIMetadata$axesSettings) <- plotDDIMetadata$plotTypes


    plotDDIMetadata$groups <- list()

    pkParameters <- NULL
    if (!is.null(plot$PKParameter)) {
      pkParameters <- toPathArray(plot$PKParameter)
    }

    for (groupNumber in seq_along(plot$Groups)) {
      group <- plot$Groups[[groupNumber]]
      plotDDIMetadata$groups[[groupNumber]] <- list()
      plotDDIMetadata$groups[[groupNumber]]$caption <- group$Caption
      plotDDIMetadata$groups[[groupNumber]]$color <- group$Color
      plotDDIMetadata$groups[[groupNumber]]$symbol <- tlfShape(group$Symbol)

      for (ddiRatio in group$DDIRatios) {
        outputPath <- ddiRatio$Output
        observedDataSet <- ddiRatio$ObservedData
        observedDataSetFilePath <- configurationPlan$getObservedDataPath(id = observedDataSet)
        observedDataRecordId <- ddiRatio$ObservedDataRecordId
        observedDataFrame <- readObservedDataFile(file = observedDataSetFilePath)
        validateIsIncluded(observedDataRecordId, observedDataFrame$ID)

        ratioList <- list()

        for (pkParameter in pkParameters) {
          ratioList[[pkParameter]] <- list()

          validateIsIncluded(ddiPKRatioColumnName[[pkParameter]], names(observedDataFrame))
          ratioList[[pkParameter]]$observedRatio <- observedDataFrame[[ddiPKRatioColumnName[[pkParameter]]]][observedDataFrame$ID %in% observedDataRecordId]
          ratioList[[pkParameter]]$mechanism <- observedDataFrame[["Mechanism"]][observedDataFrame$ID %in% observedDataRecordId]
          ratioList[[pkParameter]]$perpetrator <- observedDataFrame[["Perpetrator"]][observedDataFrame$ID %in% observedDataRecordId]
          ratioList[[pkParameter]]$victim <- observedDataFrame[["Victim"]][observedDataFrame$ID %in% observedDataRecordId]

          for (simulationType in c("SimulationControl", "SimulationDDI")) {
            plotComponent <- ddiRatio[[simulationType]]
            projectName <- plotComponent$Project
            simulationName <- plotComponent$Simulation

            startTime <- NULL
            endTime <- NULL

            if (is.numeric(plotComponent$StartTime)) {
              startTime <- ospsuite::toBaseUnit(
                quantityOrDimension = ospDimensions$Time,
                values = plotComponent$StartTime,
                unit = plotComponent$TimeUnit
              )
            }

            if (is.numeric(plotComponent$EndTime)) {
              endTime <- ospsuite::toBaseUnit(
                quantityOrDimension = ospDimensions$Time,
                values = plotComponent$EndTime,
                unit = plotComponent$TimeUnit
              )
            }
            pkParameterName <- generateDDIPlotPKParameterName(pkParameter, startTime, endTime)

            simulationResultsFile <- configurationPlan$getSimulationResultsPath(
              project = projectName,
              simulation = simulationName
            )

            pkAnalysisResultsPath <- configurationPlan$getPKAnalysisResultsPath(
              project = projectName,
              simulation = simulationName
            )

            simulationFile <- configurationPlan$getSimulationPath(
              project = projectName,
              simulation = simulationName
            )

            simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
            pkAnalysisResults <- ospsuite::importPKAnalysesFromCSV(
              filePath = pkAnalysisResultsPath,
              simulation = simulation
            )

            ratioList[[pkParameter]][[simulationType]] <- pkAnalysisResults$pKParameterFor(
              quantityPath = outputPath,
              pkParameter = pkParameterName
            )$values
          }

          df <- data.frame(
            project = projectName,
            simulation = simulationName,
            groupNumber = groupNumber,
            outputPath = outputPath,
            pkParameter = pkParameter,
            observedRatio = ratioList[[pkParameter]]$observedRatio,
            simulatedRatio = ratioList[[pkParameter]][["SimulationDDI"]] / ratioList[[pkParameter]][["SimulationControl"]],
            mechanism = ratioList[[pkParameter]]$mechanism,
            perpetrator = ratioList[[pkParameter]]$perpetrator,
            victim = ratioList[[pkParameter]]$victim
          )

          plotDDIDataFrame <- rbind.data.frame(plotDDIDataFrame, df)
        }
      }
    }

    plotDDIdata[[plotNumber]] <- list(
      dataframe = plotDDIDataFrame,
      metadata = plotDDIMetadata
    )
  }
  return(plotDDIdata)
}



#' @title buildQualificationDDIDataframe
#' @description Build dataframe for DDI
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @param pkParameter for which DDI ratios are to be evaluated
#' @param plotType for which DDI ratios are to be evaluated.  `plotType` is either `predictedVsObserved` or `residualsVsObserved`.
#' @return dataframe for plotting goodness of fit of residuals vs time type
#' @keywords internal
buildQualificationDDIDataframe <- function(dataframe,
                                           metadata,
                                           pkParameter,
                                           plotType) {
  plotSettings <- metadata$plotSettings
  axesSettings <- metadata$axesSettings[[plotType]]
  axesSettings$plotType <- plotType
  axesSettings$X$label <- plotDDIXLabel[[plotType]](pkParameter)
  axesSettings$Y$label <- plotDDIYLabel[[plotType]](pkParameter)

  ddiPlotDataframe <- NULL
  aestheticsList <- list(shape = list(), color = list())

  for (grp in unique(dataframe$groupNumber)) {
    caption <- metadata$groups[[grp]]$caption
    aestheticsList$shape[[caption]] <- metadata$groups[[grp]]$symbol
    aestheticsList$color[[caption]] <- metadata$groups[[grp]]$color

    observedRatio <- dataframe[dataframe$group == grp, ]$observedRatio
    simulatedRatio <- dataframe[dataframe$group == grp, ]$simulatedRatio

    xDataDimension <- ""
    xDataUnit <- ""
    xData <- observedRatio
    xData <- replaceInfWithNA(xData)

    yDataDimension <- ""
    yDataUnit <- ""
    yData <- getYAxisDDIValues[[plotType]](observedRatio,simulatedRatio)
    yData <- replaceInfWithNA(yData)

    df <- list()
    df[[axesSettings$X$label]] <- xData
    df[[axesSettings$Y$label]] <- yData
    df$Group <- grp
    ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, as.data.frame(df,check.names = FALSE))
  }

  ddiPlotDataframe$Caption <- sapply(ddiPlotDataframe$Group,function(groupNumber){metadata$groups[[groupNumber]]$caption})
  ddiPlotDataframe$Group <- as.factor(ddiPlotDataframe$Group)
  ddiPlotDataframe$Caption <- as.factor(ddiPlotDataframe$Caption)

  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings, plotSettings = plotSettings))
}



#' @title generateDDIQualificationDDIPlot
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot DDI plot object for DDI qualification workflow
#' @import tlf
#' @import ggplot2
#' @keywords internal
generateDDIQualificationDDIPlot <- function(data) {

  ddiData <- na.omit(data$ddiPlotDataframe)

  ddiDataMapping <- tlf::DDIRatioDataMapping$new(
    x = data$axesSettings$X$label,
    y = data$axesSettings$Y$label,
    shape = "Caption",
    color = "Caption",
    minRange = c(0.1, 10),
    residualsVsObserved = residualsVsObservedFlag[[data$axesSettings$plotType]]
  )

  ddiPlotConfiguration <- tlf::DDIRatioPlotConfiguration$new(
    data = ddiData,
    dataMapping = ddiDataMapping
  )

  ddiPlotConfiguration$labels$xlabel$font$size <- data$plotSettings$axisFontSize
  ddiPlotConfiguration$labels$ylabel$font$size <- data$plotSettings$axisFontSize
  ddiPlotConfiguration$background$watermark$font$size <- data$plotSettings$watermarkFontSize
  ddiPlotConfiguration$legend$font$size <- data$plotSettings$legendFontSize

  qualificationDDIPlot <- tlf::plotDDIRatio(
    data = ddiData,
    plotConfiguration = ddiPlotConfiguration,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_color_manual(values = sapply(data$aestheticsList$color,function(x){x}))
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_shape_manual(values = sapply(data$aestheticsList$shape,function(x){x}))

  xlabel <- paste(data$axesSettings$X$label)
  ylabel <- paste(data$axesSettings$Y$label)

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)

  if (data$axesSettings$X$scaling == "Log") {
    qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_x_continuous(trans='log10',labels = function(x) format(x, scientific = FALSE))
  }

  if (data$axesSettings$Y$scaling == "Log") {
    qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_y_continuous(trans='log10',labels = function(x) format(x, scientific = FALSE),)
  }

  return(qualificationDDIPlot)
}

#' @title plotQualificationDDIs
#' @description Plot observation vs prediction for DDI qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder Folder where logs are saved
#' @param settings A `TaskSettings` object
#' @return list of qualification DDI ggplot objects
#' @keywords internal
plotQualificationDDIs <- function(configurationPlan,
                                  logFolder = getwd(),
                                  settings) {

  ddiPlotsData <- getQualificationDDIPlotData(configurationPlan)
  ddiPlotResults <- list()
  subplotTypes <- c("mechanism", "perpetrator", "victim")

  for (plotIndex in seq_along(ddiPlotsData)) {

    dataframe <- ddiPlotsData[[plotIndex]]$dataframe
    metadata <- ddiPlotsData[[plotIndex]]$metadata
    for (plotType in metadata$plotTypes) {
      for (pkParameter in unique(dataframe$pkParameter)) {

        pkDataframe <- dataframe[dataframe$pkParameter == pkParameter, ]

        plotDDIData <- buildQualificationDDIDataframe(pkDataframe, metadata, pkParameter, plotType)
        plotID <- paste("DDIRatioPlot", plotIndex, plotType, pkParameter,"all", sep = "-")
        ddiPlot <- generateDDIQualificationDDIPlot(plotDDIData)
        ddiPlotResults[[plotID]] <- saveTaskResults(
          id = plotID,
          sectionId = metadata$sectionID,
          plot = ddiPlot,
          plotCaption = paste(metadata$title, " - ", pkParameter)
        )


        for (subplotType in subplotTypes){
          subplotTypeLevels <- unique(pkDataframe[[subplotType]])
          for (subplotTypeLevel in subplotTypeLevels){
            subplotDataframe <- droplevels(pkDataframe[pkDataframe[[subplotType]] == subplotTypeLevel,])
            plotDDIData <- buildQualificationDDIDataframe(subplotDataframe, metadata, pkParameter, plotType)
            plotID <- paste("DDIRatioPlot", plotIndex, plotType, pkParameter, subplotType, subplotTypeLevel, sep = "-")
            ddiSubplot <- generateDDIQualificationDDIPlot(plotDDIData)
            ddiPlotResults[[plotID]] <- saveTaskResults(
              id = plotID,
              sectionId = metadata$sectionID,
              plot = ddiSubplot,
              plotCaption = paste(metadata$title, pkParameter , subplotType, subplotTypeLevel , sep = "-")
            )
          }
        }

      }
    }
  }
  return(ddiPlotResults)
}

#' Names of fields in configuration plane containing axes settings data for each DDI plot type
#' @keywords internal
ddiPlotAxesSettings <- list(
  "predictedVsObserved" = "DDIRatioPlotsPredictedVsObserved",
  "residualsVsObserved" = "DDIRatioPlotsResidualsVsObserved"
)

#' Labels for DDI plot X-axis
#' @keywords internal
plotDDIXLabel <- list(
  "predictedVsObserved" = function(pk){return(paste("Observed",pk,"Ratio"))},
  "residualsVsObserved" = function(pk){return(paste("Observed",pk,"Ratio"))}
)

#' Labels for DDI plot Y-axis
#' @keywords internal
plotDDIYLabel <- list(
  "predictedVsObserved" = function(pk){return(paste("Predicted",pk,"Ratio"))},
  "residualsVsObserved" = function(pk){return(paste("Predicted",pk,"Ratio / Observed",pk,"Ratio"))}
)

#' DDI plot type identifier
#' @keywords internal
residualsVsObservedFlag <- list(
  "predictedVsObserved" = FALSE,
  "residualsVsObserved" = TRUE
)

#' Computation of Y-Axis values
#' @keywords internal
getYAxisDDIValues <- list(
  "predictedVsObserved" = function(observedRatio,simulatedRatio){return(simulatedRatio)},
  "residualsVsObserved" = function(observedRatio,simulatedRatio){return(simulatedRatio / observedRatio)}
)
