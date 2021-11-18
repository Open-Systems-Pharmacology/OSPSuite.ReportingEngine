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
            pkParameterName = pkParameterName,
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

    xData <- observedRatio
    xData <- replaceInfWithNA(xData)

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

  ddiPlotConfiguration$export$width <- 2*1.6*(data$plotSettings$width/96)
  ddiPlotConfiguration$export$height <- 2*1.2*(data$plotSettings$height/96)
  ddiPlotConfiguration$export$units <- "in"

  ddiPlotConfiguration$labels$xlabel$font$size <- 2*data$plotSettings$axisFontSize
  ddiPlotConfiguration$labels$ylabel$font$size <- 2*data$plotSettings$axisFontSize
  ddiPlotConfiguration$background$watermark$font$size <- 2*data$plotSettings$watermarkFontSize
  ddiPlotConfiguration$legend$font$size <- 2*data$plotSettings$legendFontSize

  qualificationDDIPlot <- tlf::plotDDIRatio(
    data = ddiData,
    plotConfiguration = ddiPlotConfiguration,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_color_manual(values = sapply(data$aestheticsList$color,function(x){x}))
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_shape_manual(values = sapply(data$aestheticsList$shape,function(x){x}))
  qualificationDDIPlot  <- qualificationDDIPlot + guides(col = guide_legend(ncol = 1,label.hjust = 0))

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


#' @title getDDISummaryTable
#' @description Plot observation vs prediction for DDI qualification workflow
#' @param summaryDataFrame of DDI data for current DDI section
#' @param pkParameter for current DDI section
#' @return A `data.frame` of DDI ratio summaries
#' @keywords internal
getDDISummaryTable <- function(summaryDataFrame,pkParameter){

  guestValues <- tlf::getGuestValues(x = summaryDataFrame[["observedRatio"]])
  summaryDataFrame[["guestLowerBound"]] <- guestValues$ymin
  summaryDataFrame[["guestUpperBound"]] <- guestValues$ymax
  summaryDataFrame[["withinTwoFold"]] <- sapply(summaryDataFrame[["simulatedObservedRatio"]],function(ratio){
    withinLimits <- 0
    if( ratio > 0.5 & ratio < 2){
      withinLimits <- 1
    }
    return(withinLimits)
  })

  summaryDataFrame[["withinGuest"]] <- sapply(seq_along(summaryDataFrame[["simulatedRatio"]]),function(rowNumber){
    withinLimits <- 0
    if( summaryDataFrame[["simulatedRatio"]][rowNumber] > summaryDataFrame[["guestLowerBound"]][rowNumber] & summaryDataFrame[["simulatedRatio"]][rowNumber] < summaryDataFrame[["guestUpperBound"]][rowNumber]){
      withinLimits <- 1
    }
    return(withinLimits)
  })

  pointsTotal <- nrow(summaryDataFrame)
  numberWithinGuest <- sum(summaryDataFrame[["withinGuest"]])
  numberWithinTwoFold <- sum(summaryDataFrame[["withinTwoFold"]])

  ddiTable <- list()
  ddiTable[[pkParameter]] <- c("Points total","Points within Guest et al.","Points within 2-fold")
  ddiTable[["Number"]] <- c(pointsTotal,numberWithinGuest,numberWithinTwoFold)
  ddiTable[["Ratio [%]"]] <- c("-",round(100*numberWithinGuest/pointsTotal,2), round(100*numberWithinTwoFold/pointsTotal,2))
  return(as.data.frame(ddiTable,check.names = FALSE))
}

#' @title getDDISection
#' @description Plot observation vs prediction for DDI qualification workflow
#' @param dataframe of DDI data for current DDI section
#' @param metadata for DDI plots
#' @param sectionID of report
#' @param idPrefix unique ID to index task results
#' @param captionSuffix to append to qualification plan title
#' @return a `list` of DDI results for the current DDI section
#' @keywords internal
getDDISection <- function(dataframe,metadata,sectionID,idPrefix,captionSuffix = NULL){
  ddiPlotResults <- list()
  gmfeDDI <- NULL
  ddiTableList <- list()
  for (pkParameter in unique(dataframe$pkParameter)) {
    for (plotType in metadata$plotTypes) {
      pkDataframe <- dataframe[dataframe$pkParameter == pkParameter, ]
      plotDDIData <- buildQualificationDDIDataframe(pkDataframe, metadata, pkParameter, plotType)

      plotID <- paste("plot",idPrefix, pkParameter, plotType, sep = "-")
      ddiPlot <- generateDDIQualificationDDIPlot(plotDDIData)
      ddiPlotResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = sectionID,
        plot = ddiPlot,
        plotCaption = ifnotnull(inputToCheck = captionSuffix,
                                outputIfNotNull = paste(metadata$title, captionSuffix,sep = " - "),
                                outputIfNull = metadata$title)
      )
    }

    ddiSummary <- na.omit(pkDataframe[,c("observedRatio","simulatedRatio")])
    ddiSummary[["simulatedObservedRatio"]] <- ddiSummary[["simulatedRatio"]]/ddiSummary[["observedRatio"]]
    gmfeDDI <- rbind.data.frame(gmfeDDI,
                                data.frame("PK parameter" = pkParameter,
                                           GMFE = calculateGMFE(x = ddiSummary$observedRatio,
                                                                y = ddiSummary$simulatedRatio),
                                           check.names = FALSE))


    ddiTableList[[pkParameter]] <- getDDISummaryTable(summaryDataFrame = ddiSummary,pkParameter = pkParameter)

  }

  gmfeID <- paste("gmfe",idPrefix,sep = "-")
  ddiPlotResults[[gmfeID]] <- saveTaskResults(
    id = gmfeID,
    sectionId = sectionID,
    table = gmfeDDI,
    tableCaption = paste("GMFE for", metadata$title,idPrefix),
    includeTable = TRUE
  )

  for (pkParameter in unique(dataframe$pkParameter)){
    tableID <- paste("table",pkParameter,idPrefix,sep = "-")
    ddiPlotResults[[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = sectionID,
      table = ddiTableList[[pkParameter]],
      tableCaption = paste("Summary table for", metadata$title,idPrefix,pkParameter),
      includeTable = TRUE
    )
  }

  return(ddiPlotResults)

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


  ddiData <- getQualificationDDIPlotData(configurationPlan)

  ddiResults <- list()
  subsectionLevel1Counter <- 0
  subsectionLevel2Counter <- 0
  for (plotIndex in seq_along(ddiData)) {

    dataframe <- ddiData[[plotIndex]]$dataframe
    metadata <- ddiData[[plotIndex]]$metadata
    sectionID <- metadata$sectionID
    idPrefix <-  paste("DDIRatio",plotIndex,"all",sep = "-")
    ddiResults <- c(ddiResults,getDDISection(dataframe,metadata,sectionID,idPrefix))

    for (subplotType in ddiSubplotTypes){
      subsectionLevel1Counter <- subsectionLevel1Counter + 1
      subplotTypeLevels <- unique(dataframe[[subplotType]])
      for (subplotTypeLevel in subplotTypeLevels){
        subsectionLevel2Counter <- subsectionLevel2Counter + 1
        subplotDataframe <- droplevels(dataframe[dataframe[[subplotType]] == subplotTypeLevel,])
        sectionID <- metadata$sectionID#paste0(metadata$sectionID,subsectionLevel1Counter,subsectionLevel2Counter)
        idPrefix <- paste("DDIRatio", plotIndex, subplotType, subplotTypeLevel, sep = "-")
        ddiResults <- c(ddiResults,getDDISection(subplotDataframe,metadata,sectionID,idPrefix,captionSuffix = subplotTypeLevel))
      }
      subsectionLevel2Counter <- 0
    }
    subsectionLevel1Counter <- 0

  }

  return(ddiResults)

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

#' Names of DDI subplot types
#' @keywords internal
ddiSubplotTypes <- c("mechanism", "perpetrator", "victim")
