#' @title getQualificationDDIPlotData
#' @description Build dataframes and metadata for each DDI plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return  plotDDIdata, a list of lists of the form list(dataframe,metadata) specific to each DID plot
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getQualificationDDIPlotData <- function(configurationPlan) {
  plotDDIdata <- list()
  for (plotNumber in seq_along(configurationPlan$plots$DDIRatioPlots)) {
    plotDDIDataFrame <- NULL
    plotDDIMetadata <- list()
    plot <- configurationPlan$plots$DDIRatioPlots[[plotNumber]]

    plotDDIMetadata$title <- plot$Title
    plotDDIMetadata$sectionID <- plot$SectionId
    plotDDIMetadata$artifacts <- plot$Artifacts
    plotDDIMetadata$plotSettings <- plot

    # Pipes in configuration plan will be deprecated moving forward
    plotDDIMetadata$plotTypes <- plot$PlotTypes %||% ospsuite::toPathArray(plot$PlotType)

    validateIsIncluded(plotDDIMetadata$plotTypes, names(ddiPlotTypeSpecifications))

    plotDDIMetadata$axesSettings <- lapply(plotDDIMetadata$plotTypes, function(plotType) {
      getAxesSettings(configurationPlan$plots$AxesSettings[[ ddiPlotTypeSpecifications[[plotType]]$ddiPlotAxesSettings ]])
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

          observedDataSelection <- observedDataFrame$ID %in% observedDataRecordId

          ratioList[[pkParameter]]$id <- observedDataFrame[["ID"]][observedDataSelection]
          ratioList[[pkParameter]]$studyId <- observedDataFrame[["Study ID"]][observedDataSelection]
          ratioList[[pkParameter]]$mechanism <- observedDataFrame[["Mechanism"]][observedDataSelection]
          ratioList[[pkParameter]]$perpetrator <- observedDataFrame[["Perpetrator"]][observedDataSelection]
          ratioList[[pkParameter]]$routePerpetrator <- observedDataFrame[["Route Perpetrator"]][observedDataSelection]
          ratioList[[pkParameter]]$victim <- observedDataFrame[["Victim"]][observedDataSelection]
          ratioList[[pkParameter]]$routeVictim <- observedDataFrame[["Route Victim"]][observedDataSelection]
          ratioList[[pkParameter]]$dose <- observedDataFrame[["Dose"]][observedDataSelection]
          ratioList[[pkParameter]]$doseUnit <- observedDataFrame[["Dose Unit"]][observedDataSelection]
          ratioList[[pkParameter]]$description <- observedDataFrame[["Description"]][observedDataSelection]
          ratioList[[pkParameter]]$observedRatio <- observedDataFrame[[ddiPKRatioColumnName[[pkParameter]]]][observedDataSelection]

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
            id = ratioList[[pkParameter]]$id,
            studyId = ratioList[[pkParameter]]$studyId,
            mechanism = ratioList[[pkParameter]]$mechanism,
            perpetrator = ratioList[[pkParameter]]$perpetrator,
            routePerpetrator = ratioList[[pkParameter]]$routePerpetrator,
            victim = ratioList[[pkParameter]]$victim,
            routeVictim = ratioList[[pkParameter]]$routeVictim,
            dose = ratioList[[pkParameter]]$dose,
            doseUnit = ratioList[[pkParameter]]$doseUnit,
            description = ratioList[[pkParameter]]$description
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
  axesSettings$X$label <- ddiPlotTypeSpecifications[[plotType]]$plotDDIXLabel(pkParameter)
  axesSettings$Y$label <- ddiPlotTypeSpecifications[[plotType]]$plotDDIYLabel(pkParameter)

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

    yData <- ddiPlotTypeSpecifications[[plotType]]$getYAxisDDIValues(observedRatio, simulatedRatio)
    yData <- replaceInfWithNA(yData)

    df <- list()
    df[[axesSettings$X$label]] <- xData
    df[[axesSettings$Y$label]] <- yData
    df$Group <- grp
    ddiPlotDataframe <- rbind.data.frame(ddiPlotDataframe, as.data.frame(df, check.names = FALSE))
  }

  ddiPlotDataframe$Caption <- sapply(ddiPlotDataframe$Group, function(groupNumber) {
    metadata$groups[[groupNumber]]$caption
  })
  ddiPlotDataframe$Group <- as.factor(ddiPlotDataframe$Group)
  ddiPlotDataframe$Caption <- as.factor(ddiPlotDataframe$Caption)

  return(list(ddiPlotDataframe = ddiPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings, plotSettings = plotSettings))
}



#' @title generateDDIQualificationDDIPlot
#' @description Plot observation vs prediction for qualification workflow
#' @param ddiPlotData a list containing the plot data.frame, aesthetics list, axes settings and plot settings
#' @return ggplot DDI plot object for DDI qualification workflow
#' @import tlf
#' @import ggplot2
#' @keywords internal
generateDDIQualificationDDIPlot <- function(ddiPlotData) {

  ddiData <- na.omit(ddiPlotData$ddiPlotDataframe)

  residualsVsObserved <- ddiPlotTypeSpecifications[[ddiPlotData$axesSettings$plotType]]$residualsVsObservedFlag

  ddiDataMapping <- tlf::DDIRatioDataMapping$new(
    x = ddiPlotData$axesSettings$X$label,
    y = ddiPlotData$axesSettings$Y$label,
    shape = "Caption",
    color = "Caption",
    minRange = c(0.1, 10),
    residualsVsObserved = residualsVsObserved
  )

  ddiPlotConfiguration <- getPlotConfigurationFromPlan(plotProperties = ddiPlotData$plotSettings,
                                                       plotType = "DDIRatio",
                                                       legendPosition = tlf::LegendPositions$outsideRight)

  #Set axes labels
  ddiPlotConfiguration$labels$xlabel$text <- ddiPlotData$axesSettings$X$label
  ddiPlotConfiguration$labels$ylabel$text <- ddiPlotData$axesSettings$Y$label

  # Set line color and type
  ddiPlotConfiguration$lines$color <- "black"
  ddiPlotConfiguration$lines$linetype <- c("solid","dotted","solid")

  # Set axes scaling
  if (ddiPlotData$axesSettings$X$scaling == "Log") {
    ddiPlotConfiguration$xAxis$scale <- tlf::Scaling$log
  }
  if (ddiPlotData$axesSettings$Y$scaling == "Log") {
    ddiPlotConfiguration$yAxis$scale <- tlf::Scaling$log
  }

  # Set y axis ticks and limits
  if (residualsVsObserved & ddiPlotData$axesSettings$Y$scaling == "Log" ) {

    #Minimum log10 predict/observed fold error among all data points, rounded DOWN to nearest whole number
    lowerBoundLog10 <- min(floor(log10(ddiData[[ddiPlotData$axesSettings$Y$label]])))

    #Maximum log10 predict/observed fold error among all data points, rounded UP to nearest whole number
    upperBoundLog10 <- max(ceiling(log10(ddiData[[ddiPlotData$axesSettings$Y$label]])))

    #Maximum log10 scale axis limit given by larger of the two fold error bounds, lowerBoundLog10 and upperBoundLog10
    log10Limit <- max(abs(c(lowerBoundLog10,upperBoundLog10)))

    #Set lower and upper log scale y axis limits to be equal
    ddiPlotConfiguration$yAxis$limits <- 10^(c(-log10Limit, log10Limit))

    #Include ticks at each order of magnitude and at 1/2 and 2
    ddiPlotConfiguration$yAxis$ticks <- 10^seq(-log10Limit, log10Limit, 1)
  }

  qualificationDDIPlot <- tlf::plotDDIRatio(
    data = ddiData,
    plotConfiguration = ddiPlotConfiguration,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_color_manual(values = sapply(ddiPlotData$aestheticsList$color, function(x) {x}))
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_shape_manual(values = sapply(ddiPlotData$aestheticsList$shape, function(x) {x}))

  # Force legend to be only one column to maintain plot panel width, and left-justify legend entries
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::guides(col = guide_legend(ncol = 1, label.hjust = 0))

  return(qualificationDDIPlot)
}

#' @title getQualificationDDIRatioMeasure
#' @description Get qualification measure of DDI ratio from field `DDIRatioPlots` of configuration plan
#' @param summaryDataFrame data.frame with DDI Ratios
#' @param pkParameterName Name of PK Parameter as defined by users
#' @return A data.frame
#' @keywords internal
getQualificationDDIRatioMeasure <- function(summaryDataFrame, pkParameterName) {
  guestValues <- tlf::getGuestValues(x = summaryDataFrame[["observedRatio"]])

  qualificationMeasure <- data.frame(
    parameter = c("Points total", "Points within Guest *et al.*", "Points within 2 fold"),
    number = c(
      nrow(summaryDataFrame),
      measureValuesBetween(summaryDataFrame[["simulatedRatio"]], guestValues$ymin, guestValues$ymax, method = "count"),
      measureValuesBetween(summaryDataFrame[["simulatedObservedRatio"]], 1 / 2, 2, method = "count")
    ),
    ratio = c(
      NA,
      measureValuesBetween(summaryDataFrame[["simulatedRatio"]], guestValues$ymin, guestValues$ymax, method = "percent"),
      measureValuesBetween(summaryDataFrame[["simulatedObservedRatio"]], 1 / 2, 2, method = "percent")
    )
  )
  names(qualificationMeasure) <- c(pkParameterName, "Number", "Ratio [%]")
  return(qualificationMeasure)
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
getDDISection <- function(dataframe, metadata, sectionID, idPrefix, captionSuffix = NULL) {

  ddiArtifacts <- list(
    "Plot" = list(),
    "GMFE" = list(),
    "Measure" = list()
  )

  gmfeDDI <- NULL
  ddiTableList <- list()

  for (pkParameter in unique(dataframe$pkParameter)) {
    for (plotType in metadata$plotTypes) {
      pkDataframe <- dataframe[dataframe$pkParameter == pkParameter, ]
      plotDDIData <- buildQualificationDDIDataframe(pkDataframe, metadata, pkParameter, plotType)

      plotID <- paste("plot", idPrefix, pkParameter, plotType, sep = "-")
      ddiPlot <- generateDDIQualificationDDIPlot(plotDDIData)
      ddiArtifacts[["Plot"]][[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = sectionID,
        plot = ddiPlot,
        plotCaption = ifNotNull(
          condition = captionSuffix,
          outputIfNotNull = paste(metadata$title, captionSuffix, sep = " - "),
          outputIfNull = metadata$title
        )
      )
    }

    ddiSummary <- na.omit(pkDataframe[, c("observedRatio", "simulatedRatio")])
    ddiSummary[["simulatedObservedRatio"]] <- ddiSummary[["simulatedRatio"]] / ddiSummary[["observedRatio"]]
    gmfeDDI <- rbind.data.frame(
      gmfeDDI,
      data.frame(
        "PK parameter" = pkParameter,
        GMFE = calculateGMFE(
          x = ddiSummary$observedRatio,
          y = ddiSummary$simulatedRatio
        ),
        check.names = FALSE
      )
    )

    ddiTableList[[pkParameter]] <- getQualificationDDIRatioMeasure(summaryDataFrame = ddiSummary, pkParameterName = pkParameter)
  }

  gmfeID <- paste("gmfe", idPrefix, sep = "-")
  ddiArtifacts[["GMFE"]][[gmfeID]] <- saveTaskResults(
    id = gmfeID,
    sectionId = sectionID,
    table = gmfeDDI,
    tableCaption = paste("GMFE for", metadata$title, idPrefix),
    includeTable = TRUE
  )


  for (pkParameter in unique(dataframe$pkParameter)) {
    tableID <- paste("table", pkParameter, idPrefix, sep = "-")
    ddiArtifacts[["Measure"]][[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = sectionID,
      table = ddiTableList[[pkParameter]],
      tableCaption = paste("Summary table for", metadata$title, idPrefix, pkParameter),
      includeTable = TRUE
    )
  }

  #Ensure artifacts will appear in same order as in configuration plan
  ddiPlotResults <- unlist(ddiArtifacts[metadata$artifacts])

  return(ddiPlotResults)
}

#' @title getDDITable
#' @description Summary table for DDI
#' @param dataframe for generating DDI summary table
#' @return Summary table for DDI plot
#' @keywords internal
getDDITable <- function(dataframe){

  dataframe$simObsRatio <- dataframe$simulatedRatio / dataframe$observedRatio

  ddiTable <- list()

  pkParameters <- unique(dataframe$pkParameter)

  for (pk in pkParameters){
    pkDataframe <- dataframe[dataframe$pkParameter == pk,]

    ddiTable[[pk]] <- data.frame(
      "DataID" = pkDataframe$id,
      "Perpetrator" = paste(pkDataframe$perpetrator,paste(pkDataframe$dose,pkDataframe$doseUnit),pkDataframe$routePerpetrator,pkDataframe$description,sep = ", "),
      "Victim" =  paste(pkDataframe$victim , pkDataframe$routeVictim, sep = ", ")
    )

    ddiTable[[pk]][[paste("Predicted",pk,"Ratio")]] <- pkDataframe$simulatedRatio
    ddiTable[[pk]][[paste("Observed",pk,"Ratio")]] <- pkDataframe$observedRatio
    ddiTable[[pk]][[paste("Pred/Obs",pk,"Ratio")]] <- pkDataframe$simObsRatio
    ddiTable[[pk]][["Reference"]] <- pkDataframe$studyId
  }

  #Merge together all dataframes (each of which corresponds to a different PK parameter) by combining together all rows that share common values in the columns named "DataID","Perpetrator","Victim","Reference"
  mergedDDITable <- Reduce(
    function(x, y) merge(x, y, by=c("DataID","Perpetrator","Victim","Reference")),
    ddiTable
  )

  #Move reference column to the end
  mergedDDITable <- cbind(mergedDDITable[, names(mergedDDITable) != "Reference"], data.frame("Reference" = mergedDDITable$Reference))

  #Order rows by Data ID
  mergedDDITable <- mergedDDITable[order(mergedDDITable$DataID),]

  return(mergedDDITable)
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
  for (plotIndex in seq_along(ddiData)) {
    dataframe <- ddiData[[plotIndex]]$dataframe
    metadata <- ddiData[[plotIndex]]$metadata
    sectionID <- metadata$sectionID
    sectionLevel <- configurationPlan$getSectionLevel(id = sectionID)
    idPrefix <- paste("DDIRatio", plotIndex, sep = "-")
    ddiResults <- c(ddiResults, getDDISection(dataframe, metadata, sectionID, idPrefix))

    if ("Table" %in% metadata$artifacts){

      ddiTable <- saveTaskResults(
        id = "DDI Table",
        sectionId = sectionID,
        table = getDDITable(dataframe),
        tableCaption = NULL,
        includeTable = TRUE
      )
      ddiResults <- c(ddiResults, ddiTable)
    }

    for (subplotType in names(ddiSubplotTypes)) {
      subheading <- saveTaskResults(id = subplotType, sectionId = sectionID, textChunk = paste(paste0(rep("#", sectionLevel + 1), collapse = ""), ddiSubplotTypes[[subplotType]]), includeTextChunk = TRUE)
      ddiResults <- c(ddiResults, subheading)
      subplotTypeLevels <- unique(dataframe[[subplotType]])
      for (subplotTypeLevel in subplotTypeLevels) {
        subsubheading <- saveTaskResults(id = paste(subplotType, subplotTypeLevel, sep = " - "), sectionId = sectionID, textChunk = paste(paste0(rep("#", sectionLevel + 2), collapse = ""), subplotTypeLevel), includeTextChunk = TRUE)
        ddiResults <- c(ddiResults, subsubheading)
        subplotDataframe <- droplevels(dataframe[dataframe[[subplotType]] == subplotTypeLevel, ])
        sectionID <- metadata$sectionID
        idPrefix <- paste("DDIRatio", plotIndex, subplotType, subplotTypeLevel, sep = "-")
        ddiResults <- c(ddiResults, getDDISection(subplotDataframe, metadata, sectionID, idPrefix, captionSuffix = subplotTypeLevel))
      }
    }
  }

  return(ddiResults)
}



#' Specifications of each DDI plot type
#' @keywords internal
ddiPlotTypeSpecifications <- list(
  predictedVsObserved = list(ddiPlotAxesSettings = "DDIRatioPlotsPredictedVsObserved",
                             plotDDIXLabel = function(pk) {
                               return(paste("Observed", pk, "Ratio"))
                             },
                             plotDDIYLabel = function(pk) {
                               return(paste("Predicted", pk, "Ratio"))
                             },
                             residualsVsObservedFlag = FALSE,
                             getYAxisDDIValues = function(observedRatio, simulatedRatio) {
                               return(simulatedRatio)
                             }

  ),
  residualsVsObserved = list(ddiPlotAxesSettings = "DDIRatioPlotsResidualsVsObserved",
                             plotDDIXLabel = function(pk) {
                               return(paste("Observed", pk, "Ratio"))
                             },
                             plotDDIYLabel = function(pk) {
                               return(paste("Predicted", pk, "Ratio / Observed", pk, "Ratio"))
                             },
                             residualsVsObservedFlag = TRUE,
                             getYAxisDDIValues = function(observedRatio, simulatedRatio) {
                               return(simulatedRatio / observedRatio)
                             }
  )
)


#' Named list of of DDI subplot types
#' @keywords internal
ddiSubplotTypes <- list(
  "mechanism" = "Mechanism",
  "perpetrator" = "Perpetrator",
  "victim" = "Victim"
)


#' Column names to check in observed data based on Configuration Plan PK Parameter
#' @keywords internal
ddiPKRatioColumnName <- list(
  "AUC" = "AUCR Avg",
  "AUC_tEnd" = "AUCR Avg",
  "CMAX" = "CmaxR Avg",
  "C_max" = "CmaxR Avg"
)
