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

    yData <- getYAxisDDIValues[[plotType]](observedRatio, simulatedRatio)
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

  ddiPlotConfiguration$export$width <- 2 * 1.6 * (data$plotSettings$width / 96)
  ddiPlotConfiguration$export$height <- 2 * 1.2 * (data$plotSettings$height / 96)
  ddiPlotConfiguration$export$units <- "in"

  # Set axis label font size
  ddiPlotConfiguration$labels$xlabel$font$size <- 2 * data$plotSettings$axisFontSize
  ddiPlotConfiguration$labels$ylabel$font$size <- 2 * data$plotSettings$axisFontSize

  # Set axis tick font size
  ddiPlotConfiguration$xAxis$font$size <- 2 * data$plotSettings$axisFontSize
  ddiPlotConfiguration$yAxis$font$size <- 2 * data$plotSettings$axisFontSize

  # Set watermark font size
  ddiPlotConfiguration$background$watermark$font$size <- 2 * data$plotSettings$watermarkFontSize

  # Set legend font size
  ddiPlotConfiguration$legend$font$size <- 2 * data$plotSettings$legendFontSize

  # Set line color and type
  ddiPlotConfiguration$lines$color <- "black"
  ddiPlotConfiguration$lines$linetype <- c("solid","dotted","solid")

  # Set axes scaling
  if (data$axesSettings$X$scaling == "Log") {
    ddiPlotConfiguration$xAxis$scale <- tlf::Scaling$log
  }
  if (data$axesSettings$Y$scaling == "Log") {
    ddiPlotConfiguration$yAxis$scale <- tlf::Scaling$log
  }

  # Set y axis ticks and limits
  if (residualsVsObservedFlag[[data$axesSettings$plotType]] & data$axesSettings$Y$scaling == "Log" ) {

    #Minimum log10 predict/observed fold error among all data points, rounded DOWN to nearest whole number
    lowerBoundLog10 <- min(floor(log10(ddiData[[data$axesSettings$Y$label]])))

    #Maximum log10 predict/observed fold error among all data points, rounded UP to nearest whole number
    upperBoundLog10 <- max(ceiling(log10(ddiData[[data$axesSettings$Y$label]])))

    #Maximum log10 scale axis limit given by larger of the two fold error bounds, lowerBoundLog10 and upperBoundLog10
    log10Limit <- max(abs(c(lowerBoundLog10,upperBoundLog10)))

    #Set lower and upper log scale y axis limits to be equal
    ddiPlotConfiguration$yAxis$limits <- 10^(c(-log10Limit, log10Limit))

    #Include ticks at each order of magnitude and at 1/2 and 2
    ddiPlotConfiguration$yAxis$ticks <- unique(c(10^seq(-log10Limit, log10Limit, 1), 0.5, 2))
  }

  qualificationDDIPlot <- tlf::plotDDIRatio(
    data = ddiData,
    plotConfiguration = ddiPlotConfiguration,
    dataMapping = ddiDataMapping
  )

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_color_manual(values = sapply(data$aestheticsList$color, function(x) {
    x
  }))
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_shape_manual(values = sapply(data$aestheticsList$shape, function(x) {
    x
  }))

  # Force legend to be only one column to maintain plot panel width, and left-justify legend entries
  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::guides(col = guide_legend(ncol = 1, label.hjust = 0))

  xlabel <- paste(data$axesSettings$X$label)
  ylabel <- paste(data$axesSettings$Y$label)

  qualificationDDIPlot <- qualificationDDIPlot + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)



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
  ddiPlotResults <- list()
  gmfeDDI <- NULL
  ddiTableList <- list()
  for (pkParameter in unique(dataframe$pkParameter)) {
    for (plotType in metadata$plotTypes) {
      pkDataframe <- dataframe[dataframe$pkParameter == pkParameter, ]
      plotDDIData <- buildQualificationDDIDataframe(pkDataframe, metadata, pkParameter, plotType)

      plotID <- paste("plot", idPrefix, pkParameter, plotType, sep = "-")
      ddiPlot <- generateDDIQualificationDDIPlot(plotDDIData)
      ddiPlotResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = sectionID,
        plot = ddiPlot,
        plotCaption = ifnotnull(
          inputToCheck = captionSuffix,
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
  ddiPlotResults[[gmfeID]] <- saveTaskResults(
    id = gmfeID,
    sectionId = sectionID,
    table = gmfeDDI,
    tableCaption = paste("GMFE for", metadata$title, idPrefix),
    includeTable = TRUE
  )

  for (pkParameter in unique(dataframe$pkParameter)) {
    tableID <- paste("table", pkParameter, idPrefix, sep = "-")
    ddiPlotResults[[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = sectionID,
      table = ddiTableList[[pkParameter]],
      tableCaption = paste("Summary table for", metadata$title, idPrefix, pkParameter),
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
  for (plotIndex in seq_along(ddiData)) {
    dataframe <- ddiData[[plotIndex]]$dataframe
    metadata <- ddiData[[plotIndex]]$metadata
    sectionID <- metadata$sectionID
    sectionLevel <- configurationPlan$getSectionLevel(id = sectionID)
    idPrefix <- paste("DDIRatio", plotIndex, sep = "-")
    ddiResults <- c(ddiResults, getDDISection(dataframe, metadata, sectionID, idPrefix))

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

#' Names of fields in configuration plane containing axes settings data for each DDI plot type
#' @keywords internal
ddiPlotAxesSettings <- list(
  "predictedVsObserved" = "DDIRatioPlotsPredictedVsObserved",
  "residualsVsObserved" = "DDIRatioPlotsResidualsVsObserved"
)

#' Labels for DDI plot X-axis
#' @keywords internal
plotDDIXLabel <- list(
  "predictedVsObserved" = function(pk) {
    return(paste("Observed", pk, "Ratio"))
  },
  "residualsVsObserved" = function(pk) {
    return(paste("Observed", pk, "Ratio"))
  }
)

#' Labels for DDI plot Y-axis
#' @keywords internal
plotDDIYLabel <- list(
  "predictedVsObserved" = function(pk) {
    return(paste("Predicted", pk, "Ratio"))
  },
  "residualsVsObserved" = function(pk) {
    return(paste("Predicted", pk, "Ratio / Observed", pk, "Ratio"))
  }
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
  "predictedVsObserved" = function(observedRatio, simulatedRatio) {
    return(simulatedRatio)
  },
  "residualsVsObserved" = function(observedRatio, simulatedRatio) {
    return(simulatedRatio / observedRatio)
  }
)

#' Named list of of DDI subplot types
#' @keywords internal
ddiSubplotTypes <- list(
  "mechanism" = "Mechanism",
  "perpetrator" = "Perpetrator",
  "victim" = "Victim"
)
