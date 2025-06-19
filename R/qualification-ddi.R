#' @title getQualificationDDIPlotData
#' @description Build dataframes and metadata for each DDI plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return  plotDDIdata, a list of lists of the form list(dataframe,metadata) specific to each DID plot
#' @import ospsuite.utils
#' @keywords internal
getQualificationDDIPlotData <- function(configurationPlan) {
  plotDDIdata <- list()
  for (plotNumber in seq_along(configurationPlan$plots$DDIRatioPlots)) {
    qualificationCatch(
      {
        plotDDIDataFrame <- NULL
        plotDDIMetadata <- list()
        plot <- configurationPlan$plots$DDIRatioPlots[[plotNumber]]

        plotDDIMetadata$title <- plot$Title
        plotDDIMetadata$sectionID <- plot$SectionReference %||% plot$SectionId
        validateIsIncluded(values = plot$Subunits, parentValues = names(ddiSubplotTypes), nullAllowed = TRUE)
        plotDDIMetadata$subunits <- plot$Subunits
        plotDDIMetadata$artifacts <- plot$Artifacts
        plotDDIMetadata$plotSettings <- plot

        # Pipes in configuration plan will be deprecated moving forward
        plotDDIMetadata$plotTypes <- plot$PlotTypes %||% ospsuite::toPathArray(plot$PlotType)

        validateIsIncluded(plotDDIMetadata$plotTypes, names(ddiPlotTypeSpecifications))

        plotDDIMetadata$axesSettings <- lapply(plotDDIMetadata$plotTypes, function(plotType) {
          getAxesSettings(configurationPlan$plots$AxesSettings[[ddiPlotTypeSpecifications[[plotType]]$ddiPlotAxesSettings]])
        })
        names(plotDDIMetadata$axesSettings) <- plotDDIMetadata$plotTypes


        plotDDIMetadata$groups <- list()
        plotDDIMetadata$guestDelta <- getGuestDeltaFromConfigurationPlan(plot)

        pkParameters <- plot$PKParameters %||% ospsuite::toPathArray(plot$PKParameter)
        validateIsIncluded(values = pkParameters, parentValues = names(ddiPKRatioColumnName), nullAllowed = FALSE)

        defaultProperties <- getDefaultPropertiesFromTheme("plotDDIRatio", propertyType = "points")
        for (groupNumber in seq_along(plot$Groups)) {
          group <- plot$Groups[[groupNumber]]
          plotDDIMetadata$groups[[groupNumber]] <- list()
          plotDDIMetadata$groups[[groupNumber]]$caption <- group$Caption
          plotDDIMetadata$groups[[groupNumber]]$color <- group$Color %||% defaultProperties$color
          plotDDIMetadata$groups[[groupNumber]]$symbol <- tlfShape(group$Symbol %||% defaultProperties$shape)

          for (ddiRatio in group$DDIRatios) {
            outputPath <- ddiRatio$Output
            observedDataSet <- ddiRatio$ObservedData
            observedDataSetFilePath <- configurationPlan$getObservedDataPath(id = observedDataSet)
            observedDataRecordId <- ddiRatio$ObservedDataRecordId
            observedDataFrame <- readObservedDataFile(fileName = observedDataSetFilePath)
            validateIsIncluded(observedDataRecordId, observedDataFrame$ID)

            ratioList <- list()
            for (pkParameter in pkParameters) {
              ratioList[[pkParameter]] <- list()
              validateIsIncluded(ddiPKRatioColumnName[[pkParameter]], names(observedDataFrame))

              # The following tryCatch verifies that the PK parameter columns are read as `numeric` by the call to `readObservedDataFile` above.
              # The function `readObservedDataFile` first attempts to read csv files using `read.csv`.
              # If this fails, because, for example the CSV file is semicolon separated, `readObservedDataFile` attempts to read the file using `read.csv2`.
              # If a semicolon-separated CSV contains a float column with period `.` decimal separators (and not comma ',' decimal separators) then read.csv2 will read this column as `factor`.
              # Therefore, coerce this column into `numeric` format:
              observedDataFrame[[ddiPKRatioColumnName[[pkParameter]]]] <- as.numeric(observedDataFrame[[ddiPKRatioColumnName[[pkParameter]]]])

              observedDataSelection <- observedDataFrame$ID %in% observedDataRecordId
              ratioList[[pkParameter]] <- getDDIRatioList(observedDataFrame[observedDataSelection, ], ddiPKRatioColumnName[[pkParameter]])
              for (simulationType in c("SimulationControl", "SimulationDDI")) {
                plotComponent <- ddiRatio[[simulationType]]
                projectName <- plotComponent$Project
                simulationName <- plotComponent$Simulation

                startTime <- getTimeFromPlan(plotComponent, "StartTime")
                endTime <- getTimeFromPlan(plotComponent, "EndTime")
                useAUCinf <- all(isEmpty(endTime), isIncluded(pkParameter, "AUC"))
                pkParameterName <- generateDDIPlotPKParameterName(
                  ifelse(useAUCinf, "AUC_inf", pkParameter),
                  startTime,
                  endTime
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
                pkAnalysisResults <- loadPKAnalysesFromCSV(
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
                mechanism = getMechanismName(ratioList[[pkParameter]]$mechanism),
                perpetrator = ratioList[[pkParameter]]$perpetrator %||% NA,
                routePerpetrator = ratioList[[pkParameter]]$routePerpetrator %||% NA,
                victim = ratioList[[pkParameter]]$victim %||% NA,
                routeVictim = ratioList[[pkParameter]]$routeVictim %||% NA,
                dose = ratioList[[pkParameter]]$dose,
                doseUnit = ratioList[[pkParameter]]$doseUnit,
                description = ratioList[[pkParameter]]$description %||% NA
              )

              plotDDIDataFrame <- rbind.data.frame(plotDDIDataFrame, df)
            }
          }
        }

        plotDDIdata[[plotNumber]] <- list(
          dataframe = plotDDIDataFrame,
          metadata = plotDDIMetadata
        )
      },
      configurationPlanField = plot
    )
  }
  return(plotDDIdata)
}

#' @title getMechanismName
#' @description Remove underscores from mechanism name read from DDI observed data file according to the dictionary defined in `reEnv$ddiRatioSubsetsDictionary`
#' @param mechanism name of mechanism as read form DDI observed data file
#' @return Display name of mechanism to be used in DDI report
#' @keywords internal
getMechanismName <- function(mechanism) {
  if (is.null(mechanism)) {
    return(NA)
  }
  correctedMechanismName <- reEnv$ddiRatioSubsetsDictionary[[as.character(mechanism)]]
  if (is.null(correctedMechanismName)) {
    return(mechanism)
  }
  return(correctedMechanismName)
}


#' @title getDDIRatioList
#' @description Read the entries from a DDI observations data.frame that correspond to a particular PK parameter into a named list
#' @param observedDataFrameRow data.frame of DDI observations
#' @param ddiPKRatioColumnName Name of column in data.frame `observedDataFrameRow` containing the value of the PK parameter observation to be read
#' @return A named list containing entries in `observedDataFrameRow`corresponding to the PK parameter in the data.frame column `ddiPKRatioColumnName`
#' @keywords internal
getDDIRatioList <- function(observedDataFrameRow, ddiPKRatioColumnName) {
  ratioList <- list()
  for (col in names(reEnv$ddiRatioListColumnMappings)) {
    colName <- reEnv$ddiRatioListColumnMappings[[col]]
    ratioList[[col]] <- observedDataFrameRow[[colName]]
  }
  ratioList$observedRatio <- observedDataFrameRow[[ddiPKRatioColumnName]]
  return(ratioList)
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



#' @title getSmartZoomLimits
#' @description Get default axis limits for DDI Ratio plot
#' @param dataVector a numeric vector containing the x or y values of datapoints to be plotted
#' @param residualsVsObserved A logical values indicating if DDI Ratios are plotted as residuals vs observed
#' @return a list containing the minimum and maximum axes limits
#' @keywords internal
getSmartZoomLimits <- function(dataVector, residualsVsObserved = FALSE) {
  validateIsNumeric(dataVector)
  # Use na.rm in case NAs were not removed
  minLimit <- ifelse(
    min(dataVector, na.rm = TRUE) >= 1,
    0.8,
    min(0.8 * min(dataVector, na.rm = TRUE), 0.25)
  )
  maxLimit <- ifelse(
    max(dataVector, na.rm = TRUE) >= 1,
    max(1.25 * max(dataVector, na.rm = TRUE), 4),
    1.25
  )
  # For residuals vs observed, y axis should always include the 2-fold error range
  if (residualsVsObserved) {
    return(list(min = min(minLimit, 0.25), max = max(maxLimit, 4)))
  }
  return(list(min = minLimit, max = maxLimit))
}


#' @title generateDDIQualificationDDIPlot
#' @description Plot observation vs prediction for qualification workflow
#' @param ddiPlotData a list containing the plot data.frame, aesthetics list, axes settings and plot settings
#' @param delta Delta value from Guest et al. formula
#' @return ggplot DDI plot object for DDI qualification workflow
#' @import tlf
#' @import ggplot2
#' @importFrom stats na.omit
#' @keywords internal
generateDDIQualificationDDIPlot <- function(ddiPlotData, delta) {
  ddiData <- na.omit(ddiPlotData$ddiPlotDataframe)
  if (isEmpty(ddiData)) {
    return(NULL)
  }

  residualsVsObserved <- ddiPlotTypeSpecifications[[ddiPlotData$axesSettings$plotType]]$residualsVsObservedFlag

  ddiDataMapping <- tlf::DDIRatioDataMapping$new(
    x = ddiPlotData$axesSettings$X$label,
    y = ddiPlotData$axesSettings$Y$label,
    shape = "Caption",
    color = "Caption",
    residualsVsObserved = residualsVsObserved,
    # Undefined delta in Configuration Plan corresponds to NULL
    # In such case, default value should be 1 to prevent crash when defining data mapping
    deltaGuest = delta %||% 1
  )

  ddiPlotConfiguration <- getPlotConfigurationFromPlan(
    plotProperties = ddiPlotData$plotSettings,
    plotType = "DDIRatio",
    legendPosition = tlf::LegendPositions$outsideRight
  )

  # Set axes labels
  ddiPlotConfiguration$labels$xlabel$text <- ddiPlotData$axesSettings$X$label
  ddiPlotConfiguration$labels$ylabel$text <- ddiPlotData$axesSettings$Y$label
  # Set points color and shapes
  isInLegend <- names(ddiPlotData$aestheticsList$color) %in% ddiData$Caption
  ddiPlotConfiguration$points$color <- sapply(ddiPlotData$aestheticsList$color[isInLegend], identity)
  ddiPlotConfiguration$points$shape <- sapply(ddiPlotData$aestheticsList$shape[isInLegend], identity)

  # DDI Default scales are already log, only need to update limits
  xSmartZoom <- getSmartZoomLimits(ddiData[[ddiPlotData$axesSettings$X$label]])
  ySmartZoom <- getSmartZoomLimits(ddiData[[ddiPlotData$axesSettings$Y$label]], residualsVsObserved)
  # Ensure same limits are used for quadratic obs vs pred plots
  if (!residualsVsObserved) {
    xSmartZoom <- range(xSmartZoom, ySmartZoom)
    ySmartZoom <- xSmartZoom
  }
  ddiPlotConfiguration$xAxis$axisLimits <- range(xSmartZoom)
  ddiPlotConfiguration$yAxis$axisLimits <- range(ySmartZoom)
  ddiPlotConfiguration$xAxis$ticks <- autoAxesTicksFromLimits(range(xSmartZoom))
  ddiPlotConfiguration$yAxis$ticks <- autoAxesTicksFromLimits(range(ySmartZoom))

  # minRange defines the range for simulated x values of Guest et al. criterion
  # Consequently, it also needs to be updated according to smart zoom
  ddiDataMapping$minRange <- range(xSmartZoom)

  qualificationDDIPlot <- tlf::plotDDIRatio(
    data = ddiData,
    dataMapping = ddiDataMapping,
    plotConfiguration = ddiPlotConfiguration
  )
  # Force legend to be only one column to maintain plot panel width, and left-justify legend entries
  qualificationDDIPlot <- qualificationDDIPlot +
    # Issue #1060, ncol currently enforce the number of columns in legend
    ggplot2::guides(
      color = ggplot2::guide_legend(ncol = 1, label.hjust = 0, title = NULL),
      shape = ggplot2::guide_legend(ncol = 1, label.hjust = 0, title = NULL)
    )
  # Only obs vs pred is required as quadratic
  if (!residualsVsObserved) {
    qualificationDDIPlot <- setQuadraticDimension(qualificationDDIPlot)
  }
  return(qualificationDDIPlot)
}

#' @title getQualificationDDIRatioMeasure
#' @description Get qualification measure of DDI ratio from field `DDIRatioPlots` of configuration plan
#' @param summaryDataFrame data.frame with DDI Ratios
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param delta Delta value from Guest et al. formula
#' @return A data.frame
#' @keywords internal
getQualificationDDIRatioMeasure <- function(summaryDataFrame, pkParameterName, delta) {
  guestValues <- tlf::getGuestValues(
    x = summaryDataFrame[["observedRatio"]],
    delta = delta %||% 1
  )

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

      plotID <- defaultFileNames$resultID(idPrefix, "ddi_ratio_plot", pkParameter, plotType)
      ddiPlot <- generateDDIQualificationDDIPlot(plotDDIData, delta = metadata$guestDelta[[pkParameter]])
      if (isEmpty(ddiPlot)) {
        warning(
          messages$warningDDINotPlotted(
            title = plotDDIData$plotSettings$Title,
            pkParameter = pkParameter,
            plotType = plotType,
            # Caption suffix indicates for which subunit, DDI plot is skipped
            captionSuffix = captionSuffix
          ),
          call. = FALSE
        )
        next
      }
      ddiPlotCaption <- getDDIPlotCaption(
        title = metadata$title,
        subPlotCaption = captionSuffix,
        pkParameter = pkParameter,
        plotTypeCaption = ddiPlotTypeSpecifications[[plotType]]$figureCaption,
        guestDelta = metadata$guestDelta[[pkParameter]]
      )
      ddiArtifacts[["Plot"]][[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = sectionID,
        plot = ddiPlot,
        plotCaption = ddiPlotCaption
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

    ddiTableList[[pkParameter]] <- getQualificationDDIRatioMeasure(
      summaryDataFrame = ddiSummary,
      pkParameterName = pkParameter,
      delta = metadata$guestDelta[[pkParameter]]
    )
  }

  gmfeID <- defaultFileNames$resultID(idPrefix, "ddi_ratio_gmfe")
  ddiArtifacts[["GMFE"]][[gmfeID]] <- saveTaskResults(
    id = gmfeID,
    sectionId = sectionID,
    table = gmfeDDI,
    tableCaption = captions$ddi$gmfe(metadata$title),
    includeTable = TRUE
  )


  for (pkParameter in unique(dataframe$pkParameter)) {
    tableID <- defaultFileNames$resultID(idPrefix, "ddi_ratio_measure", pkParameter)
    ddiArtifacts[["Measure"]][[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = sectionID,
      table = ddiTableList[[pkParameter]],
      tableCaption = captions$ddi$measureTable(
        title = metadata$title,
        pkParameter = pkParameter,
        guestDelta = metadata$guestDelta[[pkParameter]]
      ),
      includeTable = TRUE
    )
  }

  # Ensure artifacts will appear in same order as in configuration plan
  ddiPlotResults <- unlist(ddiArtifacts[metadata$artifacts])

  return(ddiPlotResults)
}

#' @title getDDITable
#' @description Summary table for DDI
#' @param dataframe for generating DDI summary table
#' @return Summary table for DDI plot
#' @keywords internal
getDDITable <- function(dataframe) {
  dataframe$simObsRatio <- dataframe$simulatedRatio / dataframe$observedRatio

  ddiTable <- list()

  pkParameters <- unique(dataframe$pkParameter)

  for (pk in pkParameters) {
    pkDataframe <- dataframe[dataframe$pkParameter == pk, ]

    ddiTable[[pk]] <- data.frame(
      "DataID" = pkDataframe$id,
      "Perpetrator" = paste(pkDataframe$perpetrator, paste(pkDataframe$dose, pkDataframe$doseUnit), pkDataframe$routePerpetrator, pkDataframe$description, sep = ", "),
      "Victim" = paste(pkDataframe$victim, pkDataframe$routeVictim, sep = ", ")
    )

    ddiTable[[pk]][[paste("Predicted", pk, "Ratio")]] <- pkDataframe$simulatedRatio
    ddiTable[[pk]][[paste("Observed", pk, "Ratio")]] <- pkDataframe$observedRatio
    ddiTable[[pk]][[paste("Pred/Obs", pk, "Ratio")]] <- pkDataframe$simObsRatio
    ddiTable[[pk]][["Reference"]] <- pkDataframe$studyId
  }

  # Merge together all dataframes (each of which corresponds to a different PK parameter) by combining together all rows that share common values in the columns named "DataID","Perpetrator","Victim","Reference"
  mergedDDITable <- Reduce(
    function(x, y) merge(x, y, by = c("DataID", "Perpetrator", "Victim", "Reference")),
    ddiTable
  )

  # Move reference column to the end
  mergedDDITable <- cbind(mergedDDITable[, names(mergedDDITable) != "Reference"], data.frame("Reference" = mergedDDITable$Reference))

  # Order rows by Data ID
  mergedDDITable <- mergedDDITable[order(mergedDDITable$DataID), ]

  return(mergedDDITable)
}


#' @title plotQualificationDDIs
#' @description Plot observation vs prediction for DDI qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param settings A `TaskSettings` object
#' @return list of qualification DDI ggplot objects
#' @keywords internal
plotQualificationDDIs <- function(configurationPlan, settings) {
  ddiData <- getQualificationDDIPlotData(configurationPlan)

  ddiResults <- list()
  for (plotIndex in seq_along(ddiData)) {
    dataframe <- ddiData[[plotIndex]]$dataframe
    metadata <- ddiData[[plotIndex]]$metadata
    sectionID <- metadata$sectionID
    subunits <- metadata$subunits
    sectionLevel <- configurationPlan$getSectionLevel(id = sectionID)
    idPrefix <- paste("DDIRatio", plotIndex, sep = "-")
    ddiResults <- c(ddiResults, getDDISection(dataframe, metadata, sectionID, idPrefix))

    if ("Table" %in% metadata$artifacts) {
      ddiTable <- saveTaskResults(
        id = "DDI Table",
        sectionId = sectionID,
        table = getDDITable(dataframe),
        tableCaption = captions$ddi$summaryTable(metadata$title),
        includeTable = TRUE
      )
      ddiResults <- c(ddiResults, ddiTable)
    }

    for (subplotTypeName in subunits) {
      subplotType <- ddiSubplotTypes[[subplotTypeName]]
      subheading <- saveTaskResults(
        id = subplotType,
        sectionId = sectionID,
        # Subheading result includes anchor tag to be referenced in TOC
        textChunk = paste(
          paste0(rep("#", sectionLevel + 1), collapse = ""),
          " ",
          subplotTypeName,
          anchor(paste0(sectionID, "-ddi-subunit-", length(ddiResults) + 1)),
          sep = ""
        ),
        includeTextChunk = TRUE
      )
      ddiResults <- c(ddiResults, subheading)
      # Enforce character class instead of factor
      # to ensure that function 'sort' uses alphabetical order instead of factor levels
      subplotTypeLevels <- as.character(dataframe[[subplotType]])
      subplotTypeLevels <- sort(unique(subplotTypeLevels))
      for (subplotTypeLevel in subplotTypeLevels) {
        subsubheading <- saveTaskResults(
          id = paste(subplotType, subplotTypeLevel, sep = " - "),
          sectionId = sectionID,
          # Subheading result includes anchor tag to be referenced in TOC
          textChunk = paste(
            paste0(rep("#", sectionLevel + 2), collapse = ""),
            " ",
            subplotTypeLevel,
            anchor(paste0(sectionID, "-ddi-subunit-", length(ddiResults) + 1)),
            sep = ""
          ),
          includeTextChunk = TRUE
        )
        ddiResults <- c(ddiResults, subsubheading)
        subplotDataframe <- droplevels(dataframe[dataframe[[subplotType]] == subplotTypeLevel, ])
        sectionID <- metadata$sectionID
        idPrefix <- paste("DDIRatio", plotIndex, subplotType, subplotTypeLevel, sep = "-")
        ddiResults <- c(ddiResults, getDDISection(subplotDataframe, metadata, sectionID, idPrefix, captionSuffix = paste0(subplotTypeName, ": ", subplotTypeLevel)))
      }
    }
  }

  return(ddiResults)
}



#' Specifications of each DDI plot type
#' @keywords internal
ddiPlotTypeSpecifications <- list(
  predictedVsObserved = list(
    ddiPlotAxesSettings = "DDIRatioPlotsPredictedVsObserved",
    plotDDIXLabel = function(pk) {
      return(paste("Observed", pk, "Ratio"))
    },
    plotDDIYLabel = function(pk) {
      return(paste("Predicted", pk, "Ratio"))
    },
    residualsVsObservedFlag = FALSE,
    getYAxisDDIValues = function(observedRatio, simulatedRatio) {
      return(simulatedRatio)
    },
    figureCaption = "Predicted vs. Observed"
  ),
  residualsVsObserved = list(
    ddiPlotAxesSettings = "DDIRatioPlotsResidualsVsObserved",
    plotDDIXLabel = function(pk) {
      return(paste("Observed", pk, "Ratio"))
    },
    plotDDIYLabel = function(pk) {
      return(paste("Predicted", pk, "Ratio / Observed", pk, "Ratio"))
    },
    residualsVsObservedFlag = TRUE,
    getYAxisDDIValues = function(observedRatio, simulatedRatio) {
      return(simulatedRatio / observedRatio)
    },
    figureCaption = "Predicted/Observed vs. Observed"
  )
)


#' Allowed DDI subplot types
#' @keywords internal
ddiSubplotTypes <- list(
  "Mechanism" = "mechanism",
  "Perpetrator" = "perpetrator",
  "Victim" = "victim"
)


#' Column names to check in observed data based on Configuration Plan PK Parameter
#' @keywords internal
ddiPKRatioColumnName <- list(
  "AUC" = "AUCR Avg",
  "AUC_tEnd" = "AUCR Avg",
  "CMAX" = "CmaxR Avg",
  "C_max" = "CmaxR Avg"
)


#' @title getGuestDeltaFromConfigurationPlan
#' @description Get Guest et al. delta values for a DDI Ratio plot from the configuration plan
#' @param ddiRatioPlan Properties defined in `DDIRatioPlots` field of configuration plan
#' @return A named list of Guest et al. values
#' @keywords internal
getGuestDeltaFromConfigurationPlan <- function(ddiRatioPlan) {
  pkParameters <- ddiRatioPlan$PKParameters %||% ospsuite::toPathArray(ddiRatioPlan$PKParameter)
  # By default all Guest et al.  delta values are 1
  guestDelta <- as.list(sapply(pkParameters, function(x) {
    1
  }))
  if (isEmpty(ddiRatioPlan$GuestDelta)) {
    return(guestDelta)
  }
  # If defined as a numeric, all parameters get same delta
  if (is.numeric(ddiRatioPlan$GuestDelta)) {
    return(as.list(sapply(pkParameters, function(x) {
      ddiRatioPlan$GuestDelta
    })))
  }
  # Otherwise, update defined values
  for (guestPlan in ddiRatioPlan$GuestDelta) {
    for (pkParameter in guestPlan$PKParameters) {
      guestDelta[[pkParameter]] <- guestPlan$Value
    }
  }
  # Throw an error if parameters defined in Guest Delta were not included in the DDI parameters
  validateGuestParameters(names(guestDelta), pkParameters)
  return(guestDelta)
}
