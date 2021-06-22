#' @title getQualificationGOFPlotData
#' @description Build dataframes and metadata for each GOF plot
#' @param configurationPlan A `ConfigurationPlan` object
#' @return plotGOFdata, a list of lists of the form list(dataframe,metadata) specific to each GOF plot
getQualificationGOFPlotData <- function(configurationPlan) {
  plotGOFdata <- list()
  for (plt in seq_along(configurationPlan$plots$GOFMergedPlots)) {
    gofPlotConfiguration <- configurationPlan$plots$GOFMergedPlots[[plt]]


    plotGOFDataframe <- NULL
    plotGOFMetadata <- list()
    plotGOFMetadata$title <- gofPlotConfiguration$Title
    plotGOFMetadata$sectionID <- gofPlotConfiguration$SectionId
    plotGOFMetadata$plotTypes <- ospsuite::toPathArray(gofPlotConfiguration$PlotType)


    plotGOFMetadata$axesSettings <- lapply(plotGOFMetadata$plotTypes, function(pltType) {
      getAxesSettings(configurationPlan$plots$AxesSettings[[gofPlotAxesSettings[[pltType]]]])
    })
    names(plotGOFMetadata$axesSettings) <- plotGOFMetadata$plotTypes

    plotGOFMetadata$groups <- list()


    for (group in seq_along(gofPlotConfiguration$Groups)) {
      gofPlotGroup <- gofPlotConfiguration$Groups[[group]]
      caption <- gofPlotGroup$Caption
      symbol <- gofPlotConfiguration$Groups[[group]]$Symbol
      outputMappings <- gofPlotGroup$OutputMappings

      plotGOFMetadata$groups[[caption]] <- list()
      plotGOFMetadata$groups[[caption]]$outputMappings <- list()
      plotGOFMetadata$groups[[caption]]$symbol <- symbol


      for (omap in seq_along(outputMappings)) {
        outputMapping <- outputMappings[[omap]]

        projectName <- outputMapping$Project
        simulationName <- outputMapping$Simulation
        outputPath <- outputMapping$Output
        color <- outputMapping$Color

        observedDataPathInSimulation <- outputMapping$Output
        observedDataSet <- outputMapping$ObservedData
        observedDataSetFilePath <- configurationPlan$observedDataSets[configurationPlan$observedDataSets$id == outputMapping$ObservedData, ]$path

        simulationFile <- configurationPlan$getSimulationPath(
          project = projectName,
          simulation = simulationName
        )

        simulationResultsFile <- configurationPlan$getSimulationResultsPath(
          project = projectName,
          simulation = simulationName
        )

        simulation <- loadSimulation(simulationFile, loadFromCache = TRUE)
        simulationResults <- importResultsFromCSV(simulation = simulation, filePaths = simulationResultsFile)

        outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
          Output$new(output$path)
        })
        names(outputs) <- lapply(simulation$outputSelections$allOutputs, function(output) {
          output$path
        })
        output <- outputs[[outputPath]]
        molWeight <- simulation$molWeightFor(outputPath)
        simulationDimension <- getQuantity(path = outputPath, container = simulation)$dimension



        # Setup simulations dataframe
        simulatedDataStandardized <- data.frame(
          Time = simulationResults$timeValues,
          Concentration = simulationResults$getValuesByPath(path = outputPath, individualIds = 0)
        )

        # Setup observations dataframe
        observedDataFileData <- read.csv(file.path(inputFolder, observedDataSetFilePath), check.names = FALSE, fileEncoding = "UTF-8-BOM")
        observedDataFileMetaData <- parseObservationsDataFrame(observedDataFileData)
        # If simulation in c("Amount","'"Concentration (molar)") and observations in c("Mass","'"Concentration (mass)"), convert observations to c("Amount","'"Concentration (molar)")
        if (simulationDimension %in% c(ospDimensions$Amount, ospDimensions$`Concentration (molar)`)) {
          observationsDimension <- massMoleConversion(ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit))
        }
        # Verify that simulations and observations have same dimensions
        validateIsIncluded(values = observationsDimension, parentValues = simulationDimension, nullAllowed = FALSE)

        observedDataStandardized <- observedDataFileData[, c(1, 2)]
        names(observedDataStandardized) <- c("Time", "Concentration")
        observedDataStandardized$Time <- ospsuite::toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = observedDataStandardized$Time,
          unit = observedDataFileMetaData$time$unit
        )
        observedDataStandardized$Concentration <- ospsuite::toBaseUnit(
          quantityOrDimension = observationsDimension,
          values = observedDataStandardized$Concentration,
          unit = observedDataFileMetaData$output$unit,
          molWeight = molWeight
        )


        commonTimePoints <- intersect(observedDataStandardized$Time, simulatedDataStandardized$Time)



        # Setup dataframe of GOF data
        gofData <- data.frame(
          time = commonTimePoints,
          observed = observedDataStandardized$Concentration[observedDataStandardized$Time %in% commonTimePoints],
          simulated = simulatedDataStandardized$Concentration[simulatedDataStandardized$Time %in% commonTimePoints],
          group = caption,
          outputMapping = outputPath
        )


        plotGOFDataframe <- rbind.data.frame(plotGOFDataframe, gofData)

        plotGOFMetadata$groups[[caption]]$outputMappings[[outputPath]] <- list(
          molWeight = simulation$molWeightFor(outputPath),
          color = color,
          project = projectName,
          simulation = simulationName
        )
      }
    }
    plotGOFdata[[plt]] <- list(dataframe = plotGOFDataframe, metadata = plotGOFMetadata)
  }
  return(plotGOFdata)
}






#' @title buildQualificationGOFPredictedVsObserved
#' @description Build dataframe for observation vs prediction
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @return dataframe for plotting goodness of fit of predictedVsObserved type
buildQualificationGOFPredictedVsObserved <- function(dataframe,
                                                     metadata) {
  axesSettings <- metadata$axesSettings[["predictedVsObserved"]]

  xUnit <- axesSettings$X$unit
  xDimension <- massMoleConversion(axesSettings$X$dimension)
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- massMoleConversion(axesSettings$Y$dimension)
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  # function to do obs vs sim
  # predictedVsObserved|residualsOverTime
  gofPlotDataframe <- NULL
  for (grp in unique(dataframe$group)) {
    for (omap in unique(dataframe[dataframe$group == grp, ]$outputMapping)) {
      molWeight <- metadata$groups[[grp]]$outputMappings[[omap]]$molWeight
      xData <- dataframe[dataframe$group == grp & dataframe$outputMapping == omap, ]$observed
      xData <- ospsuite::toUnit(
        quantityOrDimension = xDimension,
        values = xData,
        targetUnit = xUnit,
        molWeight = molWeight
      )
      if (xScaling == "Log") {
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      yData <- dataframe[dataframe$group == grp & dataframe$outputMapping == omap, ]$simulated
      yData <- ospsuite::toUnit(
        quantityOrDimension = yDimension,
        values = yData,
        targetUnit = yUnit,
        molWeight = molWeight
      )
      if (yScaling == "Log") {
        yData <- log10(yData)
      }
      yData <- replaceInfWithNA(yData)

      df <- data.frame(
        Observed = xData,
        Simulated = yData,
        Group = grp,
        Output = omap
      )

      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe, df)
    }
  }
  return(gofPlotDataframe)
}




#' @title buildQualificationGOFResidualsOverTime
#' @description Build dataframe for residuals vs time
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @return dataframe for plotting goodness of fit of residuals vs time type
buildQualificationGOFResidualsOverTime <- function(dataframe,
                                                   metadata) {
  axesSettings <- metadata$axesSettings[["residualsOverTime"]]

  xUnit <- axesSettings$X$unit
  xDimension <- massMoleConversion(axesSettings$X$dimension)
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- massMoleConversion(axesSettings$Y$dimension)
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  # function to do obs vs sim
  # predictedVsObserved|residualsOverTime
  gofPlotDataframe <- NULL
  for (grp in unique(dataframe$group)) {
    for (omap in unique(dataframe[dataframe$group == grp, ]$outputMapping)) {
      molWeight <- metadata$groups[[grp]]$outputMappings[[omap]]$molWeight

      xData <- dataframe[dataframe$group == grp & dataframe$outputMapping == omap, ]$time
      xData <- ospsuite::toUnit(
        quantityOrDimension = xDimension,
        values = xData,
        targetUnit = xUnit,
        molWeight = molWeight
      )
      if (xScaling == "Log") {
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      simulated <- dataframe[dataframe$group == grp & dataframe$outputMapping == omap, ]$simulated
      observed <- dataframe[dataframe$group == grp & dataframe$outputMapping == omap, ]$observed
      if (yScaling == "Log") {
        residualValues <- log10(simulated) - log10(observed)
      } else {
        residualValues <- (simulated - observed) / observed
      }
      yData <- residualValues
      yData <- ospsuite::toUnit(
        quantityOrDimension = yDimension,
        values = yData,
        targetUnit = yUnit,
        molWeight = molWeight
      )
      yData <- replaceInfWithNA(yData)

      df <- data.frame(
        Time = xData,
        Residuals = yData,
        Group = grp,
        Output = omap
      )
      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe, df)
    }
  }
  return(gofPlotDataframe)
}



#' @title plotQualificationGOFPredictedVsObserved
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFPredictedVsObserved <- function(data) {
  identityMinMax <- c(
    0.8 * min(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE),
    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE)
  )

  identityLine <- data.frame(
    "Observed" = identityMinMax,
    "Simulated" = identityMinMax,
    "Legend" = "Line of identity"
  )



  qualificationGOFPredictedVsObservedPlot <- tlf::addLine(
    data = identityLine,
    dataMapping = tlf::XYGDataMapping$new(x = "Observed", y = "Simulated", linetype = "Legend")
  )

  obsVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Observed",
    y = "Simulated",
    shape = "Group",
    color = "Output"
  )

  qualificationGOFPredictedVsObservedPlot <- tlf::addScatter(
    data = data,
    dataMapping = obsVsPredDataMapping,
    plotObject = qualificationGOFPredictedVsObservedPlot
  )
  qualificationGOFPredictedVsObservedPlot <- tlf::setLegendPosition(
    plotObject = qualificationGOFPredictedVsObservedPlot,
    position = reDefaultLegendPosition
  )

  return(qualificationGOFPredictedVsObservedPlot)
}


#' @title plotQualificationGOFResidualsOverTime
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of residuals over time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFResidualsOverTime <- function(data) {
  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals",
    shape = "Group",
    color = "Output"
  )

  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]), na.rm = TRUE)

  qualificationGOFResVsTimePlot <- tlf::initializePlot()

  qualificationGOFResVsTimePlot <- tlf::addScatter(
    data = data,
    dataMapping = resVsTimeDataMapping,
    plotObject = qualificationGOFResVsTimePlot
  )

  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::geom_hline(
    yintercept = 0,
    size = 1
  )

  qualificationGOFResVsTimePlot <- tlf::setLegendPosition(
    plotObject = qualificationGOFResVsTimePlot,
    position = reDefaultLegendPosition
  )
  qualificationGOFResVsTimePlot <- tlf::setYAxis(
    plotObject = qualificationGOFResVsTimePlot,
    limits = c(-maxRes, maxRes)
  )

  return(qualificationGOFResVsTimePlot)
}



#' @title plotQualificationGOFs
#' @description Plot observation vs prediction for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @return list of qualification GOF ggplot objects
plotQualificationGOFs <- function(configurationPlan,
                                  logFolder = getwd(),
                                  settings) {
  gofPlotsData <- getQualificationGOFPlotData(configurationPlan)
  gofPlotList <- list()
  gofPlotResults <- list()

  for (plotIndex in seq_along(gofPlotsData)) {
    gofPlotList[[plotIndex]] <- list()
    dataframe <- gofPlotsData[[plotIndex]]$dataframe
    metadata <- gofPlotsData[[plotIndex]]$metadata
    for (plotType in metadata$plotTypes) {
      plotID <- paste("GOFMergedPlot", plotIndex, plotType, sep = "-")

      plotGOFDataframe <- buildGOFDataFrameFunctions[[plotType]](dataframe, metadata)
      gofPlotList[[plotIndex]][[plotType]] <- plotGOFFunctions[[plotType]](plotGOFDataframe)

      gofPlotResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = metadata$sectionID,
        plot = gofPlotList[[plotIndex]][[plotType]],
        plotCaption = metadata$title
      )
    }
  }
  return(gofPlotResults)
}

#' Names of fields in configuration plane containing axes settings data for each GOF plot type
gofPlotAxesSettings <- list(
  "predictedVsObserved" = "GOFMergedPlotsPredictedVsObserved",
  "residualsOverTime" = "GOFMergedPlotsResidualsOverTime"
)

#' Names of functions for extracting data for each GOF plot type
buildGOFDataFrameFunctions <- list(
  "predictedVsObserved" = buildQualificationGOFPredictedVsObserved,
  "residualsOverTime" = buildQualificationGOFResidualsOverTime
)

#' Names of functions for plotting GOF plots for each GOF plot type
plotGOFFunctions <- list(
  "predictedVsObserved" = plotQualificationGOFPredictedVsObserved,
  "residualsOverTime" = plotQualificationGOFResidualsOverTime
)
