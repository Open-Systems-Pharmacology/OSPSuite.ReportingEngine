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


    for (gofPlotGroup in gofPlotConfiguration$Groups) {
      caption <- gofPlotGroup$Caption
      symbol <- gofPlotGroup$Symbol
      outputMappings <- gofPlotGroup$OutputMappings

      plotGOFMetadata$groups[[caption]] <- list()
      plotGOFMetadata$groups[[caption]]$outputMappings <- list()
      plotGOFMetadata$groups[[caption]]$symbol <- tlfShape(symbol)


      for (mappingIndex in seq_along(outputMappings)) {
        outputMapping <- outputMappings[[mappingIndex]]

        projectName <- outputMapping$Project
        simulationName <- outputMapping$Simulation
        outputPath <- outputMapping$Output
        color <- outputMapping$Color

        observedDataPathInSimulation <- outputMapping$Output
        observedDataSet <- outputMapping$ObservedData
        observedDataSetFilePath <- configurationPlan$getObservedDataPath(id = observedDataSet)

        simulationFile <- configurationPlan$getSimulationPath(
          project = projectName,
          simulation = simulationName
        )

        simulationResultsFile <- configurationPlan$getSimulationResultsPath(
          project = projectName,
          simulation = simulationName
        )

        simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
        simulationResults <- ospsuite::importResultsFromCSV(simulation = simulation, filePaths = simulationResultsFile)

        outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
          Output$new(output$path)
        })
        names(outputs) <- lapply(simulation$outputSelections$allOutputs, function(output) {
          output$path
        })
        output <- outputs[[outputPath]]
        molWeight <- simulation$molWeightFor(outputPath)
        simulationDimension <- ospsuite::getQuantity(path = outputPath, container = simulation)$dimension
        simulationBaseUnit <- ospsuite::getBaseUnit(dimension = simulationDimension)

        # Setup simulations dataframe
        simulatedDataStandardized <- data.frame(
          Time = simulationResults$timeValues,
          Concentration = simulationResults$getValuesByPath(path = outputPath, individualIds = 0)
        )

        # Setup observations dataframe
        observedDataFileData <- readObservedDataFile(observedDataSetFilePath)
        observedDataFileMetaData <- parseObservationsDataFrame(observedDataFileData)
        observationsDimension <- ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit)
        observationsBaseUnit <- ospsuite::getBaseUnit(dimension = observationsDimension)

        # Verify that simulations and observations have same dimensions
        validateIsIncluded(values = massMoleConversion(observationsDimension), parentValues = massMoleConversion(simulationDimension), nullAllowed = FALSE)

        observedDataStandardized <- observedDataFileData[, c(1, 2)]
        names(observedDataStandardized) <- c("Time", "Concentration")
        observedDataStandardized$Time <- as.numeric(observedDataStandardized$Time)
        observedDataStandardized$Time <- ospsuite::toBaseUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = observedDataStandardized$Time,
          unit = observedDataFileMetaData$time$unit
        )

        observedDataStandardized$Concentration <- ospsuite::toBaseUnit(
          quantityOrDimension = observationsDimension,
          values = observedDataStandardized$Concentration,
          unit = tolower(observedDataFileMetaData$output$unit),
          molWeight = molWeight
        )

        obsTimeMatrix <- matrix(observedDataStandardized$Time, length(simulatedDataStandardized$Time), length(observedDataStandardized$Time), byrow = TRUE)
        simTimeMatrix <- matrix(simulatedDataStandardized$Time, length(simulatedDataStandardized$Time), length(observedDataStandardized$Time))
        timeMatchedData <- as.numeric(sapply(as.data.frame(abs(obsTimeMatrix - simTimeMatrix)), which.min))

        # Setup dataframe of GOF data
        gofData <- data.frame(
          time = observedDataStandardized$Time,
          observed = observedDataStandardized$Concentration,
          simulated = simulatedDataStandardized$Concentration[timeMatchedData],
          group = caption,
          outputMapping = outputPath
        )

        plotGOFDataframe <- rbind.data.frame(plotGOFDataframe, gofData)

        plotGOFMetadata$groups[[caption]]$outputMappings[[outputPath]] <- list(
          molWeight = simulation$molWeightFor(outputPath),
          color = color,
          project = projectName,
          simulation = simulationName,
          simulatedDataDimension = simulationDimension,
          simulatedDataUnit = simulationBaseUnit,
          observedDataDimension = observationsDimension,
          observedDataUnit = observationsBaseUnit
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
  xDimension <- axesSettings$X$dimension
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- axesSettings$Y$dimension
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  # function to do obs vs sim
  # predictedVsObserved|residualsOverTime
  gofPlotDataframe <- NULL
  aestheticsList <- list(shape = list(), color = list())
  for (grp in unique(dataframe$group)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    for (outputMapping in unique(dataframe[dataframe$group == grp, ]$outputMapping)) {

      molWeight <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$molWeight
      xDataDimension <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$observedDataDimension
      xDataUnit <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$observedDataUnit
      xData <- dataframe[dataframe$group == grp & dataframe$outputMapping == outputMapping, ]$observed
      xData <- ospsuite::toUnit(
        quantityOrDimension = xDataDimension,
        values = xData,
        targetUnit = xUnit,
        sourceUnit = xDataUnit,
        molWeight = molWeight
      )

      if (xScaling == "Log") {
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      yDataDimension <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$simulatedDataDimension
      yDataUnit <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$simulatedDataUnit
      yData <- dataframe[dataframe$group == grp & dataframe$outputMapping == outputMapping, ]$simulated
      yData <- ospsuite::toUnit(
        quantityOrDimension = yDataDimension,
        values = yData,
        targetUnit = yUnit,
        sourceUnit = yDataUnit,
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
        Output = outputMapping
      )

      aestheticsList$color[[outputMapping]] <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$color
      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe, df)
    }
  }
  return(list(gofPlotDataframe = gofPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
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
  xDimension <- axesSettings$X$dimension
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- axesSettings$Y$dimension
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines


  gofPlotDataframe <- NULL
  aestheticsList <- list(shape = list(), color = list())
  for (grp in unique(dataframe$group)) {
    aestheticsList$shape[[grp]] <- metadata$groups[[grp]]$symbol
    for (outputMapping in unique(dataframe[dataframe$group == grp, ]$outputMapping)) {
      molWeight <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$molWeight

      xDataDimension <- ospsuite::ospDimensions$Time
      xDataUnit <- ospsuite::getBaseUnit(dimension = xDataDimension)
      xData <- dataframe[dataframe$group == grp & dataframe$outputMapping == outputMapping, ]$time
      xData <- ospsuite::toUnit(
        quantityOrDimension = xDataDimension,
        values = xData,
        targetUnit = xUnit,
        sourceUnit = xDataUnit,
        molWeight = molWeight
      )
      if (xScaling == "Log") {
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      simulated <- dataframe[dataframe$group == grp & dataframe$outputMapping == outputMapping, ]$simulated
      simulatedDimension <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$simulatedDataDimension
      simulatedUnit <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$simulatedDataUnit

      observed <- dataframe[dataframe$group == grp & dataframe$outputMapping == outputMapping, ]$observed
      observedDimension <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$observedDataDimension
      observedUnit <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$observedDataUnit

      # Convert observed data to base units of simulated data for subsequent calculation of residuals
      observedDataInSimulatedDataUnit <- ospsuite::toUnit(
        quantityOrDimension = observedDimension,
        values = observed,
        targetUnit = simulatedUnit,
        sourceUnit = observedUnit,
        molWeight = molWeight
      )

      if (yScaling == "Log") {
        residualValues <- log10(simulated) - log10(observedDataInSimulatedDataUnit)
      } else {
        residualValues <- (simulated - observedDataInSimulatedDataUnit) / observedDataInSimulatedDataUnit
      }

      yData <- residualValues
      yData <- replaceInfWithNA(yData)

      df <- data.frame(
        Time = xData,
        Residuals = yData,
        Group = grp,
        Output = outputMapping
      )
      aestheticsList$color[[outputMapping]] <- metadata$groups[[grp]]$outputMappings[[outputMapping]]$color
      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe, df)
    }
  }
  return(list(gofPlotDataframe = gofPlotDataframe, aestheticsList = aestheticsList, axesSettings = axesSettings))
}



#' @title plotQualificationGOFPredictedVsObserved
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFPredictedVsObserved <- function(data) {


  if (data$axesSettings$X$scaling == "Log") {
    xmin <- log10(0.8) + min(na.omit(data$gofPlotDataframe[, "Observed"]))
    xmax <- log10(1.2) + max(na.omit(data$gofPlotDataframe[, "Observed"]))
  } else {
    xmin <- 0.8*min(na.omit(data$gofPlotDataframe[, "Observed"]))
    xmax <- 1.2*max(na.omit(data$gofPlotDataframe[, "Observed"]))
  }

  if (data$axesSettings$Y$scaling == "Log") {
    ymin <- log10(0.8) + min(na.omit(data$gofPlotDataframe[, "Simulated"]))
    ymax <- log10(1.2) + max(na.omit(data$gofPlotDataframe[, "Simulated"]))
  } else {
    ymin <- 0.8*min(na.omit(data$gofPlotDataframe[, "Simulated"]))
    ymax <- 1.2*max(na.omit(data$gofPlotDataframe[, "Simulated"]))
  }

  identityMinMax <- c(min(xmin,ymin),max(xmax,ymax))

  identityLine <- data.frame(
    "Observed" = identityMinMax,
    "Simulated" = identityMinMax,
    "Legend" = "Line of identity"
  )


  qualificationGOFPredictedVsObservedPlot <- ggplot2::ggplot() #tlf::initializePlot()
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::geom_point(
    data = data$gofPlotDataframe,
    mapping = aes(
      x = Observed,
      y = Simulated,
      shape = Group,
      color = Output
    )
  )


  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::geom_line(
    data = identityLine,
    mapping = aes(
      x = Observed,
      y = Simulated
    ),
    color = "black"
  )

  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::scale_color_manual(values = data$aestheticsList$color)
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::scale_shape_manual(values = data$aestheticsList$shape)
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::theme(legend.direction = "vertical",legend.box = "vertical",legend.position = "top",legend.title = element_text())
  qualificationGOFPredictedVsObservedPlot <- qualificationGOFPredictedVsObservedPlot + ggplot2::xlab(paste("Observed",data$axesSettings$X$unit)) + ggplot2::ylab(paste("Predicted",data$axesSettings$Y$unit))

  return(qualificationGOFPredictedVsObservedPlot)
}


#' @title plotQualificationGOFResidualsOverTime
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of residuals over time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFResidualsOverTime <- function(data) {
  maxRes <- 1.2 * max(abs(data$gofPlotDataframe[, "Residuals"]), na.rm = TRUE)

  qualificationGOFResVsTimePlot <- ggplot2::ggplot()
  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::geom_point(
    data = data$gofPlotDataframe,
    mapping = aes(
      x = Time,
      y = Residuals,
      shape = Group,
      color = Output
    )
  )

  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::geom_hline(
    yintercept = 0,
    size = 1,
    color = "black"
  )

  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::scale_color_manual(values = data$aestheticsList$color)
  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::scale_shape_manual(values = data$aestheticsList$shape)
  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::theme(legend.direction = "vertical",legend.box = "vertical",legend.position = "top")
  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::xlab(paste("Time",data$axesSettings$X$unit)) + ggplot2::ylab("Residuals")

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
