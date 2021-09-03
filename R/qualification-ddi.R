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
    plotDDIMetadata$plotTypes <- plot$PlotTypes %||% ospsuite::toPathArray(plot$PlotType)
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







