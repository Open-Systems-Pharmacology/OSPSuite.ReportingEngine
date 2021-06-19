# library(ospsuite.reportingengine)
rm(list = ls())
library(ospsuite)
graphics.off()
setwd(dir = "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/")
devtools::load_all(".")


re.example.dir <- "tests/dev/Qualification-Minimal"
minimal.example.dir <-"../QualificationPlan/examples/minimal"
advanced.example.dir <-"../QualificationPlan/examples/advanced_01/"
example.dirs <- c(re.example.dir,minimal.example.dir,advanced.example.dir)
example.number <- 1

setwd(dir = example.dirs[example.number])

#-------- Qualification Inputs --------#
# Configuration Plan is stored within a json file

inputFolder <- "reporting engine input"
configurationFile <- file.path(inputFolder, "/report-configuration-plan.json")
workflowFolder <- "reporting engine output"

# #-------- Workflow Definition --------#
# workflow <- loadQualificationWorkflow(
#   workflowFolder = workflowFolder,
#   configurationPlanFile = configurationFile
# )
# # Workflow works as mean model and population workflows
# workflow$runWorkflow()

#-------- Configuration Plan --------#
configurationPlan <- loadConfigurationPlan(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

configurationPlan$sections
configurationPlan$simulationMappings
configurationPlan$observedDataSets


# outputsTimeProfile1 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[1]])
# outputsTimeProfile2 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[2]])
# outputsTimeProfile3 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[3]])
#
# outputsGOF <- getOutputsFromGOFMergedPlotsConfiguration(plot = configurationPlan$plots$GOFMergedPlots[[1]])



# workflow$runWorkflow()

# workflow$simulate$runTask(workflow$simulationStructures)
# taskResults <- plotQualificationTimeProfiles(workflow$configurationPlan,
#                                              logFolder = self$workflowFolder,
#                                              settings = NULL
# )




getQualificationGOFPlotData <- function(configurationPlan){

  plotGOFdata <- list()
  for (plt in seq_along(configurationPlan$plots$GOFMergedPlots)){

    gofPlotConfiguration <- configurationPlan$plots$GOFMergedPlots[[plt]]


    plotGOFDataframe <- NULL
    plotGOFMetadata <- list()
    plotGOFMetadata$title <- gofPlotConfiguration$Title
    plotGOFMetadata$plotTypes <-  ospsuite::toPathArray(gofPlotConfiguration$PlotType)
    plotGOFMetadata$groups <- list()


    for (group in seq_along(gofPlotConfiguration$Groups)) {
      gofPlotGroup <- gofPlotConfiguration$Groups[[group]]
      caption <- gofPlotGroup$Caption
      symbol <- gofPlotConfiguration$Groups[[1]]$Symbol
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

        simulation <- loadSimulation(simulationFile,loadFromCache = TRUE)
        simulationResults <- importResultsFromCSV(simulation = simulation, filePaths = simulationResultsFile)

        outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
          Output$new(output$path)
        })
        names(outputs) <- lapply(simulation$outputSelections$allOutputs, function(output) {
          output$path
        })
        output <- outputs[[outputPath]]

        #Setup observations dataframe
        observedDataFileData <- read.csv(file.path(inputFolder, observedDataSetFilePath),check.names = FALSE ,fileEncoding = "UTF-8-BOM")
        observedDataFileMetaData <- parseObservationsDataFrame(observedDataFileData)
        observedDataStandardized <- observedDataFileData[,c(1,2)]
        names(observedDataStandardized) <-c("Time","Concentration")

        #observedDataStandardized$Time <-  observedDataStandardized$Time
        #$Concentration <- observedDataStandardized$Concentration

        observedDataStandardized$Time <- ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                                              values = observedDataStandardized$Time,
                                                              unit = observedDataFileMetaData$time$unit)
        observedDataStandardized$Concentration <- ospsuite::toBaseUnit(quantityOrDimension = ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit),
                                                                       values = observedDataStandardized$Concentration,
                                                                       unit = observedDataFileMetaData$output$unit,
                                                                       molWeight = simulation$molWeightFor(outputPath))
        observedDataStandardized$Path <- outputPath

        #Setup simulations dataframe
        simulatedDataStandardized <- data.frame(Time = ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                                                            values = simulationResults$timeValues,
                                                                            unit = observedDataFileMetaData$time$unit),
                                                Concentration = ospsuite::toUnit(quantityOrDimension = ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit),
                                                                                 values = simulationResults$getValuesByPath(path = outputPath,individualIds = 0),
                                                                                 targetUnit = observedDataFileMetaData$output$unit,
                                                                                 sourceUnit = ospsuite::getQuantity(path = outputPath,container = simulation)$unit,
                                                                                 molWeight = simulation$molWeightFor(outputPath)),
                                                Path = outputPath,
                                                Legend = caption)



        # simulatedDataStandardized <- data.frame(Time = ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
        #                                                                     values = simulationResults$timeValues,
        #                                                                     unit = observedDataFileMetaData$time$unit),
        #                                         Concentration = ospsuite::toBaseUnit(quantityOrDimension = ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit),
        #                                                                              values = simulationResults$getValuesByPath(path = outputPath,individualIds = 0),
        #                                                                              unit = observedDataFileMetaData$output$unit,
        #                                                                              molWeight = simulation$molWeightFor(outputPath)),
        #                                         Path = outputPath,
        #                                         Legend = caption)
        # simulatedDataStandardized <- data.frame(Time = simulationResults$timeValues,
        #                                         Concentration = simulationResults$getValuesByPath(path = outputPath,individualIds = 0),
        #                                         Path = outputPath,
        #                                         Legend = caption)

        #print(simulatedDataStandardized)

        #Setup dataframe of residuals
        outputResidualsData <- getResiduals(observedData = observedDataStandardized,
                                            simulatedData = simulatedDataStandardized)

        plotGOFDataframe <- rbind.data.frame(plotGOFDataframe,outputResidualsData)

        plotGOFMetadata$groups[[caption]]$outputMappings[[outputPath]] <- list(molWeight = simulation$molWeightFor(outputPath),
                                                                               displayTimeUnit =  observedDataFileMetaData$time$unit,
                                                                               displayOutputUnit =  observedDataFileMetaData$output$unit,
                                                                               color = color)

      }
    }
    plotGOFdata[[plt]] <- list(dataframe  = plotGOFDataframe, metadata = plotGOFMetadata)
  }
  return(plotGOFdata)
}

plotData <- getQualificationGOFPlotData(configurationPlan)




plotQualificationGOF <- function(plotData){

  gofPlotList <- list()

  for (plotSet in seq_along(plotData)){
    gofPlotList[[plotSet]] <- list()

    #plotDataframeWithDisplayUnits <- scaleToDisplayUnits(plotData)

    for (plotType in plotData[[plotSet]]$metadata$plotTypes){

      #Remove Inf values before plotting
      for (col in c("Observed","Simulated","Residuals")){
        plotData[[plotSet]]$dataframe[[col]] <- replaceInfWithNA(plotData[[plotSet]]$dataframe[[col]])
      }

      #print(plotData[[plotSet]]$dataframe)
      gofPlotList[[plotSet]][[plotType]] <- gofPlotFunctions[[plotType]]( plotData[[plotSet]]$dataframe )
      show(gofPlotList[[plotSet]][[plotType]])
    }
  }
  return(gofPlotList)
}


plotQualificationGOF(plotData)

# outputObservedResults <- getObservedDataFromOutput(output = output,
#                                                    data = observedResult$data,
#                                                    dataMapping = observedResult$dataMapping,
#                                                    molWeight = simulation$molWeightFor(quantityPath = outputPath),
#                                                    timeUnit = structureSet$simulationSet$timeUnit,
#                                                    logFolder = logFolder)




#
# outputResidualsData <- getResiduals(outputObservedResults$data, outputSimulatedData, output$residualScale)
# outputData <- data.frame(
#   "Time" = ospsuite::toUnit("Time", data[selectedRows, dataMapping$time], timeUnit),
#   "Concentration" = outputConcentration,
#   "Legend" = paste0("Observed data ", output$dataDisplayName),
#   "Path" = output$path
# )
#


# dataMapping <- list(
#   time = timeColumn,
#   dv = dvColumn,
#   lloq = lloqColumn,
#   dimension = "dimension"
# )







# # Time profile plot 1
# gofTaskSettings1 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile1)
# workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings1)
# workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[2])
#
# # Time profile plot 2
# gofTaskSettings2 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile2)
# workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings2)
# workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])
#
# # Time profile plot 3
# gofTaskSettings3 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile3)
# workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings3)
# workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])
