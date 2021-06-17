# library(ospsuite.reportingengine)
rm(list = ls())
library(ospsuite)
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

parseColHead <- function(colHead) {
  colHeadData <- strsplit(x = sub("[ []", replacement = "", x = colHead), split = "[][]")[[1]]
  return(list(name = colHeadData[1],unit = colHeadData[2]))
}

parseObservationsDataFrame <- function(observationsDataFrame){
  namesObservationsDataFrame <- names(observationsDataFrame)
  return(list(time = parseColHead(namesObservationsDataFrame[1]),
              output = parseColHead(namesObservationsDataFrame[2])))
}


plotGOFDataframe <- list()
plotGOFMetadata <- list()

for (plt in seq_along(configurationPlan$plots$GOFMergedPlots)){

  gofPlotConfiguration <- configurationPlan$plots$GOFMergedPlots[[plt]]

  groupGOFDataframe <- list()
  groupGOFMetadata <- list()
  for (group in seq_along(gofPlotConfiguration$Groups)) {
    gofPlotGroup <- gofPlotConfiguration$Groups[[group]]
    caption <- gofPlotGroup$Caption
    symbol <- gofPlotConfiguration$Groups[[1]]$Symbol
    outputMappings <- gofPlotGroup$OutputMappings
    groupGOFDataframe[[caption]] <- NULL
    outputMappingGOFMetadata <- list()

    for (omap in seq_along(outputMappings)) {

      outputMapping <- outputMappings[[omap]]

      projectName <- outputMapping$Project
      simulationName <- outputMapping$Simulation
      outputPath <- outputMapping$Output




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
      observedDataStandardized$Time <- ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                                            values = observedDataStandardized$Time,
                                                            unit = observedDataFileMetaData$time$unit)
      observedDataStandardized$Concentration <- ospsuite::toBaseUnit(quantityOrDimension = ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit),
                                                                     values = observedDataStandardized$Concentration,
                                                                     unit = observedDataFileMetaData$output$unit,
                                                                     molWeight = simulation$molWeightFor(outputPath))
      observedDataStandardized$Path <- outputPath

      #Setup simulations dataframe
      simulatedDataStandardized <- data.frame(Time = simulationResults$timeValues,
                                              Concentration = simulationResults$getValuesByPath(path = outputPath,individualIds = 0),
                                              Path = outputPath,
                                              Legend = caption)

      #Setup dataframe of residuals
      outputResidualsData <- getResiduals(observedData = observedDataStandardized,
                                          simulatedData = simulatedDataStandardized)
      outputResidualsData$outputMapping <- omap

      groupGOFDataframe[[caption]] <- rbind.data.frame(groupGOFDataframe[[caption]],outputResidualsData)

      outputMappingGOFMetadata[[outputPath]] <- list(molWeight = simulation$molWeightFor(outputPath),
                                                     displayTimeUnit =  observedDataFileMetaData$time$unit,
                                                     displayOutputUnit =  observedDataFileMetaData$output$unit)

    }


    groupGOFMetadata[[caption]] <- outputMappingGOFMetadata
  }

  plotGOFDataframe[[plt]] <- groupGOFDataframe
  plotGOFMetadata[[plt]] <- groupGOFMetadata


}





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
# odatg <- data.frame(Time = c(1,2,3),Concentration = c(110,180,330), Legend = "theOleg" , Path = "theOpath")
# sdatg <- data.frame(Time = c(1,2,3),Concentration = c(100,200,300),Legend = "theSleg" , Path = "theSpath")
# getResiduals(odatg,sdatg)
#
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
