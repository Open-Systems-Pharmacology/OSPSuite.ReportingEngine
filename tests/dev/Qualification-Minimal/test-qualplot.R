# library(ospsuite.reportingengine)
rm(list = ls())
devtools::load_all(".")
#-------- Qualification Inputs --------#
# Configuration Plan is stored within a json file
configurationFile <- "tests/dev/Qualification-Minimal/reporting engine input/report-configuration-plan.json"
workflowFolder <- "tests/dev/Qualification-Minimal/reporting engine output"

#-------- Workflow Definition --------#
workflow <- loadQualificationWorkflow(
  workflowFolder = workflowFolder,
  jsonFile = configurationFile
)


#-------- Configuration Plan --------#
configurationPlan <- loadConfigurationPlan(
  workflowFolder = workflowFolder,
  jsonFile = configurationFile
)

schedule <- getScheduleOfPlots(configurationPlan)

outputsTimeProfile1 <- getOutputsTimeProfile(plot = configurationPlan$plots$TimeProfile[[1]])
outputsTimeProfile2 <- getOutputsTimeProfile(plot = configurationPlan$plots$TimeProfile[[2]])
outputsTimeProfile3 <- getOutputsTimeProfile(plot = configurationPlan$plots$TimeProfile[[3]])

outputsGOF <- getOutputsGOFMergedPlotsPlot(plot = configurationPlan$plots$GOFMergedPlots[[1]])

gofTaskSettings <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals",outputsTimeProfile3)

workflow$simModel()
workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])


workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow,active = TRUE,settings = gofTaskSettings)
workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])



