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
  configurationPlanFile = configurationFile
)


#-------- Configuration Plan --------#
configurationPlan <- loadConfigurationPlan(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

configurationPlan$sections
configurationPlan$simulationMappings
configurationPlan$observedDataSets


outputsTimeProfile1 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[1]])
outputsTimeProfile2 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[2]])
outputsTimeProfile3 <- getOutputsFromTimeProfileConfiguration(plot = configurationPlan$plots$TimeProfile[[3]])

outputsGOF <- getOutputsFromGOFMergedPlotsConfiguration(plot = configurationPlan$plots$GOFMergedPlots[[1]])



workflow$simModel()

# Time profile plot 1
gofTaskSettings1 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile1)
workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings1)
workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[2])

# Time profile plot 2
gofTaskSettings2 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile2)
workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings2)
workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])

# Time profile plot 3
gofTaskSettings3 <- GofTaskSettings$new(taskName = "plotTimeProfilesAndResiduals", outputsTimeProfile3)
workflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(workflow = workflow, active = TRUE, settings = gofTaskSettings3)
workflow$plotTimeProfilesAndResiduals$runTask(structureSets = workflow$simulationStructures[3])
