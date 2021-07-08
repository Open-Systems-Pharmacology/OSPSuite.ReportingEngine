# library(ospsuite.reportingengine)
rm(list = ls())
library(ospsuite)
graphics.off()
setwd(dir = "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/")
devtools::load_all(".")


re.example.dir <- "tests/dev/Qualification-Minimal"
minimal.example.dir <- "../QualificationPlan/examples/minimal"
advanced.example.dir <- "../QualificationPlan/examples/advanced_01/"
example.dirs <- c(re.example.dir, minimal.example.dir, advanced.example.dir)
example.number <- 3

setwd(dir = example.dirs[example.number])

#-------- Qualification Inputs --------#
# Configuration Plan is stored within a json file

inputFolder <- "reporting engine input"
configurationFile <- file.path(inputFolder, "small-report-configuration-plan.json")
workflowFolder <- "reporting engine output"

# #-------- Workflow Definition --------#
workflow <- loadQualificationWorkflow(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

workflow$inactivateTasks("simulate")
workflow$inactivateTasks("plotTimeProfiles")
# # Workflow works as mean model and population workflows
workflow$runWorkflow()

#-------- Configuration Plan --------#
configurationPlan <- loadConfigurationPlan(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

configurationPlan$sections
configurationPlan$simulationMappings
configurationPlan$observedDataSets
