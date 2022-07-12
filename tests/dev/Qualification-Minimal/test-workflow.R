#-------- Qualification Inputs --------#
# Configuration Plan is stored within a json file
configurationFile <- "reporting engine input/report-configuration-plan.json"
workflowFolder <- "reporting engine output"

#-------- Workflow Definition --------#
# Loading the workflow is done using the configuration plan
# which includes the knowledge of the models, their location...
# For ths reason, configurationPlan has become a field of workflow
workflow <- loadQualificationWorkflow(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

# Workflow works as mean model and population workflows
workflow$runWorkflow()

# Get the same logs and reports
list.files(workflow$workflowFolder)

# The main difference is that result folders follows section names
list.dir(workflow$workflowFolder)


#-------- Configuration Plan --------#
configurationPlan <- loadConfigurationPlan(
  workflowFolder = workflowFolder,
  configurationPlanFile = configurationFile
)

# The json fields are also within the ConfigurationPlan object
configurationPlan$sections
configurationPlan$simulationMappings
configurationPlan$observedDataSets

# The object also includes a bunch of methods to get the correct location of inputs and outputs
configurationPlan$getSectionPath(id = 3)
configurationPlan$getSectionMarkdown(id = 3)

configurationPlan$getSimulationPath(project = "Midazolam", simulation = "S1")
configurationPlan$getSimulationResultsPath(project = "Midazolam", simulation = "S1")
configurationPlan$getPKAnalysisResultsPath(project = "Midazolam", simulation = "S1")
