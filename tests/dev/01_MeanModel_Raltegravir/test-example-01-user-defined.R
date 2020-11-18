# Example 01 test script to upload a user-defined plot task
# Template function is available in "../script-template-user-defined-function.R"
require(ospsuite.reportingengine)
source("../script-template-user-defined-function.R")

workflowFolder <- "myWorkflowResults"

meanModelSets <- list(
  SimulationSet$new(
    simulationSetName = "Raltegravir 10 mg",
    simulationFile = "PKML/Raltegravir 10 mg   (lactose formulation).pkml"
  ),
  SimulationSet$new(
    simulationSetName = "Raltegravir 50 mg",
    simulationFile = "PKML/Raltegravir 50 mg  (lactose formulation).pkml"
  )
)

myMeanWorkflow <- MeanModelWorkflow$new(simulationSets = meanModelSets, workflowFolder = workflowFolder)
myMeanWorkflow$inactivateTasks()
myMeanWorkflow$activateTasks(c("plotAbsorption"))

# load user defined task
addUserDefinedTask(workflow = myMeanWorkflow,
                    taskFunction =plotApplicationProfile ,
                    taskName = "Application Profile",
                    active = TRUE,
                    settings = list(plotConfiguration = tlf::PlotConfiguration$new(watermark = "User Defined Plot")))

myMeanWorkflow$runWorkflow()
