#----------------------------------------------------------------------#
# Advanced example 01: Plot absorption profile for mean model workflow #
#----------------------------------------------------------------------#

library(ospsuite.reportingengine)

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
myMeanWorkflow$activateTasks(c("plotAbsorption"))
myMeanWorkflow$inactivateTasks(c("simulate"))

myMeanWorkflow$runWorkflow()
