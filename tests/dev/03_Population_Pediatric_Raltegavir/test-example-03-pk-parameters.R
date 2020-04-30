#------------------------------
# Advanced example 03: Population PK parameters
#------------------------------
library(ospsuite.reportingengine)
library(ospsuite)

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    referencePopulation = FALSE,
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    populationFile = "Larson 2013 8-18y meal-Population.csv"
  ),
  Adult = PopulationSimulationSet$new(
    referencePopulation = TRUE,
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    populationFile = "Raltegravir Adult Population.csv"
  )
)

# Optional input: workflowType: parallelComparison (default), pediatric or ratioComparison
mySimpleWorkflow <- PopulationWorkflow$new(
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)
# For ratio, check that one population was set as reference population with referencePopulation = TRUE
myRatioWorkflow <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$ratioComparison,
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# myRatioWorkflow$runWorkflow()

# If results were not already exported in myTestFolder/PKAnalysisResults
# you need to activate and run the task "populationPKParameters"
myRatioWorkflow$activateTasks(tasks = "plotPKParameters")
myRatioWorkflow$inactivateTasks(tasks = c("simulatePopulation", "populationPKParameters"))

# For pediatric, check that one population was set as reference population with referencePopulation = TRUE
myPediatricWorkflow <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$pediatric,
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# If results were not already exported in myTestFolder/PKAnalysisResults
# you need to activate and run the task "populationPKParameters"
myPediatricWorkflow$activateTasks(tasks = "plotPKParameters")
myPediatricWorkflow$inactivateTasks(tasks = c("simulatePopulation", "populationPKParameters"))

# Task "plotPKParameters": has xParameters and yParameters options
# - xParameters are path of demography parameters for VPC like range plots (use ospsuite::StandardPath)
# - yParameters are selected PK parameters to be exported in output (use ospsuite::StandardPKParameter)

myPediatricWorkflow$plotPKParameters$xParameters <- c(StandardPath$Age, StandardPath$Weight)
myPediatricWorkflow$plotPKParameters$yParameters <- "AUC_inf"

myPediatricWorkflow$runWorkflow()
