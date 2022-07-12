#------------------------------
# Advanced example 03: Demography parameters
#------------------------------
library(ospsuite.reportingengine)
library(ospsuite)

output <- Output$new(path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)")

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    referencePopulation = FALSE,
    simulationSetName = "Larson",
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    populationFile = "Larson 2013 8-18y meal-Population.csv",
    outputs = output
  ),
  Adult = PopulationSimulationSet$new(
    referencePopulation = TRUE,
    simulationSetName = "Adults",
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    populationFile = "Raltegravir Adult Population.csv",
    outputs = output
  )
)

# Optional input: workflowType: parallelComparison (default), pediatric or ratioComparison
mySimpleWorkflow <- PopulationWorkflow$new(
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# If results were not already exported in myTestFolder/PKAnalysisResults
# you need to activate and run the task "populationPKParameters"
mySimpleWorkflow$activateTasks(tasks = "plotDemography")
mySimpleWorkflow$inactivateTasks(tasks = "simulatePopulation")

# For pediatric, check that one population was set as reference population with referencePopulation = TRUE
myPediatricWorkflow <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$pediatric,
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# If results were not already exported in myTestFolder/PKAnalysisResults
# you need to activate and run the task "populationPKParameters"
myPediatricWorkflow$activateTasks(tasks = "plotDemography")
myPediatricWorkflow$inactivateTasks(tasks = "simulatePopulation")

# Task "plotPKParameters": has xParameters and yParameters options
# Use <get/set><X/Y>parametersFor to check or update these options
# Tips: ospsuite::StandardPath to get list of usual demography parameters

getXParametersForDemogrpahyPlot(myPediatricWorkflow)
getYParametersForDemogrpahyPlot(myPediatricWorkflow)

setXParametersForDemogrpahyPlot(myPediatricWorkflow, NULL)
setYParametersForDemogrpahyPlot(myPediatricWorkflow, ospsuite::StandardPath$Weight)

myPediatricWorkflow$runWorkflow()
