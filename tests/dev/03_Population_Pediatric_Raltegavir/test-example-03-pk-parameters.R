#------------------------------
# Advanced example 03: Population PK parameters
#------------------------------
library(ospsuite.reportingengine)
library(ospsuite)

# Output will create `PkParameterInfo` objects if the names of the PK parameters are directly input
output <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c("AUC_inf", "C_max")
)

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
# Use <get/set><X/Y>parametersFor to check or update these options
# Caution: yParameters are actually Output objects

getXParametersForPkParametersPlot(myPediatricWorkflow)
getYParametersForPkParametersPlot(myPediatricWorkflow)

setXParametersForPkParametersPlot(myPediatricWorkflow, c(StandardPath$Age, StandardPath$Weight))

myAUC <- PkParameterInfo$new(
  pkParameter = "AUC_inf",
  displayName = "My AUC infinity",
  displayUnit = "nmol*min/l"
)
myCmax <- PkParameterInfo$new(
  pkParameter = "C_max",
  displayName = "My Cmax",
  displayUnit = "nmol"
)

myOutput <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c(myAUC, myCmax)
)

setYParametersForPkParametersPlot(myPediatricWorkflow, myOutput)

myPediatricWorkflow$runWorkflow()
