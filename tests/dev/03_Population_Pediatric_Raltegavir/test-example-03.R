#------------------------------
# Advanced example 03: Set/run population workflow
#------------------------------

library(ospsuite.reportingengine)

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathName = "Raltegravir",
    populationFile = "Larson 2013 8-18y meal-Population.csv"
  ),
  Adult = PopulationSimulationSet$new(
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathName = "Raltegravir",
    populationFile = "Raltegravir Adult Population.csv"
  )
)

myWorkflow <- PopulationWorkflow$new(
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

myWorkflow$getAllTasks()

myWorkflow$activateTasks(tasks = c("simulatePopulation", "populationPKParameters"))

myWorkflow$runWorkflow()
