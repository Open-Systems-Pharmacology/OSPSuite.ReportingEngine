rm(list = ls())
library(ospsuite.reportingengine)

simFile <- "PKML/Raltegravir 50 mg  (lactose formulation).pkml"

modelSet <- SimulationSet$new(
  simulationFile = simFile,
  pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  pathName = "Raltegravir",
  pathUnit = "mg/l"
)

workflow <- MeanModelWorkflow$new(
  simulationSets = modelSet,
  workflowFolder = "myFolder"
)

# workflow$getAllTasks()
# workflow$activateTasks(tasks = plotMassBalance)
# workflow$inactivateTasks(tasks = meanModelPKParameters)

# workflow$runWorkflow()
