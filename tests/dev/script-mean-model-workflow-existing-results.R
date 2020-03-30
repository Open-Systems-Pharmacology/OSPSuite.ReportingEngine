rm(list = ls())
library(ospsuite.reportingengine)

simFile <- "PKML/Raltegravir 50 mg  (lactose formulation).pkml"

modelSet <- SimulationSet$new(simulationFile = simFile,
                              pathID = 'Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)',
                              pathName = 'Raltegravir',
                              pathUnit = 'mg/l')
  
# If "myWorkflowPreviousResults" already exists, used its results
workflow <- MeanModelWorkflow$new(simulationSets = modelSet,
                                  workflowFolder = "myWorkflowPreviousResults")

# workflow$getAllTasks()
workflow$inactivateTasks(tasks = simulate)
workflow$activateTasks(tasks = plotGof)

# workflow$runWorkflow()