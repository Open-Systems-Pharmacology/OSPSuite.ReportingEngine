#------------------------------
# Advanced example 03: Population time profile and residuals
#------------------------------

library(ospsuite.reportingengine)

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathUnit = "µg/l",
    pathName = "Raltegravir",
    populationFile = "Larson 2013 8-18y meal-Population.csv",
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv",
    dataFilter = 'Grouping %in% "8-18y"'
  ),
  Adult = PopulationSimulationSet$new(
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathUnit = "µg/l",
    pathName = "Raltegravir",
    populationFile = "Raltegravir Adult Population.csv",
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv",
    dataFilter = 'Grouping %in% "400mg_FCT"'
  )
)


myWorkflow <- PopulationWorkflow$new(
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# If results were not already exported in myTestFolder/SimulationResults
# you need to activate and run the task "simulatePopulation"

myWorkflow$activateTasks(tasks = "plotGoF")
myWorkflow$inactivateTasks(tasks = "simulatePopulation")

myWorkflow$runWorkflow()
