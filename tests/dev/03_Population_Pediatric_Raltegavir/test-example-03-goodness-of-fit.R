#------------------------------
# Advanced example 03: Population time profile and residuals
#------------------------------

library(ospsuite.reportingengine)

# Use of set plot format to change the properties of export
setPlotFormat(format = "pdf")

LarsonOutput <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayUnit = "µg/l",
  displayName = "Raltegravir",
  dataFilter = 'Grouping %in% "8-18y"'
)
AdultOutput <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayUnit = "µg/l",
  displayName = "Raltegravir",
  dataFilter = 'Grouping %in% "400mg_FCT"'
)

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    simulationSetName = "Larson",
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    outputs = LarsonOutput,
    populationFile = "Larson 2013 8-18y meal-Population.csv",
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
  ),
  Adult = PopulationSimulationSet$new(
    simulationSetName = "Adult",
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    outputs = AdultOutput,
    populationFile = "Raltegravir Adult Population.csv",
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
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
