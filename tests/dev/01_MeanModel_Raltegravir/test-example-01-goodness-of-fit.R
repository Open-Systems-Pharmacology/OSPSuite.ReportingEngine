#------------------------------
# Advanced example 01: Mean model workflow time profile and residuals
#------------------------------

library(ospsuite.reportingengine)

output10mg <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Raltegravir simulated data",
  displayUnit = "nmol/l",
  dataFilter = "Grouping %in% '10mg_'",
  dataDisplayName = "Raltegravir observed data"
)
output50mg <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Raltegravir simulated data",
  displayUnit = "nmol/l",
  dataFilter = "Grouping %in% '50mg'",
  dataDisplayName = "Raltegravir observed data"
)

meanModelSets <- list(
  SimulationSet$new(
    simulationSetName = "10 mg",
    simulationFile = "PKML/Raltegravir 10 mg   (lactose formulation).pkml",
    outputs = output10mg,
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
  ),
  SimulationSet$new(
    simulationSetName = "50 mg",
    simulationFile = "PKML/Raltegravir 50 mg  (lactose formulation).pkml",
    outputs = output50mg,
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
  )
)

myMeanWorkflow <- MeanModelWorkflow$new(simulationSets = meanModelSets, workflowFolder = "myTestFolder")

# If results were not already exported in myTestFolder/SimulationResults
# you need to activate and run the task "simulate"
# Tip: use the enum taskNames to get directly the name of each task

myMeanWorkflow$activateTasks(tasks = myMeanWorkflow$taskNames$plotGoF)
myMeanWorkflow$inactivateTasks(tasks = myMeanWorkflow$taskNames$simulate)

myMeanWorkflow$runWorkflow()
