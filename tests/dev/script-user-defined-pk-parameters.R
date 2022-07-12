rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)

# One time initialization of PK Parameters
ospsuite::updatePKParameter("C_max", displayName = "C max")
ospsuite::updatePKParameter("C_max_norm", displayUnit = "kg/l")
ospsuite::updatePKParameter("t_max", displayUnit = "h")
ospsuite::updatePKParameter("AUC_tEnd", displayName = "AUC tEnd")
myTMax <- ospsuite::addUserDefinedPKParameter("My T_MAX", standardPKParameter = StandardPKParameter$t_max, displayUnit = "min")
myTMax$startTime <- 10

simFile <- "tests/dev/01_MeanModel_Raltegravir/PKML/Raltegravir 10 mg   (lactose formulation).pkml"

modelSet <- SimulationSet$new(
  simulationFile = simFile,
  pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  pathName = "Raltegravir",
  pathUnit = "µg/l"
)


meanModel_Raltegravir <- MeanModelWorkflow$new(simulationSets = modelSet)

# Tasks to activate/inactivate (this can also be done using meanModel_Raltegravir$activateTasks() method)
activateWorkflowTasks(meanModel_Raltegravir, tasks = c("plotGoF", "plotAbsorption", "plotMassBalance"))
inactivateWorkflowTasks(meanModel_Raltegravir, tasks = c("meanModelPKParameters"))

# Run the mean model workflow
meanModel_Raltegravir$runWorkflow()
