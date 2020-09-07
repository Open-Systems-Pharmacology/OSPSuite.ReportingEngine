rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- ospsuite::getSimulationTree("./tests/data/input-data/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "parpopsim",
  simulationFile = "./tests/data/input-data/individualPksimSim.pkml",
  populationFile = "./tests/data/input-data/popData_short.csv",
  outputs = Output$new(path = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path)
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./expar", workflowType = PopulationWorkflowTypes$parallelComparison )
pwf$simulate$settings$showProgress <- TRUE
pwf$simulate$settings$numberOfCores <- 3

pwf$calculatePKParameters$activate()
pwf$calculateSensitivity$activate()
pwf$calculateSensitivity$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)
pwf$calculateSensitivity$settings$pkParameterSelection <- c("C_max", "CL")
pwf$calculateSensitivity$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path, tree$Organism$Pancreas$Volume$path)
pwf$calculateSensitivity$settings$numberOfCores <- 2
pwf$runWorkflow()
