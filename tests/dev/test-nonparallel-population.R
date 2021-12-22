rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- ospsuite::getSimulationTree("./tests/data/input-data/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "nonparpopsim",
  simulationFile = "./tests/data/input-data/individualPksimSim.pkml",
  populationFile = "./tests/data/input-data/popData_short.csv",
  outputs = Output$new(path = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path)
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./exnonpar", workflowType = PopulationWorkflowTypes$parallelComparison)
pwf$simulate$settings$showProgress <- FALSE
pwf$simulate$activate()
pwf$calculatePKParameters$activate()
pwf$calculateSensitivity$activate()
pwf$calculateSensitivity$settings$showProgress <- TRUE
pwf$calculateSensitivity$settings$pkParameterSelection <- c("C_max", "CL")
pwf$calculateSensitivity$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)
pwf$runWorkflow()
