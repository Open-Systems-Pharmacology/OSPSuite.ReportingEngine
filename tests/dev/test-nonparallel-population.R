rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "nonparpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv",
  outputs = Output$new(path = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path)
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./exnonpar",workflowType = PopulationWorkflowTypes$parallelComparison)
pwf$simulate$settings$showProgress <- FALSE
pwf$simulate$activate()
pwf$calculatePKParameters$activate()
pwf$calculateSensitivity$activate()
pwf$calculateSensitivity$settings$showProgress <- TRUE
pwf$calculateSensitivity$settings$pkParameterSelection <- c("C_max", "CL")
pwf$calculateSensitivity$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)
pwf$runWorkflow()
