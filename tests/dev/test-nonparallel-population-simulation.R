rm(list = ls())
library(ospsuite)
devtools::load_all(".")
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "nonparpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv"
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps))
pwf$simulatePopulation$settings$updateShowProgress(TRUE)

pwf$populationSensitivityAnalysis$updateVariableParameterPaths(tree$Organism$Heart$Volume$path)
pwf$runWorkflow()
