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
pwf$simulatePopulation$settings$showProgress <- TRUE

pwf$populationSensitivityAnalysis$settings$variableParameterPaths <-  tree$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.5)
pwf$runWorkflow()
