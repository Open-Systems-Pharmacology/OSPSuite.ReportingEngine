rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "parpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv",
  pathID = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./expar")
pwf$simulatePopulation$settings$showProgress <- TRUE
pwf$simulatePopulation$settings$numberOfCores <- 3

pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
# pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path, tree$Organism$Pancreas$Volume$path)
pwf$populationSensitivityAnalysis$settings$numberOfCores <- 2
pwf$runWorkflow()
