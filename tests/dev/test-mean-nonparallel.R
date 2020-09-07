rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- ospsuite::getSimulationTree("./tests/data/input-data/individualPksimSim.pkml")
ms <- SimulationSet$new(
  simulationSetName = "meansim",
  simulationFile = "./tests/data/input-data/individualPksimSim.pkml",
  outputs = Output$new(path = tree$Organism$Heart$Interstitial$smarties$Concentration$path)
)

mwf <- MeanModelWorkflow$new(simulationSets = list(ms), workflowFolder = "./ex")
mwf$simulate$settings$showProgress <- TRUE
mwf$calculatePKParameters$activate()
mwf$calculateSensitivity$activate()
mwf$calculateSensitivity$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path)
mwf$runWorkflow()
