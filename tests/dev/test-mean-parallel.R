rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
tree <- ospsuite::getSimulationTree("./tests/dev/individualPksimSim.pkml")
ms <- SimulationSet$new(
  simulationSetName = "meansim-par-sa",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  outputs = Output$new(path = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path)
)
mwf <- MeanModelWorkflow$new(simulationSets = list(ms), workflowFolder = "./ex")
mwf$simulate$settings$showProgress <- TRUE
mwf$calculatePKParameters$activate()
mwf$calculateSensitivity$activate()
mwf$calculateSensitivity$settings$variableParameterPaths <- c(
  tree$Organism$Heart$Volume$path,
  tree$Organism$Pancreas$Volume$path
)
mwf$calculateSensitivity$settings$numberOfCores <- 2
mwf$runWorkflow()
