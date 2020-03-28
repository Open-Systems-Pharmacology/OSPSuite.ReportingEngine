rm(list = ls())
library(ospsuite)
devtools::load_all(".")
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
  ms <- SimulationSet$new(
  simulationSetName = "meansim",
  simulationFile = "./tests/dev/individualPksimSim.pkml"
)
mwf <- MeanModelWorkflow$new(simulationSets = list(ms))
mwf$simulate$settings$showProgress <- TRUE
mwf$meanModelPKParameters$activate()
mwf$meanModelSensitivityAnalysis$activate()
mwf$meanModelSensitivityAnalysis$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path)
mwf$runWorkflow()
