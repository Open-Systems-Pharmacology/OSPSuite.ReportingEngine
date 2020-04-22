rm(list = ls())
library(ospsuite)
devtools::load_all(".")
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "nonparpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv",
  pathID = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./ex")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$activate()
pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- tree$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max","CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.25,0.75)
pwf$runWorkflow()
