rm(list = ls())
library(ospsuite)
devtools::load_all(".")
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "parpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv"
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps))
pwf$simulatePopulation$settings$updateShowProgress(TRUE)
pwf$simulatePopulation$settings$updateNumberOfCores(4)

pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
pwf$populationSensitivityAnalysis$updateQuantileVec(c(0.25, 0.75))
pwf$populationSensitivityAnalysis$updatePKParameterSelection(c("C_max", "AUC_inf"))
pwf$populationSensitivityAnalysis$updateVariableParameterPaths(c(tree$Organism$Heart$Volume$path, tree$Organism$Pancreas$Volume$path))
pwf$populationSensitivityAnalysis$updateNumberOfCores(2)
pwf$runWorkflow()
