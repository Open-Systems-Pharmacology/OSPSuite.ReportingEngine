rm(list = ls())
library(ospsuite)
devtools::load_all(".")

load(file.path("./tests/dev/ex_03_pop/simTrees.Rdata"))


RaltegravirSimSet <- PopulationSimulationSet$new(
  simulationSetName = "Raltegravir",
  simulationFile = "./tests/dev/ex_03_pop/RaltegravirSim.pkml",
  populationFile = "./tests/dev/ex_03_pop/RaltegravirPop.csv"
)

LarsonSimSet <- PopulationSimulationSet$new(
  simulationSetName = "Larson",
  simulationFile = "./tests/dev/ex_03_pop/LarsonSim.pkml",
  populationFile = "./tests/dev/ex_03_pop/LarsonPop.csv"
)



popWorkFlow <- PopulationWorkflow$new(simulationSets = list(RaltegravirSimSet, LarsonSimSet))


popWorkFlow$simulatePopulation$settings$showProgress <- TRUE


popWorkFlow$populationSensitivityAnalysis$settings$variableParameterPaths <- simTree1$Organism$Heart$Volume$path


popWorkFlow$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max")


popWorkFlow$populationSensitivityAnalysis$settings$quantileVec <- c(0.5)


popWorkFlow$runWorkflow()
