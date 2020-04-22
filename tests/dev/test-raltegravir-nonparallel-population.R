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
popWorkFlow$simulatePopulation$settings$numberOfCores <- 1

popWorkFlow$populationSensitivityAnalysis$settings$showProgress <- FALSE
popWorkFlow$populationSensitivityAnalysis$settings$numberOfCores <- 1
popWorkFlow$populationSensitivityAnalysis$settings$variableParameterPaths <- "Organism|Heart|Volume"
popWorkFlow$populationSensitivityAnalysis$settings$variationRange <- 0.1
popWorkFlow$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
popWorkFlow$populationSensitivityAnalysis$settings$quantileVec <- c(0.25, 0.75)


popWorkFlow$runWorkflow()
