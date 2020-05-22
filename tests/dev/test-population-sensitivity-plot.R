rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)
load("./tests/dev/ex_03_pop/simTrees.Rdata")

simulationFile <- "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop/LarPop10.csv"

ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
              Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path),
              Output$new(path = simTree1$Organism$Lung$Interstitial$Raltegravir$Concentration$path))
)

ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile,
  populationFile = populationFile2,
  outputs = c(Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
              Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path))
)

pwf <- PopulationWorkflow$new(simulationSets = list(ps1,ps2), workflowFolder = "./tests/dev/ex_03_pop")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
pwf$plotSensitivity$activate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c(simTree1$Organism$Heart$Volume$path,simTree1$Organism$Lung$Volume$path,simTree1$Organism$Kidney$Volume$path)
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)
pwf$runWorkflow()
