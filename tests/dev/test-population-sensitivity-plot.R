rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)

simulationFile <- "./tests/dev/ex_03_pop9/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop9/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop9/LarPop10.csv"

simTree1 <- ospsuite::getSimulationTree(simulationFile)

ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(
    Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$Lung$Interstitial$Raltegravir$Concentration$path)
  )
)

ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile,
  populationFile = populationFile2,
  outputs = c(
    Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path)
  )
)

pwf <- PopulationWorkflow$new(simulationSets = list(ps1, ps2), workflowFolder = "./tests/dev/ex_03_pop9",workflowType = PopulationWorkflowTypes$parallelComparison)
pwf$simulate$settings$showProgress <- FALSE
pwf$simulate$inactivate()
pwf$calculatePKParameters$inactivate()
pwf$calculateSensitivity$inactivate()
pwf$plotSensitivity$activate()



pwf$calculateSensitivity$settings$showProgress <- TRUE
pwf$calculateSensitivity$settings$variableParameterPaths <- c(simTree1$Organism$Heart$Volume$path, simTree1$Organism$Lung$Volume$path, simTree1$Organism$Kidney$Volume$path)
pwf$calculateSensitivity$settings$pkParameterSelection <- c("C_max", "CL")
pwf$calculateSensitivity$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$plotSensitivity$settings <- SensitivityPlotSettings$new(totalSensitivityThreshold = 1)

pwf$runWorkflow()
