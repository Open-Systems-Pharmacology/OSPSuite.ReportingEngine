rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
load("./tests/dev/ex_03_pop/simTrees.Rdata")

simulationFile <- "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile <- "./tests/dev/ex_03_pop/RalPop10.csv"

ps <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile,
  outputs = Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path)
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./tests/dev/ex_03_pop")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
# pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- simTree1$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile

plotConfig <- tlf::PlotConfiguration$new()

pL <- plotPopulationSensitivity(
  structureSet = pwf$simulationStructures[[1]],
  settings = plotConfig
)
show(pL$plots$`C_max-Organism|VenousBlood|Plasma|Raltegravir|Concentration`)
