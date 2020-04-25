rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
devtools::load_all(".")
load("./tests/dev/ex_03_pop/simTrees.Rdata")

simulationFile = "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile = "./tests/dev/ex_03_pop/RalPop10.csv"

ps <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile,
  pathID = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./tests/dev/ex_03_pop")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$activate()
pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
#pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- simTree1$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile
pwf$runWorkflow()


structureSet = pwf$simulationStructures[[1]]
pkParameter = c("C_max")
output = pwf$simulationStructures[[1]]$simulationSet$pathID[1]
quantiles = c(0.25,0.5)
rankFilter = 10
title <- paste("Population sensitivity of",pkParameter,"of",output)

sortedFilteredIndividualsDfForPKParameter <- getPopSensDfForPkAndOutput(structureSet = structureSet,
                                                                        pkParameter = pkParameter,
                                                                        output = output,
                                                                        quantiles = quantiles,
                                                                        rankFilter = rankFilter)


plt <- getPkParameterPopulationSensitivityPlot(data = sortedFilteredIndividualsDfForPKParameter,title = title)




show(plt)
