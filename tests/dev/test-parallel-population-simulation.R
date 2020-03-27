rm(list=ls())
library(ospsuite)
devtools::load_all(".");
ps <- PopulationSimulationSet$new(simulationSetName = "parpopsim",
                                  simulationFile = "./tests/dev/individualPksimSim.pkml",
                                  populationFile = "./tests/dev/popData.csv");
pwf <- PopulationWorkflow$new(simulationSets = list(ps));
pwf$simulatePopulation$settings$updateShowProgress(TRUE)
pwf$simulatePopulation$settings$updateNumberOfCores(4)
pwf$runWorkflow()
