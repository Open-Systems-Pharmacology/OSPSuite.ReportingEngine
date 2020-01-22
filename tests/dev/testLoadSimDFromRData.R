library(ospsuite)
library(ospsuite.reportingengine)
simFile <- "./data/simpleMobiEventSim.pkml"
sim <- loadSimulation(simFile)
save(sim,file = "./data/testLoadSim.Rdata")
rm(list = ls())

library(ospsuite)
library(ospsuite.reportingengine)
load(file = "./data/testLoadSim.Rdata")
print(sim$outputSelections)
