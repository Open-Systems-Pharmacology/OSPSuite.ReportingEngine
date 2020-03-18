rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)


# Example:


# #SINGLE CORE
# setwd("./")
# devtools::load_all("./")
# simfile <- "./tests/dev/individualPksimSim.pkml"
# popfile <- "./tests/dev/popData.csv"
# pwf <- PopulationWorkflow$new(simulationFile = simfile,
#                               populationFile = popfile)
# pwf$setPopulationSimulationSettings()
# pwf$setPopulationPKParameterSettings()
# res<-pwf$runWorkflow()
# print(pwf$populationSimulation$generatedResultFileNames)
# print(pwf$populationPKParameters$generatedResultFileNames)


# MULTIPLE CORE
setwd("./")
devtools::load_all("./")


tictoc::tic()
simfile <- "./tests/dev/individualPksimSim.pkml"
popfile <- "./tests/dev/popData.csv"
pwf <- PopulationWorkflow$new(
  simulationFile = simfile,
  populationFile = popfile
)
pwf$setPopulationSimulationSettings(numberOfCores = 4)
pwf$setPopulationPKParameterSettings()
res <- pwf$runWorkflow()
print(pwf$populationSimulation$generatedResultFileNames)
print(pwf$populationPKParameters$generatedResultFileNames)
tictoc::toc()
