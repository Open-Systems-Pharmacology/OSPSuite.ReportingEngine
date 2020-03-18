rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)

devtools::load_all("./")
tictoc::tic()

# # SINGLE CORE SA
# setwd("./")
# simFilePath <- "./tests/dev/simpleMobiEventSim.pkml"
# mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
# mwf$setMeanModelSimulationSettings()
# mwf$setMeanModelPKParameterSettings()
# mwf$setMeanModelSensitivityAnalysisSettings()
# mwf$runWorkflow()
# print(mwf$meanModelSimulation$generatedResultFileNames)
# print(mwf$meanModelPKParameters$generatedResultFileNames)
# print(mwf$meanModelSensitivityAnalysis$generatedResultFileNames)


# # MULTI CORE SA
setwd("./")
simFilePath <- "./tests/dev/individualPksimSim.pkml"
mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
mwf$setMeanModelSimulationSettings()
mwf$setMeanModelPKParameterSettings()
mwf$setMeanModelSensitivityAnalysisSettings(numberOfCores = 4)
mwf$runWorkflow()
print(mwf$meanModelSimulation$generatedResultFileNames)
print(mwf$meanModelPKParameters$generatedResultFileNames)
print(mwf$meanModelSensitivityAnalysis$generatedResultFileNames)

tictoc::toc()
