rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)

devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
tictoc::tic()

# SINGLE CORE SA
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow")
simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
mwf$setMeanModelSimulationSettings()
mwf$setMeanModelSensitivityAnalysisSettings()
mwf$runWorkflow()


# # # MULTI CORE SA
# setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow")
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
# mwf$setMeanModelSimulationSettings(calculatePKParameters = TRUE)
# mwf$setMeanModelSensitivityAnalysisSettings(numberOfCores = 4)
# mwf$runWorkflow()

tictoc::toc()
