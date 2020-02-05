rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)

devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
tictoc::tic()

# # SINGLE CORE SA
# setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow")
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
# mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
# mwf$setMeanModelSimulationSettings()
# mwf$setMeanModelSensitivityAnalysisSettings()
# mwf$runWorkflow()


# MULTI CORE SA
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow")
simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
mwf$setMeanModelSimulationSettings(calculatePKParameters = FALSE)
mwf$setMeanModelSensitivityAnalysisSettings(numberOfCores = 4)
mwf$runWorkflow()



# INPUTS ARE FOLDER OR PATH TO SIMULATION, LIST OF PARAMETERS (OPTIONAL,DEFAULT NULL) AND NUMBER OF CORES (OPTIONAL, DEFAULT 1), LIST OF OUTPUTS (OPTIONAL??), LIST OF PK PARAMS (OPTIONAL DEFAULT NULL)
#simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# popObject <- loadPopulation(popFilePath)
# individualParameters <- popObject$getParameterValuesForIndividual(individualId = 2) # DEFAULT NULL
#
# numberOfCores <- 2
#
#
# resultsFileFolder <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/tests/dev/"
# resultsFileName <- "SAResults"
#
# parametersToPerturb <- NULL






tictoc::toc()








