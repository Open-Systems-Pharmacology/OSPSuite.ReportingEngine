rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)


tictoc::tic()

setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow")



# INPUTS ARE FOLDER OR PATH TO SIMULATION, LIST OF PARAMETERS (OPTIONAL,DEFAULT NULL) AND NUMBER OF CORES (OPTIONAL, DEFAULT 1), LIST OF OUTPUTS (OPTIONAL??), LIST OF PK PARAMS (OPTIONAL DEFAULT NULL)
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv" # DEFAULT NULL
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


mwf <- MeanModelWorkflow$new(simulationFile = simFilePath)
mwf$setMeanModelSimulationSettings()
mwf$setSensitivityAnalysisSettings(active = TRUE)
mwf$runWorkflow()




tictoc::toc()








