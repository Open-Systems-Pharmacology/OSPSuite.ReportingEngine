rm(list = ls())
library(ospsuite)
library(tictoc)

#Package directory
rootDir <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine"
devtools::load_all(rootDir)

#Where the sim and pop files for the workflow are located
dataDir <- file.path(rootDir,"tests","dev","ex_03_pop")

#Where the workflow results will be stored
workingDir <- file.path(rootDir,"tests","dev","ten_ex_03_pop")
setwd(workingDir)

#Setup first simulation set input file paths
simulationFileName1 <- "LarsonSim"
populationFileName1 <- "ten_LarsonPop"
simFilePath1 <- file.path(dataDir, paste0(simulationFileName1, ".pkml"))
popFilePath1 <- file.path(dataDir, paste0(populationFileName1, ".csv"))

#Load simulation trees for path retrieval
load(file.path(dataDir,"simTrees.Rdata"))

# Setup workflow using two simulation sets
popSimSet1 <- PopulationSimulationSet$new(simulationFile = simFilePath1, populationFile = popFilePath1)

#Setup workflow
#popWorkFlow <- PopulationWorkflow$new(simulationSets = list(popSimSet2))
popWorkFlow <- PopulationWorkflow$new(simulationSets = list(popSimSet1))

#Number of cores for population simulation
popWorkFlow$populationSimulation$updateNumberOfCores(1)

#Number of cores for population sensitivity analysis
popWorkFlow$populationSensitivityAnalysis$updateNumberOfCores(1)
popWorkFlow$populationSensitivityAnalysis$updateVariableParameterPaths(c(simTree1$Organism$Skin$Volume$path))
popWorkFlow$populationSensitivityAnalysis$updatePKParameterSelection(c("C_max"))

popWorkFlow$populationSensitivityAnalysis$updateQuantileVec(c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95))
popWorkFlow$runWorkflow()
