rm(list = ls())
library(ospsuite)

#Package directory
rootDir <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine"
devtools::load_all(rootDir)

#Where the sim and pop files for the workflow are located
dataDir <- file.path(rootDir,"data","ex_03_pop")

#Where the workflow results will be stored
workingDir <- "C:/Users/ahamadeh/Dropbox/rproject/workflow/ex_03_pop/"
setwd(workingDir)

#Setup first simulation set input file paths
simulationFileName1 <- "LarsonSim"
populationFileName1 <- "LarsonPop"
simFilePath1 <- file.path(dataDir, paste0(simulationFileName1, ".pkml"))
popFilePath1 <- file.path(dataDir, paste0(populationFileName1, ".csv"))

#Setup Second simulation set input file paths
simulationFileName2 <- "RaltegravirSim"
populationFileName2 <- "RaltegravirPop"
simFilePath2 <- file.path(dataDir, paste0(simulationFileName2, ".pkml"))
popFilePath2 <- file.path(dataDir, paste0(populationFileName2, ".csv"))

#Load simulation trees for path retrieval
load(file.path(dataDir,"simTrees.Rdata"))

# Setup workflow using two simulation sets
popSimSet1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath1)
popSimSet2 <- PopModelSet$new(simulationFile = simFilePath2, populationFile = popFilePath2)

#Setup workflow
#popWorkFlow <- PopulationWorkflow$new(simulationSets = list(popSimSet2))
popWorkFlow <- PopulationWorkflow$new(simulationSets = list(popSimSet1,popSimSet2))

#Number of cores for population simulation
popWorkFlow$populationSimulation$numberOfCores <- 4

#Number of cores for population sensitivity analysis
popWorkFlow$populationSensitivityAnalysis$numberOfCores <- 4
popWorkFlow$populationSensitivityAnalysis$variableParameterPaths <- c(simTree1$Organism$Skin$Volume$path,
                                                                      simTree1$Organism$Skin$`Specific blood flow rate`$path,
                                                                      simTree1$Organism$Pancreas$Volume$path,
                                                                      simTree1$Organism$Heart$Volume$path)
popWorkFlow$populationSensitivityAnalysis$pkParameterSelection <- c("C_max","AUC")

popWorkFlow$populationSensitivityAnalysis$quantileVec <- c(0.25,0.75)
popWorkFlow$runWorkflow()
