rm(list = ls())
library(ospsuite)
setwd("./")
devtools::load_all("./")


inputFolderName <- "./tests/dev"
simulationFileName1 <- "individualPksimSim"
simulationFileName2 <- "simpleMobiEventSim"
populationFileName <- "popData_short" #"popData"
# resultsFolderName = "./tests/dev/"
# resultsFileName = "popSimRes"
# numberOfCores = 1

simFilePath1 <- file.path(inputFolderName, paste0(simulationFileName1, ".pkml"))
simFilePath2 <- file.path(inputFolderName, paste0(simulationFileName2, ".pkml"))
popFilePath <- file.path(inputFolderName, paste0(populationFileName, ".csv"))
print(simFilePath1)
print(simFilePath2)

simTree1 <- getSimulationTree(simFilePath1)
simTree2 <- getSimulationTree(simFilePath2)

# Workflow$new(simulationSets = list(mm1,mm2))
# mmwf<-MeanModelWorkflow$new(simulationSets = list(mm2))

# #Single core mean model simulation
# mm1 <- SimulationSet$new(simulationFile = simFilePath1,simulationSetName = "SET1")
# mm2 <- SimulationSet$new(simulationFile = simFilePath2,simulationSetName = "SET2")
# mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
# mmwf$runWorkflow()


# # Single core mean model simulation & PK analysis, parallel sensitivity analysis
# mm1 <- SimulationSet$new(simulationFile = simFilePath1, simulationSetName = "SET1")
# mm2 <- SimulationSet$new(simulationFile = simFilePath2, simulationSetName = "SET2")
# mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
# mmwf$meanModelSensitivityAnalysis$numberOfCores <- 2
# mmwf$runWorkflow()


# #Single core population simulation
# pp1 <- PopulationSimulationSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf<- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf$populationSensitivityAnalysis$inactivate()
# ppwf$runWorkflow()



#Parallel population simulation
pp1 <- PopulationSimulationSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
ppwf_par<- PopulationWorkflow$new(simulationSets = list(pp1))
ppwf_par$populationSimulation$numberOfCores <- 6
ppwf_par$populationSensitivityAnalysis$variableParameterPaths <- c(simTree1$Organism$Bone$Volume$path,simTree1$Organism$`Plasma protein scale factor`$path)
ppwf_par$populationSensitivityAnalysis$numberOfCores <- 6
#ppwf_par$populationSensitivityAnalysis$inactivate()
ppwf_par$runWorkflow()


# #Single core population simulation and sensitivity analysis
# pp1 <- PopulationSimulationSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf_single_sa <- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf_single_sa$populationSimulation$numberOfCores <- 1
# ppwf_single_sa$populationSensitivityAnalysis$numberOfCores <- 1
# ppwf_single_sa$runWorkflow()



# # Parallel population sensitivity analysis
# pp1 <- PopulationSimulationSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf_par_sa <- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf_par_sa$populationSimulation$numberOfCores <- 2
# ppwf_par_sa$populationSensitivityAnalysis$numberOfCores <- 6
# ppwf_par_sa$runWorkflow()
