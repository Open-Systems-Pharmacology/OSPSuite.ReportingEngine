rm(list = ls())
library(ospsuite)
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")


inputFolderName <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data"
simulationFileName1 <- "individualPksimSim"
simulationFileName2 <- "simpleMobiEventSim"
populationFileName <- "popData"
# resultsFolderName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212"
# resultsFileName = "popSimRes"
# numberOfCores = 1

simFilePath1 <- file.path(inputFolderName, paste0(simulationFileName1, ".pkml"))
simFilePath2 <- file.path(inputFolderName, paste0(simulationFileName2, ".pkml"))
popFilePath <- file.path(inputFolderName, paste0(populationFileName, ".csv"))
print(simFilePath1)
print(simFilePath2)
simTree1 <- getSimulationTree(simFilePath1)
simTree2 <- getSimulationTree(simFilePath2)

# # Single core mean model simulation
# modelModelSimulationSet1 <- MeanModelSet$new(simulationFile = simFilePath1, simulationSetName = "SET1")
# meanModelWorkflow1  <- MeanModelWorkflow$new(simulationSets = list(modelModelSimulationSet1))
# meanModelWorkflow1 $meanModelSensitivityAnalysis$variableParameterPaths <- c("Organism|Hematocrit", "Organism|Pancreas|Volume")
# meanModelWorkflow1 $runWorkflow()

# Single core mean model simulation, two simulation sets, two SA parameters to vary
mm1 <- MeanModelSet$new(simulationFile = simFilePath1, simulationSetName = "SET1")
mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1))
# mmwf$meanModelSensitivityAnalysis$variableParameterPaths <- "Organism|Hematocrit"
mmwf$meanModelSensitivityAnalysis$variableParameterPaths <- c(simTree1$Organism$Hematocrit$path, simTree1$Organism$Pancreas$path)
mmwf$meanModelSensitivityAnalysis$activate()
mmwf$runWorkflow()


# # Single core mean model simulation, two simulation sets, two SA parameters to vary, throws error since second set doesn't have SA parameters to vary
# mm1 <- MeanModelSet$new(simulationFile = simFilePath1, simulationSetName = "SET1")
# mm2 <- MeanModelSet$new(simulationFile = simFilePath2, simulationSetName = "SET2")
# mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
# # mmwf$meanModelSensitivityAnalysis$variableParameterPaths <- "Organism|Hematocrit"
# mmwf$meanModelSensitivityAnalysis$variableParameterPaths <- c(simTree1$Organism$Hematocrit$path, simTree1$Organism$Pancreas$path)
# mmwf$meanModelSensitivityAnalysis$activate()
# mmwf$runWorkflow()


# #Single core mean model simulation & PK analysis, parallel sensitivity analysis
# mm1 <- MeanModelSet$new(simulationFile = simFilePath1,simulationSetName = "SET1")
# mm2 <- MeanModelSet$new(simulationFile = simFilePath2,simulationSetName = "SET2")
# mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
# mmwf$meanModelSensitivityAnalysis$numberOfCores = 2
# mmwf$runWorkflow()


# #Single core population simulation
# pp1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf<- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf$runWorkflow()



# #Parallel population simulation
# pp1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf_par<- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf_par$populationSimulation$numberOfCores =2
# ppwf_par$runWorkflow()


# #Single core population simulation and sensitivity analysis
# pp1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf_single_sa <- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf_single_sa$populationSimulation$numberOfCores <- 1
# ppwf_single_sa$populationSensitivityAnalysis$numberOfCores <- 1
# ppwf_single_sa$populationSensitivityAnalysis$variableParameterPaths <- c("Organism|Hematocrit","Organism|Pancreas|Volume")
# ppwf_single_sa$runWorkflow()

# ospsuite.reportingengine::runSensitivity(simFilePath1,
#                variableParameterPaths = "Organism|Hematocrit",
#                popFilePath = popFilePath,
#                individualId = 0,
#                variationRange = 0.1,
#                numberOfCores = 1,
#                resultsFileFolder = getwd(),
#                resultsFileName = "sssa"
# )

# # Parallel population sensitivity analysis
# pp1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
# ppwf_par_sa <- PopulationWorkflow$new(simulationSets = list(pp1))
# ppwf_par_sa$populationSimulation$numberOfCores <- 2
# ppwf_par_sa$populationSensitivityAnalysis$numberOfCores <- 6
# ppwf_par_sa$runWorkflow()
