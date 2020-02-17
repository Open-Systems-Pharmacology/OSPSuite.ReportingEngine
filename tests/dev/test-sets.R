rm(list = ls())
library(ospsuite)
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")


inputFolderName <- "C:/Users/ahamadeh/Dropbox/rproject/workflow"
simulationFileName1 <- "individualPksimSim"
simulationFileName2 <- "simpleMobiEventSim"
populationFileName = "popData"
# resultsFolderName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212"
# resultsFileName = "popSimRes"
# numberOfCores = 1

simFilePath1 <- file.path(inputFolderName, paste0(simulationFileName1, ".pkml"))
simFilePath2 <- file.path(inputFolderName, paste0(simulationFileName2, ".pkml"))
popFilePath <- file.path(inputFolderName, paste0(populationFileName, ".csv"))
print(simFilePath1)
print(simFilePath2)


#Workflow$new(simulationSets = list(mm1,mm2))
# mmwf<-MeanModelWorkflow$new(simulationSets = list(mm2))

# #Single core mean model simulation
# mm1 <- MeanModelSet$new(simulationFile = simFilePath1,simulationSetName = "SET1")
# mm2 <- MeanModelSet$new(simulationFile = simFilePath2,simulationSetName = "SET2")
# mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
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


#Parallel population sensitivity analysis
pp1 <- PopModelSet$new(simulationFile = simFilePath1, populationFile = popFilePath)
ppwf_par_sa<- PopulationWorkflow$new(simulationSets = list(pp1))
ppwf_par_sa$populationSimulation$numberOfCores =2
ppwf_par_sa$populationSensitivityAnalysis$numberOfCores =6
ppwf_par_sa$runWorkflow()

