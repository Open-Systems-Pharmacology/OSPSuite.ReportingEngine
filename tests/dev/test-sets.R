rm(list = ls())
library(ospsuite)
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")


inputFolderName <- "C:/Users/ahamadeh/Dropbox/rproject/workflow"
simulationFileName1 <- "individualPksimSim"
simulationFileName2 <- "simpleMobiEventSim"
# populationFileName = "popData"
# resultsFolderName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212"
# resultsFileName = "popSimRes"
# numberOfCores = 1

simFilePath1 <- file.path(inputFolderName, paste0(simulationFileName1, ".pkml"))
simFilePath2 <- file.path(inputFolderName, paste0(simulationFileName2, ".pkml"))
print(simFilePath1)
print(simFilePath2)
mm1 <- MeanModelSet$new(simulationFile = simFilePath1)
mm2 <- MeanModelSet$new(simulationFile = simFilePath2)

# Workflow$new(simulationSets = list(mm1,mm2))

mmwf <- MeanModelWorkflow$new(simulationSets = list(mm1, mm2))
# mmwf<-MeanModelWorkflow$new(simulationSets = list(mm2))
mmwf$runWorkflow()
