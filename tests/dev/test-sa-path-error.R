rm(list = ls())
library(ospsuite)

# Package directory
rootDir <- "./"
devtools::load_all(rootDir)

# Where the sim and pop files for the workflow are located
dataDir <- file.path(rootDir, "tests", "dev", "ex_03_pop")

# Where the workflow results will be stored
workingDir <- file.path(rootDir, "tests", "dev", "sa_error")
setwd(workingDir)

# Setup first simulation set input file paths
simulationFileName1 <- "LarsonSim"
simFilePath1 <- file.path(dataDir, paste0(simulationFileName1, ".pkml"))

# Setup Second simulation set input file paths
simulationFileName2 <- "RaltegravirSim"
simFilePath2 <- file.path(dataDir, paste0(simulationFileName2, ".pkml"))

# Load simulation trees for path retrieval
load(file.path(dataDir, "simTrees.Rdata"))

# Setup workflow using two simulation sets
meanModelSimSet1 <- SimulationSet$new(simulationFile = simFilePath1)
meanModelSimSet2 <- SimulationSet$new(simulationFile = simFilePath2)

# Setup workflow
meanModelWorkflow <- MeanModelWorkflow$new(simulationSets = list(meanModelSimSet1, meanModelSimSet2))

# Number of cores for population sensitivity analysis
meanModelWorkflow$meanModelSensitivityAnalysis$activate()
meanModelWorkflow$meanModelSensitivityAnalysis$updateNumberOfCores(1)
meanModelWorkflow$meanModelSensitivityAnalysis$updateVariableParameterPaths(c(simTree1$Organism$Skin$Intracellular$CYP3A4$`Relative expression (normalized)`$path))

meanModelWorkflow$meanModelSensitivityAnalysis$quantileVec <- c(0.25, 0.75)
meanModelWorkflow$runWorkflow()
