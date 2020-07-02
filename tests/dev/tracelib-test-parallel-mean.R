rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)

rootDir <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine"
setwd(rootDir)



runscript <- function(){

  workflowFolder <- file.path(rootDir,paste0("tests/dev/tracelib_par_mean_ex_",format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")))
  simulationFile <- file.path(rootDir,"tests/dev/individualPksimSim.pkml")

  tree <- ospsuite::getSimulationTree(simulationFile)
  ms <- SimulationSet$new(
    simulationSetName = "meansim",
    simulationFile = simulationFile,
    outputs = Output$new(path = "Organism|VenousBlood|Plasma|smarties|Concentration",
                         pkParameters =  c("C_max", "CL"))
  )

  mwf <- MeanModelWorkflow$new(simulationSets = list(ms) , workflowFolder = workflowFolder)
  setwd(workflowFolder)

  mwf$simulate$settings$showProgress <- TRUE
  mwf$meanModelPKParameters$activate()
  mwf$meanModelSensitivityAnalysis$activate()
  mwf$meanModelSensitivityAnalysis$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path,tree$Organism$Liver$Volume$path)
  mwf$meanModelSensitivityAnalysis$settings$numberOfCores <- 2
  mwf$runWorkflow()
}

runscript()
