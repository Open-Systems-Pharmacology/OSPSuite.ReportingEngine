rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)

rootDir <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine"
setwd(rootDir)

runscript <- function() {
  workflowFolder <- file.path(rootDir, paste0("tests/dev/tracelib_nonpar_mean_ex_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")))
  simulationFile <- file.path(rootDir, "tests/dev/individualPksimSim.pkml")
  tree <- getSimulationTree(simulationFile)
  ms <- SimulationSet$new(
    simulationSetName = "meansim",
    simulationFile = simulationFile,
    outputs = Output$new(path = tree$Organism$Heart$Interstitial$smarties$Concentration$path)
  )

  mwf <- MeanModelWorkflow$new(simulationSets = list(ms), workflowFolder = workflowFolder)
  setwd(workflowFolder)

  mwf$simulate$settings$showProgress <- TRUE
  mwf$calculatePKParameters$activate()
  mwf$calculateSensitivity$activate()
  mwf$calculateSensitivity$settings$variableParameterPaths <- c(tree$Organism$Heart$Volume$path)
  mwf$runWorkflow()
}

runscript()
