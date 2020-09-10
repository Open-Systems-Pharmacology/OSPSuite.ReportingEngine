rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)

rootDir <- "."
setwd(rootDir)

runscript <- function() {
  workflowFolder <- file.path(rootDir, paste0("tests/dev/tracelib_nonpar_pop_ex_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")))
  ps <- PopulationSimulationSet$new(
    simulationSetName = "nonparpopsim",
    simulationFile = file.path(rootDir, "tests/data/input-data/individualPksimSim.pkml"),
    populationFile = file.path(rootDir, "tests/data/input-data/popData_short.csv"),
    outputs = Output$new(
      path = "Organism|VenousBlood|Plasma|smarties|Concentration",
      pkParameters = c("C_max", "CL")
    )
  )
  pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = workflowFolder, workflowType = PopulationWorkflowTypes$parallelComparison)
  setwd(workflowFolder)
  pwf$simulate$settings$showProgress <- FALSE
  pwf$simulate$activate()
  pwf$calculatePKParameters$activate()
  pwf$calculateSensitivity$activate()
  pwf$calculateSensitivity$settings$showProgress <- TRUE
  pwf$calculateSensitivity$settings$variableParameterPaths <- "Organism|Heart|Volume"
  pwf$calculateSensitivity$settings$quantileVec <- c(0.25, 0.75)
  pwf$plotSensitivity$activate()
  pwf$runWorkflow()
}

runscript()
