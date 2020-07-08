rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)

simulationFile <- "./tests/dev/ex_03_pop9/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop9/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop9/LarPop10.csv"


pkC_max_norm = PkParameterInfo$new("C_max_norm")

ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(
    Output$new(path = "Organism|ArterialBlood|Plasma|Raltegravir|Concentration",displayName = "op2",pkParameters = "C_max_norm")
  )
)


pwf <- PopulationWorkflow$new(simulationSets = list(ps1), workflowFolder = "./tests/dev/testSAPK")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
pwf$plotSensitivity$activate()

pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c("Organism|Heart|Volume","Organism|Lung|Volume")

pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.25,0.75)

pwf$plotSensitivity$settings <- SensitivityPlotSettings$new(totalSensitivityThreshold = 1, maximalParametersPerSensitivityPlot = 12)

pwf$runWorkflow()
