rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)
#devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simulationFile1 <- "./tests/dev/popdiffsens/RaltegravirSim.pkml"
simulationFile2 <- "./tests/dev/popdiffsens/LarsonSim.pkml"
populationFile1 <- "./tests/dev/popdiffsens/RalPop10.csv"
populationFile2 <- "./tests/dev/popdiffsens/LarPop10.csv"


ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile1,
  populationFile = populationFile1,
  outputs = c(
    Output$new(
      path = "Organism|Lung|Interstitial|Raltegravir|Concentration",
      pkParameters = c("C_max")
    )
  )
)

ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile2,
  populationFile = populationFile2,
  outputs = c(Output$new(
    path = "Organism|Lung|Interstitial|Raltegravir|Concentration",
    pkParameters = c("C_max")
  ))
)



pwf <- PopulationWorkflow$new(simulationSets = list(ps1,ps2), workflowFolder = "./tests/dev/popdiffsens")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$activate()
pwf$plotSensitivity$activate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c("Organism|Stomach|Volume",
                                                                       "Applications|Iwamoto 2008 400mg PO (Figure 1) omeprazole study|filmcoated tablet (original Merck formulation)|Application_1|ProtocolSchemaItem|Dose")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.5)
pwf$plotSensitivity$settings <- SensitivityPlotSettings$new(totalSensitivityThreshold = 1, maximalParametersPerSensitivityPlot = 12, plotFontSize = 6)

pwf$runWorkflow()
