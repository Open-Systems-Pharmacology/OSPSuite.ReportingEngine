rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)

simulationFile <- "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop/LarPop10.csv"


ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
     Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path,
                pkParameters = c("t_max","AUC_tEnd")),
    Output$new(path = simTree1$Organism$Lung$Interstitial$Raltegravir$Concentration$path,
               pkParameters = c("C_max","MRT"))
  )
)

ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile,
  populationFile = populationFile2,
  outputs = c(Output$new(path = simTree1$Organism$Lung$Interstitial$Raltegravir$Concentration$path,
              pkParameters = c("C_max","MRT"))
  )
)


pwf <- PopulationWorkflow$new(simulationSets = list(ps1, ps2), workflowFolder = "./tests/dev/ex_03_pop9")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$activate()
pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$plotSensitivity$activate()



pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c(
  "Organism|Heart|Volume",
  "Organism|Lung|Volume",
  "Organism|Kidney|Volume",
  "Organism|Brain|Volume",
  "Organism|Muscle|Volume",
  "Organism|LargeIntestine|Volume",
  "Organism|PortalVein|Volume",
  "Organism|Spleen|Volume",
  "Organism|Skin|Volume",
  "Organism|Pancreas|Volume",
  "Organism|SmallIntestine|Mucosa|UpperIleum|Fraction mucosa",
  "Organism|SmallIntestine|Volume",
  "Organism|Lumen|Effective surface area variability factor",
  "Organism|Bone|Volume",
  "Organism|Stomach|Volume")

pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.25,0.5,0.75)

pwf$plotSensitivity$settings <- SensitivityPlotSettings$new(totalSensitivityThreshold = 0.9, maximalParametersPerSensitivityPlot = 12, plotFontSize = 6)

pwf$runWorkflow()



