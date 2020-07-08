rm(list = ls())
library(ospsuite)
graphics.off()
library(ospsuite.reportingengine)

simulationFile <- "./tests/dev/ex_03_pop9/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop9/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop9/LarPop10.csv"


ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(
    Output$new(path = "Organism|VenousBlood|Plasma|Raltegravir|Concentration",displayName = "op1"),
    Output$new(
      path = "Organism|ArterialBlood|Plasma|Raltegravir|Concentration",displayName = "op2",
      pkParameters = c(PkParameterInfo$new("t_max","new_t_max"),
                       PkParameterInfo$new("AUC_tEnd","new_AUC_tEnd"))
    ),
    Output$new(
      path = "Organism|Lung|Interstitial|Raltegravir|Concentration",displayName = "op3",
      pkParameters = c(PkParameterInfo$new("C_max","new_C_max"),
                       PkParameterInfo$new("MRT","new_MRT"))
    )
  )
)




ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile,
  populationFile = populationFile2,
  outputs = c(Output$new(
    path = "Organism|Lung|Interstitial|Raltegravir|Concentration",displayName = "op4",
    pkParameters = c(PkParameterInfo$new("C_max","kai_C_max"),
                     PkParameterInfo$new("MRT","kai_MRT")))
  )
)


pwf <- PopulationWorkflow$new(simulationSets = list(ps1, ps2), workflowFolder = "./tests/dev/ex_03_pop9")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
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
  "Organism|Stomach|Volume"
)

pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.25, 0.5, 0.75)

pwf$plotSensitivity$settings <- SensitivityPlotSettings$new(totalSensitivityThreshold = 0.9,
                                                            maximalParametersPerSensitivityPlot = 12,
                                                            xAxisFontSize = 10,
                                                            yAxisFontSize = 6)

pwf$runWorkflow()
