rm(list = ls())
library(ospsuite)
library(stringr)
graphics.off()
#library(ospsuite.reportingengine)
load("./tests/dev/ex_03_pop/simTrees.Rdata")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")

pause <- function(seconds = NULL,show = TRUE) {
  if (is.null(seconds)){
    msg = NULL
    if (show){
      msg = "Press [enter] to continue."
    }
    line <- readline(prompt=msg)
  } else {
    if (show){
      disp("Pausing for ",seconds," seconds.")
    }
    Sys.sleep(seconds)
  }
}

simulationFile <- "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile1 <- "./tests/dev/ex_03_pop/RalPop10.csv"
populationFile2 <- "./tests/dev/ex_03_pop/LarPop10.csv"

ps1 <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile1,
  outputs = c(
    Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$Lung$Interstitial$Raltegravir$Concentration$path)
  )
)

ps2 <- PopulationSimulationSet$new(
  simulationSetName = "lar",
  simulationFile = simulationFile,
  populationFile = populationFile2,
  outputs = c(
    Output$new(path = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path),
    Output$new(path = simTree1$Organism$ArterialBlood$Plasma$Raltegravir$Concentration$path)
  )
)

pwf <- PopulationWorkflow$new(simulationSets = list(ps1, ps2), workflowFolder = "./tests/dev/ex_03_pop3")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$inactivate()
pwf$populationPKParameters$inactivate()
pwf$populationSensitivityAnalysis$inactivate()
pwf$plotSensitivity$activate()



pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- c(simTree1$Organism$Heart$Volume$path,
                                                                       simTree1$Organism$Lung$Volume$path,
                                                                       simTree1$Organism$Kidney$Volume$path,
                                                                       "Organism|Muscle|Peripheral blood flow fraction",
                                                                       "Organism|Muscle|Volume",
                                                                       "Organism|LargeIntestine|Volume",
                                                                       "Organism|PortalVein|Volume",
                                                                       "Organism|Spleen|Volume",
                                                                       "Organism|Skin|Volume",
                                                                       "Organism|Pancreas|Volume",
                                                                       "Organism|SmallIntestine|Mucosa|UpperIleum|Fraction mucosa",
                                                                       "Organism|SmallIntestine|Intracellular|CYP3A4|Relative expression (normalized)",
                                                                       "Organism|Lumen|Effective surface area variability factor",
                                                                       "Organism|Bone|Volume",
                                                                       "Organism|Acidic phospholipids (blood cells) [mg/g] - RR")
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$plotSensitivity$settings <-  SensitivityPlotSettings$new(totalSensitivityThreshold = 1) #pwf$populationSensitivityAnalysis$settings # or use <- SensitivityPlotSettings$new(totalSensitivityThreshold = 0.8)

pwf$runWorkflow()
