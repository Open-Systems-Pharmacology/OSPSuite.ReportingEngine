rm(list = ls())
library(ospsuite.reportingengine)

# Set working folder
setwd("C:/Design2Code/R-RE-tests/01_MeanModel_Raltegravir")

simFile <- "PKML/Raltegravir 50 mg  (lactose formulation).pkml"

modelSet <- SimulationSet$new(simulationFile = simFile,
                              pathID = 'Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)',
                              pathName = 'Raltegravir',
                              pathUnit = 'mg/l')
  
workflow <- MeanModelWorkflow$new(simulationSets = modelSet,
                                  workflowFolder = "myFolder")