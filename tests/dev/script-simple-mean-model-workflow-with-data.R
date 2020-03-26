rm(list = ls())
library(ospsuite.reportingengine)

# Set working folder
setwd("C:/Design2Code/R-RE-tests/01_MeanModel_Raltegravir")

simFile <- "PKML/Raltegravir 50 mg  (lactose formulation).pkml"
dataFile <- "Raltegravir_PK.csv"
dataDictionary <- "tpDictionary.csv"
dataFilter <- 'Grouping %in% "50mg"'

modelSet <- SimulationSet$new(simulationFile = simFile,
                              pathID = 'Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)',
                              pathName = 'Raltegravir',
                              pathUnit = 'mg/l',
                              dataFilter = dataFilter,
                              observedDataFile = dataFile,
                              observedMetaDataFile = dataDictionary)

meanModel_Raltegravir <- MeanModelWorkflow$new(simulationSets = modelSet)
