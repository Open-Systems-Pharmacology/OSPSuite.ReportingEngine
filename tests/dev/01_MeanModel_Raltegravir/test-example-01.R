rm(list = ls())
library(ospsuite.reportingengine)

simFiles <- c("PKML/Raltegravir 10 mg   (lactose formulation).pkml",
              "PKML/Raltegravir 50 mg   (lactose formulation).pkml")

Raltegravir_PKFile <- "Raltegravir_PK.csv"
tpDictionaryFile <- "tpDictionary.csv"

dataFilters <- c('Grouping %in% "10mg_"', 
                 'Grouping %in% "50mg"')

# Definition of mean model sets
meanModelSets <- list()
for (simIndex in seq_along(simFiles)) {
  modelSet <- SimulationSet$new(
    simulationFile = simFile[simIndex],
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathName = "Raltegravir",
    pathUnit = "µg/l",
    dataFilter = dataFilters[simFile],
    observedDataFile = Raltegravir_PKFile,
    observedMetaDataFile = tpDictionaryFile
  )
  meanModelSets[[simIndex]] <- modelSet
}

# Definition of workflow structure and tasks
workflow <- MeanModelWorkflow$new(simulationSets = meanModelSets)

workflow$activateTasks(tasks = c("plotGoF", "plotAbsorption", "plotMassBalance"))
workflow$inactivateTasks(tasks = c("meanModelPKParameters"))

# Run the mean model workflow
workflow$runWorkflow()
