rm(list = ls())
library(ospsuite)
library(tlf)
library(ospsuite.reportingengine)

setwd("./tests/dev/01_MeanModel_Raltegravir")

simFiles <- paste0("PKML/", list.files("PKML"))
Raltegravir_PKFile <- "Raltegravir_PK.csv"
tpDictionaryFile <- "tpDictionary.csv"

dataFilters <- list(
  "10mg_", "25mg", "50mg", "100mg", "200mg", "400mg", "800mg", "1200mg", "1600mg",
  "100mg_FCT_MD", "200mg_FCT_MD", "400mg_FCT", "400mg_FCT_MD", "Chewable_tablet_fasted", "Chewable_tablet_fed", "granules_suspension"
)
names(dataFilters) <- simFiles

# Shorten the process by limiting to only 2 simulation sets
simFiles <- simFiles[1:2]

# Initilize list of mean model sets
meanModelSets <- list()
for (simFile in simFiles) {
  modelSet <- SimulationSet$new(
    simulationFile = simFile,
    pathID = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    pathName = "Raltegravir",
    pathUnit = "µg/l",
    pkParameters = c("C_max", "C_max_norm", "t_max", "C_tEnd", "AUC", "AUC_norm", "AUC_inf", "AUC_inf_norm", "MRT", "Thalf", "CL", "Vss", "Vd"),
    pkParametersUnits = c("µg/l", "kg/l", "h", "µg/l", "µg*h/l", "kg*h/l", "µg*h/l", "kg*h/l", "h", "h", "ml/min/kg", "ml/kg", "ml/kg"),
    dataFilter = dataFilters[simFile],
    observedDataFile = Raltegravir_PKFile,
    observedMetaDataFile = tpDictionaryFile
  )

  meanModelSets[[modelSet$simulationSetName]] <- modelSet
}

# Initialize workflow structure
meanModel_Raltegravir <- MeanModelWorkflow$new(simulationSets = meanModelSets)

# Tasks to activate/inactivate (this can also be done using meanModel_Raltegravir$activateTasks() method)
activateWorkflowTasks(meanModel_Raltegravir, tasks = c("plotGoF", "plotAbsorption", "plotMassBalance"))
inactivateWorkflowTasks(meanModel_Raltegravir, tasks = c("meanModelPKParameters"))

# Run the mean model workflow
meanModel_Raltegravir$runWorkflow()
