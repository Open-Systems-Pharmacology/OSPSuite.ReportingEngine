rm(list = ls())
library(ospsuite)
library(tlf)
library(ospsuite.reportingengine)

setwd("C:/Design2Code/R-RE-tests/01_MeanModel_Raltegravir")

simFiles <- paste0("PKML/", list.files("PKML"))
Raltegravir_PKFile <- "Raltegravir_PK.csv"
tpDictionaryFile <- "tpDictionary.csv"

simFiles <- simFiles[1:2]
# Initilize list of mean model sets
meanModelSets <- list()
for (simFile in simFiles){
modelSet <-  MeanModelSet$new(simulationFile = simFile,
                                  pathID = 'Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)',
                                  pathName = 'Raltegravir',
                                  pathUnit = 'nmol/l',
                                  pkParameters = c('C_max','C_max_norm','t_max','C_tEnd','AUC','AUC_norm','AUC_inf','AUC_inf_norm','MRT','Thalf','CL','Vss','Vd'),
                                  pkParametersUnits = c('μg/l','kg/l','h','μg/l','μg*h/l','kg*h/l','μg*h/l','kg*h/l','h','h','ml/min/kg','ml/kg','ml/kg'),
                                  dataFilter = list(variableNames = "Grouping", values = "values"),
                                  observedDataFile = Raltegravir_PKFile,
                                  observedMetaDataFile = tpDictionaryFile)

meanModelSets[[modelSet$simulationSetName]] <- modelSet
}

# Initialize workflow structure
meanModel_Raltegravir <- MeanModelWorkflow$new(simulationSets = meanModelSets)

# Tasks to activate/inactivate
activateWorkflowTasks(meanModel_Raltegravir, tasks = c("plotGoF", "plotAbsorption", "plotMassBalance"))
inactivateWorkflowTasks(meanModel_Raltegravir, tasks = c("meanModelPKParameters"))

# Define options specific to the tasks: e.g. plot configuration,
# selected compounds here
meanModel_Raltegravir$plotMassBalance$settings$compoundNames <- c("Raltegravir", "Raltegravir-UGT1A1-Kassahun 2007 Metabolite", "Raltegravir-UGT1A9-Kassahun 2007 Metabolite")

# Run the mean model workflow
meanModel_Raltegravir$runWorkflow()

