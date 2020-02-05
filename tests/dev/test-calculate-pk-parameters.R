# rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)

devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")


# #MULTIPLE CORE
# setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
# pwf <- PopulationWorkflow$new(simulationFile = simfile,
#                               populationFile = popfile)
# pwf$setPopulationSimulationSettings(numberOfCores = 4)
# res<-pwf$runWorkflow()

fldr <- pwf$populationSimulation$resultsFolderName
lst <- list.files(path = pwf$populationSimulation$resultsFolderName)
clst <- paste(fldr, lst, sep = "/")

impres <- importResultsFromCSV(simulation = loadSimulation(simfile), filePaths = clst)
