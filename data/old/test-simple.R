# rm(list = ls())

library("ospsuite.reportingengine")
library("ospsuite")
# library("tlf")
# source("./R/get-enum.R")

pkmlFilePath <- "./data/simpleMobiEventSim.pkml"
sim <- loadSimulation(pkmlFilePath)
pathEnumList <- getEnum(pkmlFilePath)
addOutputs(quantitiesOrPaths = pathEnumList$Organism$blockA$mol1$Concentration$path, simulation = sim)
res <- runSimulation(sim)
res$getValuesByPath(path = pathEnumList$Organism$blockA$mol1$Concentration$path, individualIds = 0)
# ospsuite::exportResultsToCSV( results = res , filePath = "C:/Users/ahamadeh/Dropbox/rproject/workflow/exp1.csv")
# res$getValuesByPath(path = res$allQuantityPaths[1] , individualIds = 0)  #



# library(ospsuite)
# library(tlf)
#
# source("./get-enum.R")
#
# pkmlFilePath <- "individualPksimSim.pkml"
#
# pkmlFilePath <- "simpleMobiEventSim.pkml"
#
# pathEnumList <- getEnum(pkmlFilePath)
#
# indSim <- loadSimulation(pkmlFilePath)
# population <- loadPopulation(csvPopulationFile = "popData.csv")
# popRes <- runSimulation(simulation = indSim , population = population)
# allResultsPopTLF <- getOutputValuesTLF(simulationResults = popRes,
#                                        population = population,
#                                        quantitiesOrPaths = popRes$allQuantityPaths[c(1,2)],
#                                        individualIds = seq(0,99))
# write.csv(allResultsPopTLF$data , file = "popsimdata.csv")
#
#
#
#
# allCon  <-ospsuite::getAllContainersMatching(path = "*" , container = indSim)
# allCon1 <-ospsuite::getAllContainersMatching(path = "*" , container = allCon[[1]])
