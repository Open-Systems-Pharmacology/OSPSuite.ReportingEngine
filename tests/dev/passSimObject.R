library(ospsuite)
library(ospsuite.reportingengine)


receiverFunc <- function(simObject) {
  print(simObject$outputSelections)
}

simFile <- "./data/simpleMobiEventSim.pkml"
sim <- loadSimulation(simFile)
receiverFunc(sim)
print(sim$outputSelections)
