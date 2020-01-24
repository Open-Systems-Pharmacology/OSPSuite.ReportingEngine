rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)





# simFileName <- "individualPksimSim.pkml"
simFileName <- "simpleMobiEventSim.pkml"
simFileFolder <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/"

sim <- loadSimulation(paste0(simFileFolder, simFileName))
# sim<-loadSimulation("./data/simpleMobiEventSim.pkml")

print("Getting param list...")
allParameters <- ospsuite::getAllParametersMatching(paths = "**", container = sim)
totalNumberParameters <- length(allParameters)
print("...done")

numberOfCores <- 1
numParametersPerCore <- ceiling(totalNumberParameters / numberOfCores)

paramIndices <- list()
if (numberOfCores > 1) {
  for (n in 1:(numberOfCores - 1)) {
    paramIndices[[n]] <- c(((n - 1) * numParametersPerCore) + 1, (n * numParametersPerCore))
  }
  paramIndices[[numberOfCores]] <- c((((numberOfCores - 1)) * numParametersPerCore) + 1, totalNumberParameters)
  mpi.spawn.Rslaves(nslaves = numberOfCores)
  mpi.bcast.cmd(library("ospsuite"))
  mpi.bcast.cmd(library("ospsuite.reportingengine"))
  mpi.bcast.Robj2slave(obj = simFileName)
  mpi.bcast.Robj2slave(obj = simFileFolder)
  mpi.bcast.Robj2slave(obj = paramIndices)
  mpi.bcast.cmd(ospsuite.reportingengine::analyzeSensitivity(simFileName, simFileFolder, paramIndices[[mpi.comm.rank()]]))
  mpi.close.Rslaves()
} else {
  # paramIndices[[1]] <- c(1,totalNumberParameters)
  resa <- ospsuite.reportingengine::analyzeSensitivity(simFileName, simFileFolder)
}
