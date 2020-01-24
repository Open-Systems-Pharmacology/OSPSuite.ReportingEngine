library("Rmpi")
library(ospsuite)



if (!(mpi.comm.size() == 0)) {
  mpi.close.Rslaves()
}

theNumberOfSlaves <- 5
theFolderName <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/"
theFileName <- "popData"
theNumberOfCommentLines <- 2

# start 2 R workers (slaves) instances (once per WORKFLOW or once per Task?)
mpi.spawn.Rslaves(nslaves = theNumberOfSlaves)

library("ospsuite")
library("ospsuite.reportingengine")

# tempPopDataFiles <- splitPopDataFile(fileName = theFileName,
#                                      folderName = theFolderName,
#                                      numberOfSlaves = theNumberOfSlaves,
#                                      numberOfCommentLines = theNumberOfCommentLines)


tempPopDataFiles <- ospsuite::splitPopulationFile(
  csvPopulationFile = paste0(theFolderName, theFileName, ".csv"),
  numberOfCores = theNumberOfSlaves,
  outputFolder = theFolderName,
  outputFileName = theFileName
)





# load ospsuite and ospsuite.reportingengine libs on the slaves
mpi.bcast.cmd(library("ospsuite"))
mpi.bcast.cmd(library("ospsuite.reportingengine"))


runpop <- function(popDataFileName, popDataFolderName) {
  popDataFilePath <- paste0(popDataFolderName, popDataFileName)
  simfile <- c("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml")
  sim <- loadSimulation(simfile, addToCache = FALSE, loadFromCache = FALSE)
  LL <- getEnum(simulationFilePath = simfile)
  print(popDataFilePath)

  popsim.OutputList <- c(LL$Organism$blockA$mol1$Concentration$path)
  op <- getAllQuantitiesMatching(paths = popsim.OutputList, container = sim)
  pop <- loadPopulation(popDataFilePath)
  addOutputs(op, simulation = sim)

  res <- runSimulation(sim, population = pop)
  print(res$count)

  exportResultsToCSV(res, paste0(popDataFolderName, "results", mpi.comm.rank(), ".csv"))
}

mpi.bcast.Robj2slave(obj = runpop)
mpi.bcast.Robj2slave(obj = tempPopDataFiles)
mpi.bcast.Robj2slave(obj = theFolderName)

mpi.remote.exec(runpop(popDataFileName = paste0(tempPopDataFiles[mpi.comm.rank()]), popDataFolderName = theFolderName)) # mpi.remote.exec and not mpi.bcast.cmd so we ensure that this process has finished before next process begins

# removeTempPopFiles(folderName = theFolderName, fileNamesVec= tempPopDataFiles)

# resultsList <- list()
# for (n in 1:theNumberOfSlaves){
#   resultsList[[n]] <- read.csv(file = paste0(theFolderName,"results",n,".csv"),check.names=FALSE, encoding="UTF-8")
# }

# resultsDf <- do.call("rbind", resultsList)
# write.csv(x = resultsDf,file = paste0(theFolderName,"results.csv"),row.names = FALSE)

# for (n in 1:theNumberOfSlaves){
#   file.remove(paste0(theFolderName,"results",n,".csv"))
# }

mpi.close.Rslaves()
