# Helper for test copied from ospsuite
getTestDataFilePath <- function(fileName) {
  dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not want to load from cache by default. Instead
# new instances should be created unless specifically specified otherwise
loadTestSimulation <- function(simulationName, loadFromCache = FALSE, addToCache = TRUE) {
  simFile <- getSimulationFilePath(simulationName)
  simulation <- loadSimulation(simFile, loadFromCache = loadFromCache, addToCache = addToCache)
  return(simulation)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

comparisonTolerance <- function(tolerance = 1e-4) {
  tolerance
}
