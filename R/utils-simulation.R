#' @title simulatePopulation
#' @description Simulate model for a given population
#' @return Simulation results for population
#' @export
#' @import ospsuite
simulatePopulation <- function(simFileName,
                               simFileFolder,
                               popDataFileName,
                               popDataFileFolder,
                               resultFileName,
                               resultFileFolder){
  sim <- loadSimulation(paste0(simFileFolder,simFileName),
                        addToCache = FALSE,
                        loadFromCache = FALSE)
  pop <- loadPopulation(paste0(popDataFileFolder,popDataFileName))
  res<-runSimulation(sim,population = pop)
  exportResultsToCSV(res, paste0(resultFileFolder,resultFileName))
}
