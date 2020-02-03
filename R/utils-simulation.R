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
                               resultFileFolder) {
  sim <- loadSimulation(paste0(simFileFolder, simFileName),
                        addToCache = FALSE,
                        loadFromCache = FALSE
  )
  pop <- loadPopulation(paste0(popDataFileFolder, popDataFileName))
  res <- runSimulation(sim, population = pop)
  exportResultsToCSV(res, paste0(resultFileFolder, resultFileName))
}



#' @title updateSimulationIndividualParameters
#' @description Update individual parameters with parameters in individualParameters,
#' individualParameters is obtained from a population object's getParameterValuesForIndividual() function.
#' @export
#' @import ospsuite
updateSimulationIndividualParameters <- function(simulation,individualParameters = NULL){
  if(!is.null(individualParameters)){
    sapply(1:length(individualParameters$paths),
           function(n,sim,par){ospsuite::setSimulationParameterValues(parameterPaths = par$paths[n],
                                                                                      values = par$values[n],
                                                                                      simulation = sim)},
           simulation,
           individualParameters)
  }
}


