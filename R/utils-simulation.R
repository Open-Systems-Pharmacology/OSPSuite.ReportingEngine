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



#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation and save it in file resultFileName.
#' @return Simulation results for population
#' @export
#' @import ospsuite
runParallelPopulationSimulation<-function(numberOfCores,
                                          workingDirectory,
                                          inputFolder,
                                          outputFolder,
                                          simFileName,
                                          popFileName,
                                          resultFileName){

  library("Rmpi")
  mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  mpi.bcast.cmd(library("ospsuite"))
  mpi.bcast.cmd(library("ospsuite.reportingengine"))
  tempPopDataFiles <- ospsuite::splitPopulationFile(
    csvPopulationFile = self$populationSimulation$input$population,
    numberOfCores = numberOfCores,
    outputFolder = paste0(inputFolder, "/"),
    outputFileName = popFileName
  )
  mpi.bcast.Robj2slave(obj = simFileName)
  mpi.bcast.Robj2slave(obj = popFileName)
  mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  mpi.bcast.Robj2slave(obj = wdir)
  mpi.bcast.Robj2slave(obj = inputFolder)
  mpi.bcast.Robj2slave(obj = outputFolder)

  mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
    simFileName = paste0(simFileName, ".pkml"),
    simFileFolder = paste0(wdir, "/", inputFolder, "/"),
    popDataFileName = paste0(popFileName, "_", mpi.comm.rank(), ".csv"),
    popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
    resultFileName = paste0(resultsFileName, "_", mpi.comm.rank(), ".csv"),
    resultFileFolder = paste0(wdir, "/", outputFolder, "/")
  ))
  mpi.close.Rslaves() # Move to end of workflow
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


