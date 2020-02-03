#' @title simulatePopulation
#' @description Simulate model for a given population
#' @return Simulation results for population
#' @export
#' @import ospsuite
simulatePopulation <- function(simFilePath,
                               popDataFilePath,
                               resultsFilePath) {
  sim <- loadSimulation(simFilePath,
                        addToCache = FALSE,
                        loadFromCache = FALSE)
  pop <- loadPopulation(popDataFilePath)
  res <- runSimulation(sim, population = pop)
  exportResultsToCSV(res, resultsFilePath)
}



#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation on cores, save results as CSV.
#' @return Simulation results for population
#' @export
#' @import ospsuite
runParallelPopulationSimulation<-function(numberOfCores,
                                          workingDirectory,
                                          inputFolder,
                                          outputFolder,
                                          simFileName,
                                          popFileName,
                                          resultsFileName){
  library("Rmpi")
  mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  mpi.bcast.cmd(library("ospsuite"))
  mpi.bcast.cmd(library("ospsuite.reportingengine"))
  #print("Here")
  #mpi.bcast.cmd(print("Starting para"))
  tempPopDataFiles <- ospsuite::splitPopulationFile(
    csvPopulationFile = paste0(workingDirectory,"/",inputFolder,"/",popFileName,".csv"),
    numberOfCores = numberOfCores,
    outputFolder = paste0(inputFolder, "/"),
    outputFileName = popFileName
  )
  mpi.bcast.Robj2slave(obj = simFileName)
  mpi.bcast.Robj2slave(obj = popFileName)
  mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  mpi.bcast.Robj2slave(obj = workingDirectory)
  mpi.bcast.Robj2slave(obj = inputFolder)
  mpi.bcast.Robj2slave(obj = outputFolder)
  mpi.bcast.Robj2slave(obj = resultsFileName)
  #mpi.bcast.cmd(print("Here 2"))
  #mpi.bcast.cmd(print(paste0("hhh",workingDirectory,"/", outputFolder, "/",resultsFileName, "_", mpi.comm.rank(), ".csv")))

  mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
    simFilePath = paste0(workingDirectory,"/", inputFolder, "/",simFileName, ".pkml"),
    popDataFilePath = paste0(workingDirectory,"/", inputFolder, "/",popFileName, "_", mpi.comm.rank(), ".csv"),
    resultsFilePath = paste0(workingDirectory,"/", outputFolder, "/",resultsFileName, "_", mpi.comm.rank(), ".csv")
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


