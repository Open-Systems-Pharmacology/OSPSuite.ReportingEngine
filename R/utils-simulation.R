#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulateModel <- function(simFilePath,
                          popDataFilePath=NULL,
                          resultsFilePath,
                          calculatePKParameters=FALSE,
                          PKParametersFilePath=NULL) {
  sim <- loadSimulation(simFilePath,
                        addToCache = FALSE,
                        loadFromCache = FALSE)
  pop <- NULL
  if(!is.null(popDataFilePath)){
    pop <- loadPopulation(popDataFilePath)
  }
  res <- runSimulation(sim, population = pop)
  exportResultsToCSV(res, resultsFilePath)
  if(calculatePKParameters){
    pkAnalyses <- calculatePKAnalyses(results = res)
    exportPKAnalysesToCSV(pkAnalyses = pkAnalyses,filePath = PKParametersFilePath)
  }
}



#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation on cores, save results as CSV.
#' @return Simulation results for population
#' @export
#' @import ospsuite
#' @import Rmpi
runParallelPopulationSimulation <- function(numberOfCores,
                                            workingDirectory,
                                            inputFolder,
                                            outputFolder,
                                            simFileName,
                                            popFileName,
                                            resultsFileName) {
  #library("Rmpi")
  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  Rmpi::mpi.bcast.cmd(library("ospsuite"))
  Rmpi::mpi.bcast.cmd(library("ospsuite.reportingengine"))
  tempPopDataFiles <- splitPopulationFile(
    csvPopulationFile = paste0(workingDirectory, "/", inputFolder, "/", popFileName, ".csv"),
    numberOfCores = numberOfCores,
    outputFolder = paste0(inputFolder, "/"),
    outputFileName = popFileName
  )
  Rmpi::mpi.bcast.Robj2slave(obj = simFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = popFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  Rmpi::mpi.bcast.Robj2slave(obj = workingDirectory)
  Rmpi::mpi.bcast.Robj2slave(obj = inputFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = outputFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = resultsFileName)

  Rmpi::mpi.remote.exec(simulateModel(
    simFilePath = paste0(workingDirectory, "/", inputFolder, "/", simFileName, ".pkml"),
    popDataFilePath = paste0(workingDirectory, "/", inputFolder, "/", popFileName, "_", mpi.comm.rank(), ".csv"),
    resultsFilePath = paste0(workingDirectory, "/", outputFolder, "/", resultsFileName, "_", mpi.comm.rank(), ".csv")
  ))
  Rmpi::mpi.close.Rslaves() # Move to end of workflow
}


#' @title updateSimulationIndividualParameters
#' @description Update individual parameters with parameters in individualParameters,
#' individualParameters is obtained from a population object's getParameterValuesForIndividual() function.
#' @export
#' @import ospsuite
updateSimulationIndividualParameters <- function(simulation, individualParameters = NULL) {
  if (!is.null(individualParameters)) {
    sapply(
      1:length(individualParameters$paths),
      function(n, sim, par) {
        ospsuite::setSimulationParameterValues(
          parameterPaths = par$paths[n],
          values = par$values[n],
          simulation = sim
        )
      },
      simulation,
      individualParameters
    )
  }
}
