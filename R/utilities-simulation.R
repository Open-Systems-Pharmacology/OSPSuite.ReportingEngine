#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population.  Calculate and save PK parameters as an option.
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulateModel <- function(simFilePath,
                          popDataFilePath = NULL,
                          resultsFilePath) {
  sim <- loadSimulation(simFilePath,
    addToCache = FALSE,
    loadFromCache = FALSE
  )
  pop <- NULL
  if (!is.null(popDataFilePath)) {
    pop <- loadPopulation(popDataFilePath)
  }
  res <- runSimulation(sim, population = pop)
  exportResultsToCSV(res, resultsFilePath)
  return(resultsFilePath)
}



#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation on cores, save results as CSV.
#' @return Simulation results for population
#' @export
#' @import ospsuite
## #' @import Rmpi
runParallelPopulationSimulation <- function(numberOfCores,
                                            inputFolderName,
                                            simulationFileName,
                                            populationFileName,
                                            resultsFolderName,
                                            resultsFileName) {

  # library("Rmpi")
  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  Rmpi::mpi.bcast.cmd(library("ospsuite"))
  Rmpi::mpi.bcast.cmd(library("ospsuite.reportingengine"))
  tempPopDataFiles <- splitPopulationFile(
    csvPopulationFile = file.path(inputFolderName, paste0(populationFileName, ".csv")),
    numberOfCores = numberOfCores,
    outputFolder = paste0(inputFolderName, "/"), #### USE file.path()?  Do we need "/"?
    outputFileName = populationFileName
  )

  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores, folderName = resultsFolderName, fileName = resultsFileName)

  Rmpi::mpi.bcast.Robj2slave(obj = simulationFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = populationFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  Rmpi::mpi.bcast.Robj2slave(obj = inputFolderName)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)


  Rmpi::mpi.remote.exec(simulateModel(
    simFilePath = file.path(inputFolderName, paste0(simulationFileName, ".pkml")),
    popDataFilePath = file.path(inputFolderName, paste0(populationFileName, "_", mpi.comm.rank(), ".csv")),
    resultsFilePath = allResultsFileNames[mpi.comm.rank()]
  ))
  Rmpi::mpi.close.Rslaves() # Move to end of workflow

  return(allResultsFileNames)
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
