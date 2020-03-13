#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population.  Calculate and save PK parameters as an option.
#' @param simFilePath path to pkml model file
#' @param popDataFilePath path to the population data file
#' @param resultsFilePath path to simulation results CSV files
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulateModel <- function(simFilePath,
                          popDataFilePath = NULL,
                          resultsFilePath,
                          debugLogFileName = file.path(defaultFileNames$workflowFolderPath(),defaultFileNames$logDebugFile()),
                          infoLogFileName = file.path(defaultFileNames$workflowFolderPath(),defaultFileNames$logInfoFile()),
                          errorLogFileName = file.path(defaultFileNames$workflowFolderPath(),defaultFileNames$logErrorFile()),
                          nodeName = NULL,
                          showProgress = FALSE) {

  sim <- loadSimulation(simFilePath,
    addToCache = FALSE,
    loadFromCache = FALSE
  )
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Simulation loaded"), file = debugLogFileName, printConsole = FALSE)
  pop <- NULL
  if (!is.null(popDataFilePath)) {
    pop <- loadPopulation(popDataFilePath)
  }
  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = showProgress)
  res <- runSimulation(sim, population = pop,simulationRunOptions = simRunOptions)
  logDebug(message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), NULL), "Simulation run complete"), file = debugLogFileName, printConsole = FALSE)
  exportResultsToCSV(res, resultsFilePath)
  return(resultsFilePath)
}



#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation on cores, save results as CSV.
#' @param numberOfCores number of cores over which to parallelize the population simulation
#' @param inputFolderName path to folder storing pkml and population data files for the simulation
#' @param simulationFileName name of pkml model file
#' @param populationFileName name of population data CSV file
#' @param resultsFileFolder path to population simulation results CSV files
#' @param resultsFileName root name of population simulation results CSV files
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

population <- loadPopulation(file.path(inputFolderName, paste0(populationFileName, ".csv")))
numberOfIndividuals <- length(population$allIndividualIds)
numberOfCores <- min(numberOfCores,numberOfIndividuals)
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
  tempLogFileNamePrefix <- file.path(defaultFileNames$workflowFolderPath(),"logDebug-core-simulation")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores))
  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores, folderName = resultsFolderName, fileName = resultsFileName)

  Rmpi::mpi.bcast.Robj2slave(obj = simulationFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = populationFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNamePrefix)
  Rmpi::mpi.bcast.Robj2slave(obj = inputFolderName)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  Rmpi::mpi.remote.exec(simulateModel(
    simFilePath = file.path(inputFolderName, paste0(simulationFileName, ".pkml")),
    popDataFilePath = file.path(inputFolderName, paste0(populationFileName, "_", mpi.comm.rank(), ".csv")),
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    debugLogFileName = paste0(tempLogFileNamePrefix, mpi.comm.rank()),
    nodeName = paste("Core", mpi.comm.rank())
  ))
  Rmpi::mpi.close.Rslaves() # Move to end of workflow

  for (core in seq(1, numberOfCores)) {
    logDebug(message = readLines(tempLogFileNames[core]))
    file.remove(tempLogFileNames[core])
  }

  return(allResultsFileNames)
}


#' @title updateSimulationIndividualParameters
#' @description Update simulation with parameters from individualParameters
#' @param simulation is the simulation object to be updated
#' @param individualParameters is an object storing an individual's parameters, obtained from a population object's getParameterValuesForIndividual() function.
#' @export
#' @import ospsuite
updateSimulationIndividualParameters <- function(simulation, individualParameters = NULL) {
  if (!is.null(individualParameters)) {
    sapply(
      1:length(individualParameters$paths),
      function(n, sim, par) {
        ospsuite::setParameterValuesByPath(
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
