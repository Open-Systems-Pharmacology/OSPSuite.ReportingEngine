#' @title simulatePopulationModel
#' @description Simulate model for a population.  Run parallel workflow if numberOfCores > 1.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param taskSettings settings for the
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulatePopulationModel <- function(structureSet,
                                    settings,
                                    logFolder = getwd()) {

  if (settings$numberOfCores == 1) {
    #logInfo(message = "Starting population simulation")
    #createFolder(set$simulationResultsFolder)
    #resultsFilePath <- file.path(set$simulationResultsFolder, defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName))
    simulateModel(structureSet = structureSet,
                  settings = settings,
                  logFolder = logFolder)
  }
  else if (settings$numberOfCores > 1) {
    logWorkflow(
      message = "Starting parallel population simulation",
      pathFolder = logFolder
    )
    #createFolder(set$simulationResultsFolder)
    set$simulationResultFileNames <- runParallelPopulationSimulation(structureSet = structureSet,
                                                                   settings = settings,
                                                                   logFolder = logFolder)
      # numberOfCores = self$simulatePopulation$numberOfCores,
      # inputFolderName = set$inputFilesFolder,
      # simulationFileName = set$simulationSet$simulationName,
      # populationFileName = set$simulationSet$populationName,
      # resultsFolderName = set$simulationResultsFolder,
      # resultsFileName = trimFileName(defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName), extension = "csv"))
    logWorkflow(message = "Parallel population simulation completed.",
                pathFolder = logFolder
    )
  }

}



#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param logFolder folder where the logs are saved
#' @param nodeName node name for parallel simulations
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulateModel <- function(structureSet,
                          settings,
                          logFolder = getwd(),
                          nodeName = NULL) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile,
                                         addToCache = FALSE,
                                         loadFromCache = FALSE)
  logWorkflow(
    message = paste0(
      ifnotnull(nodeName, paste0(nodeName, ": "), ""),
      "Simulation file '",
      structureSet$simulationSet$simulationFile,
      "' successfully loaded"
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  population <- NULL
  if (!is.null(structureSet$simulationSet$populationFile)) {
    population <- ospsuite::loadPopulation(structureSet$simulationSet$populationFile)
    logWorkflow(
      message = paste0(
        ifnotnull(nodeName, paste0(nodeName, ": "), ""),
        "Population file '",
        structureSet$simulationSet$populationFile,
        "' successfully loaded"
      ),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = settings$showProgress)
  simulationResult <- ospsuite::runSimulation(simulation, population = population, simulationRunOptions = simRunOptions)

  logWorkflow(
    message = paste0(
      ifnotnull(nodeName, paste0(nodeName, ": "), ""),
      "Simulation run complete"
    ),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  return(simulationResult)
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
runParallelPopulationSimulation <- function(structureSet = structureSet,
                                            settings = settings,
                                            logFolder = logFolder) {
                                            # numberOfCores,
                                            # inputFolderName,
                                            # simulationFileName,
                                            # populationFileName,
                                            # resultsFolderName,
                                            # resultsFileName


  populationFileName <- structureSet$simulationSet$populationFile
  population <- ospsuite::loadPopulation(populationFileName)

  numberOfIndividuals <- length(population$allIndividualIds)
  numberOfCores <- min(settings$numberOfCores, numberOfIndividuals)

  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  Rmpi::mpi.remote.exec(library("ospsuite"))
  Rmpi::mpi.remote.exec(library("ospsuite.reportingengine"))
  tempPopDataFiles <- splitPopulationFile(
    csvPopulationFile = structureSet$simulationSet$populationFile,
    numberOfCores = numberOfCores,
    outputFolder =  paste0(structureSet$simulationResultsFolder,"/") ,
    outputFileName = trimFileName(structureSet$simulationSet$populationFile,"csv")
  )
  tempLogFileNamePrefix <- file.path(defaultFileNames$workflowFolderPath(), "logDebug-core-simulation")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores))
  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores,
                                                 folderName = structureSet$simulationResultsFolder,
                                                 fileName = structureSet$simulationResultFileNames)

  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
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
  if (is.null(individualParameters)) {
    return()
  }
  ospsuite::setParameterValuesByPath(parameterPaths = individualParameters$paths, values = individualParameters$values, simulation = simulation)
}

#' @title defaultSimulationNumberOfCores
#' @description default numberOfCores for simulation
#' @export
defaultSimulationNumberOfCores <- 1
