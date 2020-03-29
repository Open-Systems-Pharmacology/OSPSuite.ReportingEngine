#' @title simulateModelForPopulation
#' @description Simulate model for a population.  Run parallel workflow if numberOfCores > 1 AND population size is >1.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @return Simulation results for individual or population
#' @export
#' @import ospsuite
simulateModelForPopulation <- function(structureSet,
                                       settings = NULL,
                                       logFolder = getwd()) {
  population <- ospsuite::loadPopulation(structureSet$simulationSet$populationFile)
  numberOfIndividuals <- length(population$allIndividualIds)
  numberOfCores <- min(ifnotnull(inputToCheck = settings, outputIfNotNull = settings$numberOfCores, outputIfNull = 1), numberOfIndividuals)

  if (numberOfCores == 1) {
    simulationResult <- simulateModel(
      structureSet = structureSet,
      settings = settings,
      logFolder = logFolder
    )
  }
  else if (numberOfCores > 1) {
    logWorkflow(
      message = "Starting parallel population simulation",
      pathFolder = logFolder
    )

    simulationResultFileNames <- runParallelPopulationSimulation(
      numberOfCores = numberOfCores,
      structureSet = structureSet,
      settings = settings,
      logFolder = logFolder
    )

    simulationResult <- ospsuite::importResultsFromCSV(
      simulation = ospsuite::loadSimulation(structureSet$simulationSet$simulationFile),
      filePaths = simulationResultFileNames
    )
    file.remove(simulationResultFileNames)

    logWorkflow(
      message = "Parallel population simulation completed.",
      pathFolder = logFolder
    )
  }

  return(simulationResult)
}

#' @title simulateModelOnCore
#' @description Simulate model, either for an individual or for a given population.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param populationFilePath path to population file to be used for simulation
#' @param resultsFilePath path to CSV files to which results will be saved,
#' @param debugLogFileName path to file where core debug logs are saved
#' @param nodeName node name for parallel simulations
#' @param showProgress option to print progress of simulation to console
#' @export
#' @import ospsuite
simulateModelOnCore <- function(structureSet,
                                populationFilePath = NULL,
                                resultsFilePath,
                                debugLogFileName = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logDebugFile()),
                                nodeName = NULL) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile,
    addToCache = FALSE,
    loadFromCache = FALSE
  )

  logDebug(
    message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), ""), "Simulation file '", structureSet$simulationSet$simulationFile, "' successfully loaded"),
    file = debugLogFileName,
    printConsole = FALSE
  )

  if (!is.null(populationFilePath)) {
    population <- ospsuite::loadPopulation(populationFilePath)
    logDebug(
      message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), ""), "Population file '", populationFilePath, "' successfully loaded"),
      file = debugLogFileName,
      printConsole = FALSE
    )
  }

  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = FALSE)
  simulationResult <- ospsuite::runSimulation(simulation, population = population, simulationRunOptions = simRunOptions)

  logDebug(
    message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), ""), "Simulation run complete"),
    file = debugLogFileName,
    printConsole = FALSE
  )

  ospsuite::exportResultsToCSV(
    results = simulationResult,
    filePath = resultsFilePath
  )
  logDebug(
    message = paste0(ifnotnull(nodeName, paste0(nodeName, ": "), ""), "Simulation results exported to CSV"),
    file = debugLogFileName,
    printConsole = FALSE
  )
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
                          settings = NULL,
                          logFolder = getwd()) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile,
    addToCache = FALSE,
    loadFromCache = FALSE
  )

  logWorkflow(
    message = paste0("Simulation file '", structureSet$simulationSet$simulationFile, "' successfully loaded"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  population <- NULL
  if (!is.null(structureSet$simulationSet$populationFile)) {
    population <- ospsuite::loadPopulation(structureSet$simulationSet$populationFile)
    logWorkflow(
      message = paste0("Population file '", structureSet$simulationSet$populationFile, "' successfully loaded"),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = ifnotnull(settings, outputIfNotNull = settings$showProgress, outputIfNull = FALSE))
  simulationResult <- ospsuite::runSimulation(simulation,
    population = population,
    simulationRunOptions = simRunOptions
  )

  logWorkflow(
    message = "Simulation run complete",
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
runParallelPopulationSimulation <- function(structureSet,
                                            numberOfCores,
                                            settings,
                                            logFolder) {
  populationFileName <- trimFileName(structureSet$simulationSet$populationFile, "csv")

  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) { #-1 since mpi.comm.size() counts master
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }
  Rmpi::mpi.remote.exec(library("ospsuite"))
  Rmpi::mpi.remote.exec(library("ospsuite.reportingengine"))

  tempPopDataFiles <- ospsuite::splitPopulationFile(
    csvPopulationFile = structureSet$simulationSet$populationFile,
    numberOfCores = numberOfCores,
    outputFolder = getwd(),
    outputFileName = populationFileName
  )

  tempLogFileNamePrefix <- file.path(logFolder, "logDebug-core-simulation")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores))
  allResultsFileNames <- paste0(structureSet$simulationSet$simulationSetName, seq(1, numberOfCores), ".csv")

  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.bcast.Robj2slave(obj = populationFileName)
  Rmpi::mpi.bcast.Robj2slave(obj = tempPopDataFiles)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNames)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  Rmpi::mpi.remote.exec(simulateModelOnCore(
    structureSet = structureSet,
    populationFilePath = tempPopDataFiles[mpi.comm.rank()], # populationFilePath = file.path(".", paste0(populationFileName, "_", mpi.comm.rank(), ".csv")),
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    debugLogFileName = tempLogFileNames[mpi.comm.rank()],
    nodeName = paste("Core", mpi.comm.rank())
  ))

  Rmpi::mpi.close.Rslaves()

  for (core in seq(1, numberOfCores)) {
    logWorkflow(
      message = readLines(tempLogFileNames[core]),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  file.remove(tempLogFileNames)
  file.remove(tempPopDataFiles)

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
