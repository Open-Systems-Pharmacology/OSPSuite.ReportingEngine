#' @title simulateWorkflowModels
#' @description Simulate model for a population.  Run parallel workflow if numberOfCores > 1 AND population size is >1.
#' @param structureSets List of `SimulationStructure` objects contain paths of files to be used
#' @param settings list of simulation settings
#' @param logFolder folder where the logs are saved
#' @return Simulation results for individual or population
#' @import ospsuite
#' @keywords internal
simulateWorkflowModels <- function(structureSets,
                                   settings = NULL,
                                   logFolder = getwd()) {
  allSimulationResults <- vector(mode = "list", length = length(structureSets))
  # Split between mean and population simulations
  populationSets <- sapply(structureSets, function(set) isOfType(set$simulationSet, "PopulationSimulationSet"))
  populationSimulationResults <- NULL
  simulationResults <- NULL

  # Run population simulations in series
  if (sum(populationSets) > 0) {
    populationSimulationResults <- simulateModelForPopulation(
      structureSets = structureSets[populationSets],
      settings = settings,
      logFolder = logFolder
    )
  }

  # Run mean simulations in parallel
  if (sum(!populationSets) > 0) {
    simulationResults <- simulateModelParallel(
      structureSet = structureSets[!populationSets],
      settings = settings,
      logFolder = logFolder
    )
  }
  # Place the simulation results in same order as structureSet
  allSimulationResults[populationSets] <- c(populationSimulationResults)
  allSimulationResults[!populationSets] <- c(simulationResults)

  return(allSimulationResults)
}

#' @title simulateModelForPopulation
#' @description Simulate model for a population.  Run parallel workflow if numberOfCores > 1 AND population size is >1.
#' @param structureSets List of `SimulationStructure` objects contain paths of files to be used
#' @param settings list of simulation settings
#' @param logFolder folder where the logs are saved
#' @return Simulation results for individual or population
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
simulateModelForPopulation <- function(structureSets,
                                       settings = NULL,
                                       logFolder = getwd()) {
  simulationResults <- NULL
  for (set in structureSets) {
    re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$populationFile)
    population <- loadWorkflowPopulation(set$simulationSet)
    numberOfIndividuals <- length(population$allIndividualIds)
    numberOfCores <- min(settings$numberOfCores %||% 1, numberOfIndividuals)

    # If one core available, run in series
    if (numberOfCores == 1) {
      settings <- settings %||% SimulationSettings$new()
      settings$allowedCores <- getAllowedCores()

      simulationResult <- simulateModel(
        structureSet = set,
        settings = settings,
        logFolder = logFolder
      )
      simulationResults <- c(simulationResults, simulationResult)
      next
    }

    # If multiple cores available, run in parallel
    logWorkflow(
      message = paste0("Starting parallel population simulation on ", numberOfCores, " cores"),
      pathFolder = logFolder
    )

    simulationResultFileNames <- runParallelPopulationSimulation(
      numberOfCores = numberOfCores,
      structureSet = set,
      settings = settings,
      logFolder = logFolder
    )

    re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$simulationFile)
    simulationResult <- ospsuite::importResultsFromCSV(
      simulation = loadSimulationWithUpdatedPaths(set$simulationSet),
      filePaths = simulationResultFileNames
    )
    file.remove(simulationResultFileNames)

    logWorkflow(
      message = "Parallel population simulation completed.",
      pathFolder = logFolder
    )
    simulationResults <- c(simulationResults, simulationResult)
  }

  return(simulationResults)
}

#' @title simulateModelOnCore
#' @description Simulate model, either for an individual or for a given population.
#' @param simulation A `Simulation` object
#' @param population A `Population` object
#' @param debugLogFileName path to file where core debug logs are saved
#' @param nodeName node name for parallel simulations
#' @param showProgress option to print progress of simulation to console
#' @export
#' @import ospsuite
simulateModelOnCore <- function(simulation,
                                population, # resultsFilePath,
                                debugLogFileName = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logDebugFile()), # errorLogFileName = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logErrorFile()),
                                nodeName = NULL,
                                showProgress = FALSE) {
  logDebug(
    message = paste0(ifNotNull(nodeName, paste0(nodeName, ": "), ""), "Starting simulation."),
    file = debugLogFileName,
    printConsole = TRUE
  )

  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = showProgress, numberOfCores = 1)
  simulationResult <- NULL
  simulationResult <- ospsuite::runSimulation(simulation = simulation, population = population, simulationRunOptions = simRunOptions)

  logDebug(
    message = paste0(ifNotNull(nodeName, paste0(nodeName, ": "), ""), "Simulation run complete."),
    file = debugLogFileName,
    printConsole = TRUE
  )
  return(simulationResult)
}


#' @title simulateModelParallel
#' @description Simulate models within a list of structure sets in parallel for an individual.
#' @param structureSets, a list of `SimulationStructure` R6 class objects contain paths of files to be used
#' @param settings list of options to be passed to the function
#' @param logFolder folder where the logs are saved
#' @return List of simulation results for each simulation set
#' @import ospsuite
#' @keywords internal
simulateModelParallel <- function(structureSets,
                                  settings = NULL,
                                  logFolder = getwd()) {

  simulationResults <- list()
  maxSimulationsPerSubset <- settings$maxSimulationsPerCore*getAllowedCores()  #To be set in settings argument

  #Split the complete set of structureSets into a list of subsets of structureSets, each containing at most maxSimulationsPerSubset structureSets
  structureSetList <- split(structureSets,ceiling(seq_along(structureSets)/maxSimulationsPerSubset))

  #Loop through the list of structureSet subsets
  for (subsetNumber in seq_along(structureSetList)){

    structureSetsSubset <- structureSetList[[subsetNumber]]

    logWorkflow(
      message = paste("Loading subset",subsetNumber,"of",length(structureSetList)),
      pathFolder = logFolder,
      logTypes = LogTypes$Info
    )

    simulations <- lapply(structureSetsSubset, function(set) {
      re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$simulationFile)
      simulation <- loadSimulationWithUpdatedPaths(set$simulationSet,loadFromCache = TRUE)
      logWorkflow(
        message = paste0("Simulation file '", set$simulationSet$simulationFile, "' successfully loaded"),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )
      return(simulation)
    })

    logWorkflow(
      message = paste("Simulating subset",subsetNumber,"of",length(structureSetList)),
      pathFolder = logFolder,
      logTypes = LogTypes$Info
    )

    subsetSimulationResults <- ospsuite::runSimulations(
      simulations = simulations
    )

    simulationResults <- c(simulationResults,subsetSimulationResults)

  }

  logWorkflow(
    message = "Parallel simulation run complete",
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  return(c(simulationResults))
}

#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @param logFolder folder where the logs are saved
#' @return Simulation results for individual or population
#' @import ospsuite
#' @keywords internal
simulateModel <- function(structureSet,
                          settings = NULL,
                          logFolder = getwd()) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  logWorkflow(
    message = paste0("Simulation file '", structureSet$simulationSet$simulationFile, "' successfully loaded"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  population <- NULL
  if (!is.null(structureSet$simulationSet$populationFile)) {
    population <- loadWorkflowPopulation(structureSet$simulationSet)
    logWorkflow(
      message = paste0("Population file '", structureSet$simulationSet$populationFile, "' successfully loaded"),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  ospsuite::clearOutputs(simulation)
  allSimulationSetPaths <- sapply(structureSet$simulationSet$outputs, function(output) {
    output$path
  })
  quantitiesToSimulate <- ospsuite::getAllQuantitiesMatching(paths = allSimulationSetPaths, simulation)
  for (quantity in quantitiesToSimulate) {
    ospsuite::addOutputs(quantitiesOrPaths = quantity, simulation = simulation)
  }

  simRunOptions <- ospsuite::SimulationRunOptions$new(
    showProgress = ifNotNull(settings, outputIfNotNull = settings$showProgress, outputIfNull = FALSE),
    numberOfCores = settings$allowedCores
  )

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
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param numberOfCores number of cores do be used by the parallel simulation
#' @param settings list of options to be passed on the function
#' @param logFolder folder where the logs are saved
#' @return Simulation results for population
#' @import ospsuite
#' @keywords internal
runParallelPopulationSimulation <- function(structureSet,
                                            numberOfCores,
                                            settings,
                                            logFolder) {
  populationFileName <- trimFileName(structureSet$simulationSet$populationFile, "csv")

  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  # numberOfCores = Rmpi::mpi.comm.size() - 1 since mpi.comm.size() counts master
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) {
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }

  loadLibraryOnCores(libraryName = "ospsuite.reportingengine", logFolder = logFolder)
  loadLibraryOnCores(libraryName = "ospsuite", logFolder = logFolder)

  tempPopDataFiles <- ospsuite::splitPopulationFile(
    csvPopulationFile = structureSet$simulationSet$populationFile,
    numberOfCores = numberOfCores,
    outputFolder = structureSet$workflowFolder,
    outputFileName = populationFileName
  )

  tempLogFileNamePrefix <- file.path(logFolder, "logDebug-core-simulation")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq(1, numberOfCores), ".txt")
  allResultsFileNames <- paste0(structureSet$simulationSet$simulationSetName, seq(1, numberOfCores), ".csv")

  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.bcast.Robj2slave(obj = settings)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNames)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  # Load simulation on each core
  loadSimulationOnCores(structureSet = structureSet, logFolder = logFolder)
  loadPopulationOnCores(populationFiles = tempPopDataFiles, logFolder = logFolder)

  # Run simulation on each core
  Rmpi::mpi.remote.exec(simulationResult <- simulateModelOnCore(
    simulation = sim,
    population = population,
    debugLogFileName = tempLogFileNames[mpi.comm.rank()],
    nodeName = paste("Core", mpi.comm.rank()),
    showProgress = settings$showProgress
  ))

  # Verify simulations ran successfully
  simulationRunSuccess <- Rmpi::mpi.remote.exec(!(simulationResult$count == 0))
  successfulCores <- which(unlist(unname(simulationRunSuccess)))
  verifySimulationRunSuccessful(
    simulationRunSuccess = simulationRunSuccess,
    tempPopDataFiles = tempPopDataFiles,
    logFolder = logFolder
  )


  # Write core logs to workflow logs
  for (core in seq(1, numberOfCores)) {
    logWorkflow(
      message = readLines(tempLogFileNames[core]),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }

  # Remove any previous temporary results files
  Rmpi::mpi.remote.exec(if (file.exists(allResultsFileNames[mpi.comm.rank()])) {
    file.remove(allResultsFileNames[mpi.comm.rank()])
  })
  anyPreviousPartialResultsRemoved <- Rmpi::mpi.remote.exec(!file.exists(allResultsFileNames[mpi.comm.rank()]))
  verifyAnyPreviousFilesRemoved(anyPreviousPartialResultsRemoved, logFolder = logFolder)

  # Export temporary results files to CSV
  Rmpi::mpi.remote.exec(ospsuite::exportResultsToCSV(
    results = simulationResult,
    filePath = allResultsFileNames[mpi.comm.rank()]
  ))
  partialResultsExported <- Rmpi::mpi.remote.exec(file.exists(allResultsFileNames[mpi.comm.rank()]))
  verifyPartialResultsExported(partialResultsExported,
    numberOfCores = numberOfCores,
    logFolder = logFolder
  )

  # Close slaves
  Rmpi::mpi.close.Rslaves()

  # Remove temporary log and population date files.
  file.remove(tempLogFileNames)
  file.remove(tempPopDataFiles)

  # Return names of temporary results files from cores that completed simulation successfully.
  return(allResultsFileNames[successfulCores])
}


#' @title updateSimulationIndividualParameters
#' @description Update simulation with parameters from individualParameters
#' @param simulation is the simulation object to be updated
#' @param individualParameters is an object storing an individual's parameters, obtained from a population object's getParameterValuesForIndividual() function.
#' @export
#' @import ospsuite
updateSimulationIndividualParameters <- function(simulation, individualParameters = NULL) {
  if (is.null(individualParameters)) {
    return(TRUE)
  }
  ospsuite::setParameterValuesByPath(parameterPaths = individualParameters$paths, values = individualParameters$values, simulation = simulation)
  return(TRUE)
}

#' @title defaultSimulationNumberOfCores
#' @description default numberOfCores for simulation
#' @export
defaultSimulationNumberOfCores <- 1
