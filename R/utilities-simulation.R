#' @title simulateWorkflowModels
#' @description Simulate model for a population.  Run parallel workflow if numberOfCores > 1 AND population size is >1.
#' @param structureSets List of `SimulationStructure` objects contain paths of files to be used
#' @param settings list of simulation settings
#' @return Simulation results for individual or population
#' @import ospsuite
#' @keywords internal
simulateWorkflowModels <- function(structureSets, settings = NULL) {
  allSimulationResults <- vector(mode = "list", length = length(structureSets))
  # Split between mean and population simulations
  populationSets <- sapply(structureSets, function(set) isOfType(set$simulationSet, "PopulationSimulationSet"))
  populationSimulationResults <- NULL
  simulationResults <- NULL

  # Run population simulations in series
  if (sum(populationSets) > 0) {
    populationSimulationResults <- simulateModelForPopulation(
      structureSets = structureSets[populationSets],
      settings = settings
    )
  }

  # Run mean simulations in parallel
  if (sum(!populationSets) > 0) {
    simulationResults <- simulateModelParallel(
      structureSets = structureSets[!populationSets],
      settings = settings
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
#' @return Simulation results for individual or population
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
simulateModelForPopulation <- function(structureSets, settings = NULL) {
  simulationResults <- NULL
  setIndex <- 0
  logInfo(messages$runStarting("Population Simulations"))
  # Display a nice progress bar for users
  simulationProgress <- txtProgressBar(max = length(structureSets), style = 3)
  cat("\n")
  # Loop through the list of structureSets
  for (set in structureSets) {
    re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$populationFile)
    population <- loadWorkflowPopulation(set$simulationSet)
    numberOfIndividuals <- length(population$allIndividualIds)
    numberOfCores <- min(settings$numberOfCores %||% 1, numberOfIndividuals)

    # Display name of simulation and population on console
    setName <- paste0(
      set$simulationSet$simulationSetName,
      " (", set$simulationSet$populationName, ")",
      ifelse(numberOfCores == 1, "", paste(" using", numberOfCores, "cores"))
    )
    logInfo(messages$runStarting(setName))

    # If one core available, run in series
    if (numberOfCores == 1) {
      settings <- settings %||% SimulationSettings$new()
      settings$allowedCores <- getAllowedCores()

      simulationResult <- simulateModel(
        structureSet = set,
        settings = settings
      )
      simulationResults <- c(simulationResults, simulationResult)
      # Update progress bar after each simulation
      setIndex <- setIndex + 1
      setTxtProgressBar(simulationProgress, value = setIndex)
      cat("\n")
      next
    }

    simulationResultFileNames <- runParallelPopulationSimulation(
      numberOfCores = numberOfCores,
      structureSet = set,
      settings = settings
    )

    re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$simulationFile)
    simulationResult <- ospsuite::importResultsFromCSV(
      simulation = loadSimulationWithUpdatedPaths(set$simulationSet),
      filePaths = simulationResultFileNames
    )
    file.remove(simulationResultFileNames)

    logDebug("Parallel population simulation completed.")
    simulationResults <- c(simulationResults, simulationResult)
    # Update progress bar after each simulation
    setIndex <- setIndex + 1
    setTxtProgressBar(simulationProgress, value = setIndex)
    cat("\n")
  }
  close(simulationProgress)
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
                                debugLogFileName = file.path(getwd(), defaultFileNames$logDebugFile()),
                                nodeName = NULL,
                                showProgress = FALSE) {
  write(
    paste0(ifNotNull(nodeName, paste0(nodeName, ": "), ""), "Starting simulation."),
    file = debugLogFileName,
    append = TRUE
  )

  simRunOptions <- ospsuite::SimulationRunOptions$new(showProgress = showProgress, numberOfCores = 1)
  simulationResult <- NULL
  simulationResult <- ospsuite::runSimulation(simulation = simulation, population = population, simulationRunOptions = simRunOptions)

  write(
    paste0(ifNotNull(nodeName, paste0(nodeName, ": "), ""), "Simulation run complete."),
    file = debugLogFileName,
    append = TRUE
  )
  return(simulationResult)
}


#' @title simulateModelParallel
#' @description Simulate models within a list of structure sets in parallel for an individual.
#' @param structureSets, a list of `SimulationStructure` R6 class objects contain paths of files to be used
#' @param settings list of options to be passed to the function
#' @return List of simulation results for each simulation set
#' @import ospsuite
#' @keywords internal
simulateModelParallel <- function(structureSets, settings = NULL) {
  simulationResults <- list()
  maxSimulationsPerSubset <- settings$maxSimulationsPerCore * getAllowedCores() # To be set in settings argument

  # Split the complete set of structureSets into a list of subsets of structureSets, each containing at most maxSimulationsPerSubset structureSets
  structureSetList <- split(structureSets, ceiling(seq_along(structureSets) / maxSimulationsPerSubset))

  messages$subsetsCreated

  logInfo(messages$subsetsCreated(length(structureSetList), length(structureSets)))
  logInfo(messages$runStarting("subset simulations"))
  # Display a nice progress bar for users
  simulationProgress <- txtProgressBar(max = length(structureSetList), style = 3)
  # Loop through the list of structureSet subsets
  for (subsetNumber in seq_along(structureSetList)) {
    structureSetsSubset <- structureSetList[[subsetNumber]]
    simulations <- lapply(structureSetsSubset, function(set) {
      re.tStoreFileMetadata(access = "read", filePath = set$simulationSet$simulationFile)
      simulation <- loadSimulationWithUpdatedPaths(set$simulationSet, loadFromCache = TRUE)
      logDebug(messages$simulationLoaded(paste0("Simulation file '", set$simulationSet$simulationFile, "' successfully loaded")))
      return(simulation)
    })
    # Update progress bar after each simulation
    setTxtProgressBar(simulationProgress, value = subsetNumber)

    # Catch, save and display any error or warning
    # from ospsuite::runSimulations
    subsetSimulationResults <- tryCatch(
      {
        ospsuite::runSimulations(simulations = simulations)
      },
      error = function(e) {
        close(simulationProgress)
        logErrorThenStop(e)
      },
      warning = function(w) {
        logError(w)
        # Since simulationResults is a list,
        # return same output type
        return(vector(mode = "list", length = length(simulations)))
      }
    )

    # Stop simulation run if any individual simulations not run successfully in ospsuite::runSimulations
    failedSimulationIndices <- which(sapply(subsetSimulationResults, function(x) {
      is.null(x)
    }))

    if (!isEmpty(failedSimulationIndices)) {
      errorMessages <- sapply(failedSimulationIndices, function(setNumber) {
        set <- structureSetsSubset[[setNumber]]
        setSimFile <- set$simulationSet$simulationFile
        setName <- set$simulationSet$simulationSetName
        messages$errorRunSimulationsNotSuccessful(setSimFile, setName)
      })

      stop(paste(errorMessages, collapse = "; "))
    }

    simulationResults <- c(simulationResults, subsetSimulationResults)
    clearMemory(clearSimulationsCache = TRUE)
  }
  close(simulationProgress)
  return(c(simulationResults))
}

#' @title simulateModel
#' @description Simulate model, either for an individual or for a given population.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @return Simulation results for individual or population
#' @import ospsuite
#' @keywords internal
simulateModel <- function(structureSet, settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  logDebug(paste0("Simulation file '", structureSet$simulationSet$simulationFile, "' successfully loaded"))

  population <- NULL
  if (!is.null(structureSet$simulationSet$populationFile)) {
    population <- loadWorkflowPopulation(structureSet$simulationSet)
    logDebug(paste0("Population file '", structureSet$simulationSet$populationFile, "' successfully loaded"))
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

  logDebug("Simulation run complete")

  return(simulationResult)
}

#' @title runParallelPopulationSimulation
#' @description Spawn cores, divide population among cores, run population simulation on cores, save results as CSV.
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param numberOfCores number of cores do be used by the parallel simulation
#' @param settings list of options to be passed on the function
#' @return Simulation results for population
#' @import ospsuite
#' @keywords internal
runParallelPopulationSimulation <- function(structureSet,
                                            numberOfCores,
                                            settings) {
  populationFileName <- trimFileName(structureSet$simulationSet$populationFile, "csv")

  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)

  # Check that the correct number of slaves has been spawned.
  # numberOfCores = Rmpi::mpi.comm.size() - 1 since mpi.comm.size() counts master
  if (!(Rmpi::mpi.comm.size() - 1 == numberOfCores)) {
    Rmpi::mpi.close.Rslaves()
    stop(paste0(numberOfCores, " cores were not successfully spawned."))
  }

  loadPackageOnCores("ospsuite.reportingengine")
  loadPackageOnCores("ospsuite")

  tempPopDataFiles <- ospsuite::splitPopulationFile(
    csvPopulationFile = structureSet$simulationSet$populationFile,
    numberOfCores = numberOfCores,
    outputFolder = structureSet$workflowFolder,
    outputFileName = populationFileName
  )

  tempLogFileNamePrefix <- file.path(reEnv$log$folder, "logDebug-core-simulation")
  tempLogFileNames <- paste0(tempLogFileNamePrefix, seq_len(numberOfCores), ".txt")
  allResultsFileNames <- paste0(structureSet$simulationSet$simulationSetName, seq_len(numberOfCores), ".csv")

  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.bcast.Robj2slave(obj = settings)
  Rmpi::mpi.bcast.Robj2slave(obj = tempLogFileNames)
  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  # Load simulation on each core
  loadSimulationOnCores(structureSet = structureSet)
  loadPopulationOnCores(populationFiles = tempPopDataFiles)

  # Run simulation on each core
  Rmpi::mpi.remote.exec(simulationResult <- simulateModelOnCore(
    simulation = sim,
    population = population,
    debugLogFileName = tempLogFileNames[Rmpi::mpi.comm.rank()],
    nodeName = paste("Core", Rmpi::mpi.comm.rank()),
    showProgress = settings$showProgress
  ))
  # Check and warn if some runs were not successful for specific split population files
  simulationRunSuccess <- Rmpi::mpi.remote.exec(!(simulationResult$count == 0))
  successfulCores <- which(unlist(unname(simulationRunSuccess)))
  checkHasRunOnAllCores(
    coreResults = simulationRunSuccess,
    inputName = tempPopDataFiles,
    inputType = paste(
      "Run(s) using simulation file '",
      structureSet$simulationSet$simulationFile,
      "' and population file(s)"
    ),
    runType = "task"
  )

  # Write core logs to workflow logs
  for (core in seq_len(numberOfCores)) {
    logDebug(readLines(tempLogFileNames[core]))
  }

  # Remove any previous temporary results files
  Rmpi::mpi.remote.exec(
    if (file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()])) {
      file.remove(allResultsFileNames[Rmpi::mpi.comm.rank()])
    }
  )
  validateHasRunOnAllCores(
    coreResults = Rmpi::mpi.remote.exec(!file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()])),
    inputName = allResultsFileNames,
    inputType = "Clean up of temporary files",
    runType = "task"
  )

  # Export temporary results files to CSV
  Rmpi::mpi.remote.exec(ospsuite::exportResultsToCSV(
    results = simulationResult,
    filePath = allResultsFileNames[Rmpi::mpi.comm.rank()]
  ))
  # Check and warn if some runs could not be exported
  checkHasRunOnAllCores(
    coreResults = Rmpi::mpi.remote.exec(file.exists(allResultsFileNames[Rmpi::mpi.comm.rank()])),
    inputName = allResultsFileNames,
    inputType = "Export of simulation results for",
    runType = "task"
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
  ospsuite::setParameterValuesByPath(
    parameterPaths = individualParameters$paths,
    values = individualParameters$values,
    simulation = simulation,
    # Issue #497 prevent crash if parameter is not found
    stopIfNotFound = FALSE
  )
  return(TRUE)
}
