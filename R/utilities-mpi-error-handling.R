#' @title checkSimulationLoaded
#' @description Check that the simulation has been loaded on the core.
#' @param simulation the simulation object to be checked if loaded successfully
#' @export
checkSimulationLoaded <- function(simulation) {
  return(is.null(validateIsOfType(simulation, "Simulation")))
}

#' @title checkPopulationLoaded
#' @description Check that the population has been loaded on the core.
#' @param population the population object to be checked if loaded successfully
#' @export
checkPopulationLoaded <- function(population) {
  return(is.null(validateIsOfType(population, "Population")))
}

#' @title checkLibraryLoaded
#' @description Check that a library has been successfully loaded on the core.
#' @param libraryName the library to be checked if loaded successfully
#' @export
checkLibraryLoaded <- function(libraryName) {
  return(libraryName %in% .packages())
}

#' @title checkAllCoresSuccessful
#' @description Check that all cores executed an mpi.remote.exec command successfully.
#' @param coreResults list of results returned by each core after an mpi.remote.exec command is complete
#' @keywords internal
checkAllCoresSuccessful <- function(coreResults) {
  allCoreResults <- sapply(coreResults, function(obj) {
    obj
  })
  return(all(allCoreResults))
}

#' @title loadSimulationOnCores
#' @description Send structureSet to core, check its simulation has been loaded successfully
#' @param structureSet containing simulationSet which contains path to simulation file and pathIDs to be loaded in simulation object as outputs
#' @param logFolder folder where the logs are saved
#' @keywords internal
loadSimulationOnCores <- function(structureSet, logFolder) {
  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.remote.exec(sim <- NULL)
  Rmpi::mpi.remote.exec(sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet))
  simulationLoaded <- Rmpi::mpi.remote.exec(checkSimulationLoaded(simulation = sim))
  success <- checkAllCoresSuccessful(simulationLoaded)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = paste("Simulation file", structureSet$simulationSet$simulationFile, "not loaded successfully on all cores"), logFolderPath = logFolder)
  }
  logWorkflow(message = paste("Simulation file", structureSet$simulationSet$simulationFile, "loaded successfully on all cores"), pathFolder = logFolder)
  return(invisible())
}

#' @title loadPopulationOnCores
#' @description Send population file names to cores, check that population is loaded on each core successfully
#' @param populationFiles population files to be loaded on cores
#' @param logFolder folder where the logs are saved
#' @keywords internal
loadPopulationOnCores <- function(populationFiles, logFolder) {
  Rmpi::mpi.bcast.Robj2slave(obj = populationFiles)
  Rmpi::mpi.remote.exec(population <- ospsuite::loadPopulation(populationFiles[Rmpi::mpi.comm.rank()]))
  populationLoaded <- Rmpi::mpi.remote.exec(checkPopulationLoaded(population = population))
  success <- checkAllCoresSuccessful(populationLoaded)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = paste("Population files not loaded successfully on all cores"), logFolderPath = logFolder)
  }
  logWorkflow(message = paste("Population files loaded successfully on all cores"), pathFolder = logFolder)
  return(invisible())
}

#' @title loadLibraryOnCores
#' @description Send libraryName to core, load the library and check that it has been loaded successfully
#' @param libraryName string containing name of library to be loaded
#' @param logFolder folder where the logs are saved
#' @keywords internal
loadLibraryOnCores <- function(libraryName, logFolder) {
  Rmpi::mpi.bcast.Robj2slave(obj = libraryName)
  Rmpi::mpi.remote.exec(library(libraryName, character.only = TRUE))
  libraryLoaded <- Rmpi::mpi.remote.exec(checkLibraryLoaded(libraryName))
  success <- checkAllCoresSuccessful(libraryLoaded)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = paste(libraryName, "not loaded successfully on all cores"), logFolderPath = logFolder)
  }
  logWorkflow(message = paste(libraryName, "loaded successfully on all cores"), pathFolder = logFolder)
  return(invisible())
}

#' @title updateIndividualParametersOnCores
#' @description Update individual parameters on core
#' @param individualParameters parameters to update
#' @param logFolder folder where the logs are saved
#' @keywords internal
updateIndividualParametersOnCores <- function(individualParameters, logFolder) {
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  individualParametersUpdated <- Rmpi::mpi.remote.exec(updateSimulationIndividualParameters(simulation = sim, individualParameters))
  success <- checkAllCoresSuccessful(individualParametersUpdated)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = "Individual parameters updated successfully on all cores.", logFolderPath = logFolder)
  }
  logWorkflow(message = "Individual parameters updated successfully on all cores.", pathFolder = logFolder)
  return(invisible())
}

#' @title verifySimulationRunSuccessful
#' @description Check that all cores ran simulation successfully
#' @param simulationRunSuccess logical vector indicating success of simulation run on all cores
#' @param tempPopDataFiles name of all temporary population files sent to cores
#' @param logFolder folder where the logs are saved
#' @keywords internal
verifySimulationRunSuccessful <- function(simulationRunSuccess, tempPopDataFiles, logFolder) {
  success <- checkAllCoresSuccessful(simulationRunSuccess)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Simulations completed successfully on all cores.", pathFolder = logFolder)
  } else {
    unsuccessfulCores <- setdiff(1:length(tempPopDataFiles), which(unlist(unname(simulationRunSuccess))))
    for (core in unsuccessfulCores) {
      pop <- ospsuite::loadPopulation(tempPopDataFiles[core])
      logErrorMessage(message = paste("Simulations for individuals", paste(pop$allIndividualIds, collapse = ", "), "not completed successfully."), logFolderPath = logFolder)
    }
  }
}


#' @title verifySensitivityAnalysisRunSuccessful
#' @description Check that all cores ran sensitivity analysis successfully
#' @param sensitivityRunSuccess logical vector indicating success of sensitivity analysis run on all cores
#' @param logFolder folder where the logs are saved
#' @keywords internal
verifySensitivityAnalysisRunSuccessful <- function(sensitivityRunSuccess, logFolder) {
  success <- checkAllCoresSuccessful(sensitivityRunSuccess)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = "Sensitivity analyses not completed successfully on all cores.", logFolderPath = logFolder)
  }
  logWorkflow(message = "Sensitivity analyses completed successfully on all cores.", pathFolder = logFolder)
  return(invisible())
}

#' @title verifyAnyPreviousFilesRemoved
#' @description Check that any existing results from individual cores have been removed
#' @param anyPreviousPartialResultsRemoved result files to be removed
#' @param logFolder folder where the logs are saved
#' @keywords internal
verifyAnyPreviousFilesRemoved <- function(anyPreviousPartialResultsRemoved, logFolder) {
  success <- checkAllCoresSuccessful(anyPreviousPartialResultsRemoved)
  validateIsLogical(success)
  if (!success) {
    logErrorThenStop(message = "Previous results not removed successfully.", logFolderPath = logFolder)
  }
  logWorkflow(message = "Verified that no previous results exist.", pathFolder = logFolder)
  return(invisible())
}

#' @title verifyPartialResultsExported
#' @description Check that results from individual cores have been exported
#' @param partialResultsExported logical vector indicating success of result file export
#' @param numberOfCores number of cores from which result files are to be exported
#' @param logFolder folder where the logs are saved
#' @keywords internal
verifyPartialResultsExported <- function(partialResultsExported, numberOfCores, logFolder) {
  success <- checkAllCoresSuccessful(partialResultsExported)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "All successful core results exported successfully.", pathFolder = logFolder)
  } else {
    unsuccessfulCores <- setdiff(1:numberOfCores, which(unlist(unname(partialResultsExported))))
    logErrorMessage(message = paste("Results from cores", paste(unsuccessfulCores, collapse = ", "), "not exported successfully."), logFolderPath = logFolder)
  }
}
