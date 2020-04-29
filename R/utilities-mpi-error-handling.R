#' @title checkSimulationLoaded
#' @description Check that the simulation has been loaded on the core.
#' @param simulation the simulation object to be checked if loaded successfully
#' @param simulationFilePath the path to the simulation object to be checked if loaded successfully
#' @export
checkSimulationLoaded <- function(simulation,simulationFilePath){
  success <- FALSE
  if( identical(simulation$sourceFile , simulationFilePath) ){
    success <- TRUE
  }
  return(success)
}

#' @title checkLibraryLoaded
#' @description Check that a library has been successfully loaded on the core.
#' @param libraryName the library to be checked if loaded successfully
#' @export
checkLibraryLoaded <- function(libraryName){
  success <- FALSE
  if( libraryName %in% .packages() ){
    success <- TRUE
  }
  return(success)
}


#' @title checkNotNull
#' @description Check that an object is not null.
#' @param object to be checked if not null
#' @export
checkNotNull <- function(object){
  success <- FALSE
  if( !is.null(object) ){
    success <- TRUE
  }
  return(success)
}

#' @title checkAllCoresSuccessful
#' @description Check that all cores executed an mpi.remote.exec command successfully.
#' @param coreResults list of results returned by each core after an mpi.remote.exec command is complete
checkAllCoresSuccessful <- function(coreResults){
  allCoreResults <- sapply(1:length(coreResults),function(n,obj) obj[[n]],coreResults)
  return(all(allCoreResults))
}

#' @title loadSimulationOnCores
#' @description Send structureSet to core, check its simulation has been loaded successfully
#' @param structureSet containing simulationSet which contains path to simulation file and pathIDs to be loaded in simulation object as outputs
loadSimulationOnCores <- function(structureSet,logFolder = getwd()){
  success <- FALSE
  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.remote.exec(sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet))
  simulationLoaded <- Rmpi::mpi.remote.exec( checkSimulationLoaded(simulation = sim,simulationFilePath = structureSet$simulationSet$simulationFile)   )
  success <- checkAllCoresSuccessful(simulationLoaded)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = paste("Simulation file",structureSet$simulationSet$simulationFile,"loaded successfully on all cores"), pathFolder = logFolder)
  } else {
    logErrorThenStop(message = paste("Simulation file",structureSet$simulationSet$simulationFile,"not loaded successfully on all cores") ,logFolderPath = logFolder)
  }
}

#' @title loadSimulationOnCores
#' @description Send libraryName to core, load the library and check that it has been loaded successfully
#' @param libraryName string containing name of library to be loaded
loadLibraryOnCores <- function(libraryName, logFolder = getwd()){
  success <- FALSE
  Rmpi::mpi.bcast.Robj2slave(obj = libraryName)
  Rmpi::mpi.remote.exec(print("11111111"))
  Rmpi::mpi.remote.exec(print(libraryName))
  Rmpi::mpi.remote.exec(library(libraryName,character.only = TRUE))
  libraryLoaded <- Rmpi::mpi.remote.exec(checkLibraryLoaded(libraryName))
  success <- checkAllCoresSuccessful(libraryLoaded)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = paste(libraryName,"loaded successfully on all cores"), pathFolder = logFolder)
  } else {
    logErrorThenStop(message = paste(libraryName,"not loaded successfully on all cores") ,logFolderPath = logFolder)
  }
}

#' @title loadSimulationOnCores
#' @description Send libraryName to core, load the library and check that it has been loaded successfully
#' @param libraryName string containing name of library to be loaded
updateIndividualParametersOnCores <- function(individualParameters , logFolder = getwd()){
  success <- FALSE
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  individualParametersUpdated <- Rmpi::mpi.remote.exec(updateSimulationIndividualParameters(simulation = sim, individualParameters))
  success <- checkAllCoresSuccessful(individualParametersUpdated)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Individual parameters updated successfully on all cores.", pathFolder = logFolder)
  } else {
    logErrorThenStop(message = "Individual parameters updated successfully on all cores." ,logFolderPath = logFolder)
  }
}


#' @title verifySimulationRunSuccessful
#' @description Check that all cores ran simulation successfully
#' @param simulationRunSuccess logical vector indicating success of simulation run on all cores
verifySimulationRunSuccessful <- function(simulationRunSuccess,logFolder = getwd()){
  success <- FALSE
  success <- checkAllCoresSuccessful(simulationRunSuccess)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Simulations completed successfully on all cores.", pathFolder = logFolder)
  } else {
    logErrorThenStop(message = "Simulations not completed successfully on all cores." ,logFolderPath = logFolder)
  }
}


#' @title verifySensitivityAnalysisRunSuccessful
#' @description Check that all cores ran sensitivity analysis successfully
#' @param sensitivityRunSuccess logical vector indicating success of sensitivity analysis run on all cores
verifySensitivityAnalysisRunSuccessful <- function(sensitivityRunSuccess,logFolder = getwd()){
  success <- FALSE
  success <- checkAllCoresSuccessful(sensitivityRunSuccess)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Sensitivity analyses completed successfully on all cores.", pathFolder = logFolder)
  } else {
    logErrorThenStop(message = "Sensitivity analyses not completed successfully on all cores." ,logFolderPath = logFolder)
  }
}

#' @title verifyAnyPreviousFilesRemoved
#' @description Check that any existing partial sensitivity results from individual cores have been removed
#' @param sensitivityRunSuccess logical vector indicating success of result file removal
verifyAnyPreviousFilesRemoved <- function(anyPreviousPartialResultsRemoved,logFolder = getwd()){
  success <- FALSE
  success <- checkAllCoresSuccessful(anyPreviousPartialResultsRemoved)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Verified that no previous partial sensitivity analysis results exist.", pathFolder = logFolder)
  } else {
    logErrorThenStop(message = "Previous partial sensitivity analyses not removed successfully." ,logFolderPath = logFolder)
  }
}

#' @title verifyPartialResultsExported
#' @description Check that partial sensitivity results from individual cores have been exported
#' @param sensitivityRunSuccess logical vector indicating success of result file export
verifyPartialResultsExported <- function(partialResultsExported,logFolder = getwd()){
  success <- FALSE
  success <- checkAllCoresSuccessful(partialResultsExported)
  validateIsLogical(success)
  if (success) {
    logWorkflow(message = "Partial sensitivity analysis results exported successfully.", pathFolder = logFolder)
  } else {
    logErrorThenStop(message = "Partial sensitivity analysis results not exported successfully." ,logFolderPath = logFolder)
  }
}
