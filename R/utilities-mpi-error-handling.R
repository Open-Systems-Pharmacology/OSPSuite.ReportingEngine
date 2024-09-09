#' @title loadSimulationOnCores
#' @description Send structureSet to core, check its simulation has been loaded successfully
#' @param structureSet containing simulationSet which contains path to simulation file and pathIDs to be loaded in simulation object as outputs
#' @keywords internal
loadSimulationOnCores <- function(structureSet) {
  # Rmpi being only suggested, first check if installed
  if (!requireNamespace("Rmpi", quietly = TRUE)) {
    stop(messages$errorPackageNotInstalled("Rmpi"))
  }
  # Initially, simulation was internally named "sim" in Rmpi
  # Updating the name here may lead to errors with other functions calling Rmpi
  Rmpi::mpi.bcast.Robj2slave(obj = structureSet)
  Rmpi::mpi.remote.exec(sim <- NULL)
  Rmpi::mpi.remote.exec(sim <- loadSimulationWithUpdatedPaths(structureSet$simulationSet))
  isLoadedOnEachCore <- Rmpi::mpi.remote.exec(isLoadedSimulation(simulation = sim))
  validateHasRunOnAllCores(
    coreResults = isLoadedOnEachCore,
    inputName = structureSet$simulationSet$simulationFile,
    inputType = "Simulation"
  )
  logDebug(messages$loadedOnCores(paste("Simulation", structureSet$simulationSet$simulationFile)))
  return(invisible())
}

#' @title loadPopulationOnCores
#' @description Send population file names to cores, check that population is loaded on each core successfully
#' @param populationFiles population files to be loaded on cores
#' @keywords internal
loadPopulationOnCores <- function(populationFiles) {
  # Rmpi being only suggested, first check if installed
  if (!requireNamespace("Rmpi", quietly = TRUE)) {
    stop(messages$errorPackageNotInstalled("Rmpi"))
  }
  Rmpi::mpi.bcast.Robj2slave(obj = populationFiles)
  Rmpi::mpi.remote.exec(population <- ospsuite::loadPopulation(populationFiles[Rmpi::mpi.comm.rank()]))
  isLoadedOnEachCore <- Rmpi::mpi.remote.exec(isLoadedPopulation(population = population))
  validateHasRunOnAllCores(
    coreResults = isLoadedOnEachCore,
    inputName = populationFiles,
    inputType = "Populations"
  )
  logDebug(messages$loadedOnCores(paste("Populations", paste(populationFiles, collapse = ", "))))
  return(invisible())
}

#' @title loadPackageOnCores
#' @description Load `packageName` namespace to core, load the library and check that it has been loaded successfully
#' @param packageName Name of package to be loaded
#' @keywords internal
loadPackageOnCores <- function(packageName) {
  # Rmpi being only suggested, first check if installed
  if (!requireNamespace("Rmpi", quietly = TRUE)) {
    stop(messages$errorPackageNotInstalled("Rmpi"))
  }
  Rmpi::mpi.bcast.Robj2slave(obj = packageName)
  Rmpi::mpi.remote.exec(library(packageName, character.only = TRUE))
  isLoadedOnEachCore <- Rmpi::mpi.remote.exec(isLoadedPackage(packageName))
  validateHasRunOnAllCores(
    coreResults = isLoadedOnEachCore,
    inputName = packageName,
    inputType = "Package"
  )
  logDebug(messages$loadedOnCores(paste("Package", packageName)))
  return(invisible())
}

#' @title updateIndividualParametersOnCores
#' @description Update individual parameters on core
#' @param individualParameters parameters to update
#' @keywords internal
updateIndividualParametersOnCores <- function(individualParameters) {
  # Rmpi being only suggested, first check if installed
  if (!requireNamespace("Rmpi", quietly = TRUE)) {
    stop(messages$errorPackageNotInstalled("Rmpi"))
  }
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)
  isUpdatedOnEachCore <- Rmpi::mpi.remote.exec(
    updateSimulationIndividualParameters(
      simulation = sim,
      individualParameters
    )
  )
  validateHasRunOnAllCores(
    coreResults = isUpdatedOnEachCore,
    inputName = paste(individualParameters$paths, collapse = ", "),
    inputType = "Individual parameters"
  )
  logDebug(messages$loadedOnCores(
    paste(
      "Individual parameters",
      paste(individualParameters$paths, collapse = ", ")
    )
  ))
  return(invisible())
}
