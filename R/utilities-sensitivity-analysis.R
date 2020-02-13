#' @title analyzeSensitivity
#' @description Determine whether to run SA for individual or population.  If for individual,  pass simulation to individualSensitivityAnalysis.
#' If SA is for population, loop thru population file, extract parameters for each individual, and pass them to individualSensitivityAnalysis.
#' @return SA results for individual or population
#' @export
#' @import ospsuite
analyzeSensitivity <- function(simFilePath,
                               parametersToPerturb = NULL,
                               popFilePath = NULL,
                               individualID = NULL,
                               numberOfCores = 1,
                               resultsFileFolder = "getwd()",
                               resultsFileName = "sensitivityAnalysisResults") {


  # If there is a population file and individualID then for each individual perform SA
  # If there is a population file and no individualID then do SA for entire population
  # If there is no population file and individualID then do SA for mean model
  # If there is no population file and no individualID then do SA for mean model.



  # Determine if SA is to be done for a single individual or more
  if (!is.null(popFilePath)) {
    popObject <- loadPopulation(popFilePath)
    individualSeq <- individualID %||% seq(1, popObject$count)
    allResultsFileNames <- NULL
    for (ind in individualSeq) {
      resFile <- individualSensitivityAnalysis(
        simFilePath = simFilePath,
        parametersToPerturb = parametersToPerturb,
        individualParameters = popObject$getParameterValuesForIndividual(individualId = ind),
        numberOfCores = numberOfCores,
        resultsFileFolder = resultsFileFolder,
        resultsFileName = paste0(resultsFileName,"_IndividualId_",ind)
      )
      allResultsFileNames <- c(allResultsFileNames, resFile)
    }
  }
  else {
    allResultsFileNames <- individualSensitivityAnalysis(
      simFilePath = simFilePath,
      parametersToPerturb = parametersToPerturb,
      individualParameters = NULL,
      numberOfCores = numberOfCores,
      resultsFileFolder = resultsFileFolder,
      resultsFileName = resultsFileName
    )
  }
  return(allResultsFileNames)
}


#' @title individualSensitivityAnalysis
#' @description Run SA for an individual, possibly after modifying the simulation using individualParameters.  Determine whether to run SA for on single core or in parallel.
#' If on single core, pass simulation to analyzeCoreSensitivity.  If in parallel, pass simulation to runParallelSensitivityAnalysis.
#' @return SA results for individual or population
#' @export
#' @import ospsuite
individualSensitivityAnalysis <- function(simFilePath,
                                          parametersToPerturb = NULL,
                                          individualParameters,
                                          numberOfCores = 1,
                                          resultsFileFolder = resultsFileFolder,
                                          resultsFileName = resultsFileName) {
  # Load simulation to determine number of perturbation parameters
  sim <- loadSimulation(simFilePath)

  # If no perturbation parameters specified, perturb all parameters
  if (is.null(parametersToPerturb)) {
    parametersToPerturb <- ospsuite::potentialVariableParameterPathsFor(simulation = sim)
  }
  totalNumberParameters <- length(parametersToPerturb)

  # In case there are more cores specified in numberOfCores than there are parameters, ensure at least one parameter per spawned core
  numberOfCores <- min(numberOfCores, totalNumberParameters)
  if (totalNumberParameters == 0) {
    stop("No variable parameters found for sensitivity analysis.")
  }

  # Determine if SA is to be done on a single core or more
  if (numberOfCores > 1) {
    allResultsFileNames <- runParallelSensitivityAnalysis(
      simFilePath,
      parametersToPerturb,
      individualParameters,
      numberOfCores,
      resultsFileFolder,
      resultsFileName
    )
  } else {
    # No parallelization
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    allResultsFileNames <- file.path(resultsFileFolder, paste0(resultsFileName, ".csv"))
    analyzeCoreSensitivity(
      simulation = sim,
      parametersToPerturb = parametersToPerturb,
      totalSensitivityThreshold = 1,
      resultsFilePath = allResultsFileNames
    )
  }
  return(allResultsFileNames)
}


#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores, save results as CSV.
#' @return Simulation results for population
#' @export
#' @import ospsuite
#' @import Rmpi
runParallelSensitivityAnalysis <- function(simFilePath,
                                           parametersToPerturb,
                                           individualParameters,
                                           numberOfCores,
                                           resultsFileFolder,
                                           resultsFileName) {
  totalNumberParameters <- length(parametersToPerturb)

  # Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  seqVec <- (1 + ((1:totalNumberParameters) %% numberOfCores)) # Create a vector, of length totalNumberParameters, consisting of a repeating sequence of integers from 1 to numberOfCores
  sortVec <- sort(seqVec) # Sort seqVec to obtain an concatenated array of repeated integers, with the repeated integers ranging from from 1 to numberOfCores.  These are the core numbers to which each parameter will be assigned.
  listSplitParameters <- split(x = parametersToPerturb, sortVec) # Split the parameters of the model according to sortVec
  # listSplitParameters <- split(x = parametersToPerturb, sort(1+( (1:length(totalNumberParameters)) %% numberOfCores))  )
  Rmpi::mpi.spawn.Rslaves(nslaves = numberOfCores)
  Rmpi::mpi.bcast.cmd(library("ospsuite"))
  Rmpi::mpi.bcast.cmd(library("ospsuite.reportingengine"))
  Rmpi::mpi.bcast.Robj2slave(obj = simFilePath)
  Rmpi::mpi.bcast.Robj2slave(obj = listSplitParameters)
  Rmpi::mpi.bcast.Robj2slave(obj = resultsFileFolder)
  Rmpi::mpi.bcast.Robj2slave(obj = individualParameters)

  # Generate a listcontaining names of SA CSV result files that will be output by each core

  allResultsFileNames <- generateResultFileNames(numberOfCores = numberOfCores, folderName = resultsFileFolder, fileName = resultsFileName)

  Rmpi::mpi.bcast.Robj2slave(obj = allResultsFileNames)

  # Load simulation on each core
  Rmpi::mpi.bcast.cmd(sim <- loadSimulation(simFilePath))

  # Update simulation with individual parameters
  Rmpi::mpi.bcast.cmd(updateSimulationIndividualParameters(simulation = sim, individualParameters))



  Rmpi::mpi.remote.exec(analyzeCoreSensitivity(
    simulation = sim,
    parametersToPerturb = listSplitParameters[[mpi.comm.rank()]],
    totalSensitivityThreshold = 1,
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    numberOfCoresToUse = 1 # Number of local cores, set to 1 when parallelizing.
  ))
  Rmpi::mpi.close.Rslaves()

  return(allResultsFileNames)
}

#' @title analyzeCoreSensitivity
#' @description Run a sensitivity analysis from a simulation
#' @param simulation simulation class object
#' @param pkParameterName name of parameter to be analyzed
#' @param totalSensitivityThreshold numeric value between 0 and 1.
#' Close to 0, only the most sensitive output paths are returned.
#' Close to 1, almost all the output paths are returned.
#' @return sensitivityResults
#' @export
#' @import ospsuite
analyzeCoreSensitivity <- function(simulation,
                                   parametersToPerturb = NULL,
                                   totalSensitivityThreshold = 1,
                                   resultsFilePath = paste0(getwd(), "sensitivityAnalysisResults.csv"),
                                   numberOfCoresToUse = NULL) {
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = simulation)
  sensitivityAnalysis$addParameterPaths(parametersToPerturb)

  if (is.null(numberOfCoresToUse)) {
    sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)
  }
  else {
    sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(
      showProgress = FALSE,
      numberOfCoresToUse = numberOfCoresToUse
    )
  }
  print("Running sensitivity analysis...")
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  print("...done")
  exportSensitivityAnalysisResultsToCSV(results = sensitivityAnalysisResults, resultsFilePath)
}
