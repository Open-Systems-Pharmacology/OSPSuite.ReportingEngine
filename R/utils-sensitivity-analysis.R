#' @title analyzeSensitivity
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




#' @title runParallelSensitivityAnalysis
#' @description Spawn cores, divide parameters among cores, run sensitivity analysis on cores, save results as CSV.
#' @return Simulation results for population
#' @export
#' @import ospsuite
runParallelSensitivityAnalysis <- function(simFilePath,
                                           parametersToPerturb,
                                           individualParameters,
                                           numberOfCores,
                                           resultsFileFolder,
                                           resultsFileName){

  totalNumberParameters <- length(parametersToPerturb)

  # Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  seqVec <- (1 + ((1:totalNumberParameters) %% numberOfCores)) # Create a vector, of length totalNumberParameters, consisting of a repeating sequence of integers from 1 to numberOfCores
  sortVec <- sort(seqVec) # Sort seqVec to obtain an concatenated array of repeated integers, with the repeated integers ranging from from 1 to numberOfCores.  These are the core numbers to which each parameter will be assigned.
  listSplitParameters <- split(x = parametersToPerturb, sortVec) # Split the parameters of the model according to sortVec
  # listSplitParameters <- split(x = parametersToPerturb, sort(1+( (1:length(totalNumberParameters)) %% numberOfCores))  )
  mpi.spawn.Rslaves(nslaves = numberOfCores)
  mpi.bcast.cmd(library("ospsuite"))
  mpi.bcast.cmd(library("ospsuite.reportingengine"))
  mpi.bcast.Robj2slave(obj = simFilePath)
  mpi.bcast.Robj2slave(obj = listSplitParameters)
  mpi.bcast.Robj2slave(obj = resultsFileFolder)
  mpi.bcast.Robj2slave(obj = individualParameters)

  #Generate a listcontaining names of SA CSV result files that will be output by each core
  allResultsFileNames <- sapply(
    X = 1:numberOfCores, function(x, resultsFileFolder, resultsFileName) {
      return(paste0(resultsFileFolder, resultsFileName, "_", x, ".csv"))
    },
    resultsFileFolder = resultsFileFolder,
    resultsFileName = resultsFileName,
    USE.NAMES = FALSE
  )
  mpi.bcast.Robj2slave(obj = allResultsFileNames)


  #Load simulation on each core
  mpi.bcast.cmd(sim <- loadSimulation(simFilePath))

  #Update simulation with individual parameters
  mpi.bcast.cmd(updateSimulationIndividualParameters(simulation = sim, individualParameters))

  mpi.remote.exec(ospsuite.reportingengine::analyzeCoreSensitivity(
    simulation = sim,
    perturbationParameterNamesVector = listSplitParameters[[mpi.comm.rank()]],
    totalSensitivityThreshold = 1,
    resultsFilePath = allResultsFileNames[mpi.comm.rank()],
    numberOfCoresToUse = 1 # Number of local cores, set to 1 when parallelizing.
  ))


  mpi.close.Rslaves()

}
