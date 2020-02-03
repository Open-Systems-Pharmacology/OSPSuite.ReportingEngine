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
analyzeSensitivity <- function(simFilePath,
                               perturbationParameterNamesVector = NULL,
                               totalSensitivityThreshold = 1,
                               individualParameters = NULL,
                               resultsFilePath = paste0(getwd(), "sensitivityAnalysisResults.csv"),
                               numberOfCoresToUse = NULL) {
  sim <- loadSimulation(simFilePath)
  updateSimulationIndividualParameters(simulation = sim, individualParameters)
  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = sim)
  sensitivityAnalysis$addParameterPaths(perturbationParameterNamesVector)

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
runParallelSensitivityAnalysis <- function(numberOfCores,
                                           workingDirectory,
                                           inputFolder,
                                           outputFolder,
                                           simFileName,
                                           SAResultsFileName,
                                           popFileName=NULL,
                                           IndividualId=NULL){

}
