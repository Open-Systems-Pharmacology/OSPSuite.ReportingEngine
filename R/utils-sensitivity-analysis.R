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
                               resultsFilePath = paste0(getwd(),"sensitivityAnalysisResults.csv")){

  sim <- loadSimulation(simFilePath)


  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = sim)
  sensitivityAnalysis$addParameterPaths(perturbationParameterNamesVector)
  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)


  print("Running sensitivity analysis...")
  sensitivityAnalysisResults <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  print("...done")
  exportSensitivityAnalysisResultsToCSV(results = sensitivityAnalysisResults,resultsFilePath)
  # pkSensitivities <- list()
  # for (output in outputSelections$allOutputs) {
  #   pkSensitivities <- results$allPKParameterSensitivitiesFor(
  #     pkParameterName = "AUC",
  #     outputPath = output$path,
  #     totalSensitivityThreshold = totalSensitivityThreshold
  #   )
  # }
  # print(pkSensitivities)
}
