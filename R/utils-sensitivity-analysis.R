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
analyzeSensitivity <- function(simFileName,
                               simFileFolder,
                               paramRange = NULL,
                               pkParameterName = "AUC",
                               totalSensitivityThreshold = 1) {
  sim <- loadSimulation(paste0(simFileFolder, simFileName))
  print("Getting param list...")
  allParameters <- ospsuite::getAllParametersMatching(paths = "**", container = sim)
  print("...done")
  outputSelections <- sim$outputSelections

  if (is.null(paramRange)) {
    parameters <- allParameters
  }
  else {
    parameters <- allParameters[ paramRange ]
  }

  print(paste0("Performing sensitivity analysis for parameters ", paramRange[1], " to ", paramRange[2], "."))

  sensitivityAnalysis <- SensitivityAnalysis$new(
    simulation = sim,
    parameters = parameters
  )
  print("****")
  print(sensitivityAnalysis)
  print("****")
  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)


  print("Running sensitivity analysis...")
  results <- runSensitivityAnalysis(
    sensitivityAnalysis = sensitivityAnalysis,
    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
  )
  print("...done")

  pkSensitivities <- list()
  for (output in outputSelections$allOutputs) {
    pkSensitivities <- results$allPKParameterSensitivitiesFor(
      pkParameterName = "AUC",
      outputPath = output$path,
      totalSensitivityThreshold = totalSensitivityThreshold
    )
  }
  print(pkSensitivities)
}
