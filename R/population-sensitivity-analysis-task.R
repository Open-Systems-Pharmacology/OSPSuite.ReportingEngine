#' @title PopulationSensitivityAnalysisTask
#' @description  R6 class for PopulationSensitivityAnalysisTask settings
#' @field getTaskResults function called by task that computes and format figure results
#' @field settings instance of SensitivityAnalysisSettings class
PopulationSensitivityAnalysisTask <- R6::R6Class(
  "PopulationSensitivityAnalysisTask",
  inherit = SensitivityAnalysisTask,
  public = list(
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `PopulationSensitivityAnalysisTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings `SensitivityAnalysisSettings` object
    #' @param ... parameters inherited from R6 class `SensitivityAnalysisTask` object
    #' @return A new `PopulationSensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                              settings = NULL,
                              ...) {
      validateIsOfType(settings, "SensitivityAnalysisSettings", nullAllowed = TRUE)
      super$initialize(...)
      self$settings <- settings %||% SensitivityAnalysisSettings$new()
      self$getTaskResults <- getTaskResults
    },

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    saveResults = function(set, taskResults) {
      results <- taskResults$populationSensitivityResults
      indexDataFrame <- taskResults$indexDataFrame
      indexFileName <- taskResults$indexFileName
      for (fileName in names(results)) {
        filePath <- file.path(self$workflowFolder, self$outputFolder, fileName)
        ospsuite::exportSensitivityAnalysisResultsToCSV(
          results = results[[fileName]],
          filePath = filePath
        )
        re.tStoreFileMetadata(access = "write", filePath = filePath)
      }
      indexFilePath <- file.path(self$workflowFolder, self$outputFolder, indexFileName)
      write.csv(x = indexDataFrame, file = indexFilePath, row.names = FALSE)
      re.tStoreFileMetadata(access = "write", filePath = indexFilePath)
    }
  )
)
