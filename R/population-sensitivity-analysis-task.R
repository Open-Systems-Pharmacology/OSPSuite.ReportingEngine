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
    #' @param settings instance of SensitivityAnalysisSettings class
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `PopulationSensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                          settings = NULL,
                          ...) {
      super$initialize(...)
      if (is.null(settings)) {
        self$settings <- SensitivityAnalysisSettings$new()
      } else {
        validateIsOfType(object = settings, SensitivityAnalysisSettings)
        self$settings <- settings
      }
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
        ospsuite::exportSensitivityAnalysisResultsToCSV(
          results = results[[fileName]],
          filePath = file.path(self$workflowFolder, self$outputFolder, fileName)
        )
      }
      write.csv(x = indexDataFrame, file = file.path(self$workflowFolder, self$outputFolder, indexFileName),row.names = FALSE)
    }
  )
)
