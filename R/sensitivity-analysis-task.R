#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
SensitivityAnalysisTask <- R6::R6Class(
  "SensitivityAnalysisTask",
  inherit = Task,
  public = list(
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `SensitivityAnalysisTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings instance of SensitivityAnalysisSettings class
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
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
    saveResults = function(set,
                           taskResults) {
      ospsuite::exportSensitivityAnalysisResultsToCSV(
        taskResults,
        set$sensitivityAnalysisResultsFileNames
      )
    },

    #' @description
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    runTask = function(structureSets) {
      logWorkflow(
        message = paste0("Starting ", self$message),
        pathFolder = self$workflowFolder
      )

      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder))
      }

      for (set in structureSets) {
        logWorkflow(
          message = paste0("Run sensitivity for simulation: ", set$simulationSet$simulationName),
          pathFolder = self$workflowFolder
        )
        if (self$validateInput()) {
          taskResults <- self$getTaskResults(
            structureSet = set,
            settings = self$settings,
            logFolder = self$workflowFolder
          )

          self$saveResults(
            set,
            taskResults
          )
        }
      }
    }
  )
)
