#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
#' @field getTaskResults function called by task that computes and format figure results
#' @field settings instance of SensitivityAnalysisSettings class
#' @field nameTaskResults name of function that returns task results
SensitivityAnalysisTask <- R6::R6Class(
  "SensitivityAnalysisTask",
  inherit = Task,
  public = list(
    getTaskResults = NULL,
    settings = NULL,
    nameTaskResults = "none",

    #' @description
    #' Create a `SensitivityAnalysisTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings instance of SensitivityAnalysisSettings class
    #' @param nameTaskResults name of function that returns task results
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                              settings = NULL,
                              nameTaskResults = "none",
                              ...) {
      super$initialize(...)
      if (is.null(settings)) {
        self$settings <- SensitivityAnalysisSettings$new()
      } else {
        validateIsOfType(object = settings, SensitivityAnalysisSettings)
        self$settings <- settings
      }
      self$getTaskResults <- getTaskResults
      self$nameTaskResults <- nameTaskResults
    },

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    saveResults = function(set,
                               taskResults) {
      ospsuite::exportSensitivityAnalysisResultsToCSV(
        results = taskResults,
        filePath = set$sensitivityAnalysisResultsFileNames
      )
      re.tStoreFileMetadata(access = "write", filePath = set$sensitivityAnalysisResultsFileNames)
    },

    #' @description
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "Analysis", actionNameExtension = self$nameTaskResults)
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
      re.tEndAction(actionToken = actionToken)
    }
  )
)
