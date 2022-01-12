#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
#' @field getTaskResults function called by task that computes and format figure results
#' @field settings instance of SensitivityAnalysisSettings class
#' @field nameTaskResults name of function that returns task results
#' @importFrom ospsuite.utils %||%
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
    #' @param settings `SensitivityAnalysisSettings` object
    #' @param nameTaskResults name of function that returns task results
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                          settings = NULL,
                          nameTaskResults = "none",
                          ...) {
      validateIsOfType(settings, "SensitivityAnalysisSettings", nullAllowed = TRUE)
      super$initialize(...)
      self$settings <- settings %||% SensitivityAnalysisSettings$new()
      self$getTaskResults <- getTaskResults
      self$nameTaskResults <- nameTaskResults
    },

    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of results from task run.
    saveResults = function(structureSet, taskResults) {
      ospsuite::exportSensitivityAnalysisResultsToCSV(
        results = taskResults,
        filePath = structureSet$sensitivityAnalysisResultsFileNames
      )
      re.tStoreFileMetadata(access = "write", filePath = structureSet$sensitivityAnalysisResultsFileNames)
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
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
          message = paste0("Run sensitivity for simulation set: ", set$simulationSet$simulationSetName),
          pathFolder = self$workflowFolder
        )
        if (self$validateStructureSetInput(set)) {
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
