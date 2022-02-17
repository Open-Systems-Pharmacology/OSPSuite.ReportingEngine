#' @title SimulationTask
#' @description  R6 class for SimulationTask settings
#' @field getTaskResults function called by task that computes and format figure results
#' @field settings instance of SimulationSettings class
#' @field nameTaskResults name of function that returns task results
#' @keywords internal
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    getTaskResults = NULL,
    settings = NULL,
    nameTaskResults = "none",

    #' @description
    #' Create a `SimulationTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings instance of SimulationSettings class
    #' @param nameTaskResults name of function that returns task results
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SimulationTask` object
    initialize = function(getTaskResults = NULL,
                              settings = NULL,
                              nameTaskResults = "none",
                              ...) {
      super$initialize(...)
      if (is.null(settings)) {
        self$settings <- SimulationSettings$new()
      } else {
        validateIsOfType(object = settings, SimulationSettings)
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
      ospsuite::exportResultsToCSV(
        taskResults,
        set$simulationResultFileNames
      )
      re.tStoreFileMetadata(access = "write", filePath = set$simulationResultFileNames)
    },

    #' @description
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "Simulation", actionNameExtension = self$nameTaskResults)
      logWorkflow(
        message = paste0("Starting ", self$message),
        pathFolder = self$workflowFolder
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder))
      }

      sapply(structureSets, function(set) {
        self$validateStructureSetInput(set)
      })

      taskResults <- self$getTaskResults(
        structureSets = structureSets,
        settings = self$settings,
        logFolder = self$workflowFolder
      )

      for (setNumber in seq_along(structureSets)) {
        if (is.null(taskResults[[setNumber]])) {
          next
        }
        self$saveResults(
          structureSets[[setNumber]],
          taskResults[[setNumber]]
        )
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
