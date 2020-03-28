#' @title SimulationTask
#' @description  R6 class for SimulationTask settings
#' @field getTaskResults function called by task that computes and format figure results
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `SimulationTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings instance of SimulationSettings class
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SimulationTask` object
    initialize = function(getTaskResults = NULL,
                          settings = NULL,
                          ...) {
      super$initialize(...)
      if(is.null(settings)){
        self$settings <- SimulationSettings$new()
      } else{
        validateIsOfType(object = settings,SimulationSettings)
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
      ospsuite::exportResultsToCSV(
        taskResults,
        set$simulationResultFileNames
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
          message = paste0("Run simulation: ", set$simulationSet$simulationName),
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
