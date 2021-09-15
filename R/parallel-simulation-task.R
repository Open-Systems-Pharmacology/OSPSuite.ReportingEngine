#' @title ParallelSimulationTask
#' @description  R6 class for ParallelSimulationTask settings
ParallelSimulationTask <- R6::R6Class(
  "ParallelSimulationTask",
  inherit = SimulationTask,
  public = list(

    #' @description
    #' Create a `ParallelSimulationTask` object
    #' @param ... parameters inherited from R6 class `SimulationTask` object
    #' @return A new `SimulationTask` object
    initialize = function(...) {
      super$initialize(...)
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

      sapply(structureSets,function(set){self$validateStructureSetInput(set)})

      taskResults <- self$getTaskResults(
        structureSets = structureSets,
        settings = self$settings,
        logFolder = self$workflowFolder
      )

      for (setNumber in seq_along(structureSets)){
        if(is.null(taskResults[[setNumber]])){
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
