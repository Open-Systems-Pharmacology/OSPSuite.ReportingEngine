#' @title CalculatePKParametersTask
#' @description  R6 class for defining how pk parameters are calculated and save
#' @keywords internal
CalculatePKParametersTask <- R6::R6Class(
  "CalculatePKParametersTask",
  inherit = SimulationTask,
  public = list(
    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of results from task run.
    saveResults = function(structureSet, taskResults) {
      ospsuite::exportPKAnalysesToCSV(
        taskResults,
        structureSet$pkAnalysisResultsFileNames
      )
      re.tStoreFileMetadata(access = "write", filePath = structureSet$pkAnalysisResultsFileNames)
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
          message = paste0("Calculate PK parameters for simulation set ", set$simulationSet$simulationSetName),
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

          logWorkflow(
            message = "PK parameter calculation completed.",
            pathFolder = self$workflowFolder
          )
        }
        clearMemory(clearSimulationsCache = TRUE)
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
