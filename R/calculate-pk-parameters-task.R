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
      logDebug(paste0(
        "PK parameters for '", structureSet$simulationSet$simulationSetName,
        "' saved in '", structureSet$pkAnalysisResultsFileNames
      ))
      re.tStoreFileMetadata(access = "write", filePath = structureSet$pkAnalysisResultsFileNames)
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "Analysis", actionNameExtension = self$nameTaskResults)
      logInfo(messages$runStarting(self$message))
      t0 <- tic()

      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE, recursive = TRUE)
      }

      for (set in structureSets) {
        logInfo(messages$runStarting(self$message, set$simulationSet$simulationSetName))
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(
            structureSet = set,
            settings = self$settings
          )

          self$saveResults(set, taskResults)
        }
        clearMemory(clearSimulationsCache = TRUE)
      }
      re.tEndAction(actionToken = actionToken)
      logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
    }
  )
)
