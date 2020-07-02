#' @title CalculatePKParametersTask
#' @description  R6 class for CalculatePKParametersTask settings
CalculatePKParametersTask <- R6::R6Class(
  "CalculatePKParametersTask",
  inherit = SimulationTask,
  public = list(


    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    saveResults = function(set,
                           taskResults) {
      ospsuite::exportPKAnalysesToCSV(
        taskResults,
        set$pkAnalysisResultsFileNames
      )
      re.tStoreFileMetadata(access = "write", filePath = set$pkAnalysisResultsFileNames)
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
          message = paste0("Calculate PK parameters for simulation set ", set$simulationSet$simulationSetName),
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


          logWorkflow(
            message = "PK parameter calculation completed.",
            pathFolder = self$workflowFolder
          )
        }
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
