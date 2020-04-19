#' @title CalculatePKParametersTask
#' @description  R6 class for CalculatePKParametersTask settings
#' @field simulationFilePath simulation folder
#' @field simulationResultFilePaths simulation files
#' @field pkParametersToEvaluate list of PK parameters to evaluate
#' @field userDefinedPKFunctions list of user defined functions to calculate PK parameters
#' @field pkParameterResultsFilePath files where PK parameters are saved
#' @field generatedResultFileNames name of files where PK parameters are saved
CalculatePKParametersTask <- R6::R6Class(
  "CalculatePKParametersTask",
  inherit = SimulationTask,
  public = list(
    simulationFilePath = NULL,
    simulationResultFilePaths = NULL,
    pkParametersToEvaluate = NULL,
    userDefinedPKFunctions = NULL,
    pkParameterResultsFilePath = NULL,
    generatedResultFileNames = NULL,

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    saveResults = function(set,
                           taskResults) {
      ospsuite::exportPKAnalysesToCSV(taskResults,
                                      set$pkAnalysisResultsFileNames
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
