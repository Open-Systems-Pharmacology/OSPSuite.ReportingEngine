#' @title SimulationTask
#' @description  R6 class for SimulationTask settings
#' @field numberOfCores number of cores for parallel computation
#' @field getTaskResults function called by task that computes and format figure results
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    numberOfCores = NULL,
    showProgress = NULL,
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `SimulationTask` object
    #' @param numberOfCores number of cores for parallel computation
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SimulationTask` object
    initialize = function(numberOfCores = defaultSimulationNumberOfCores,
                          showProgress = FALSE,
                          getTaskResults = NULL,
                          ...) {
      super$initialize(...)
      self$getTaskResults <- getTaskResults
      self$settings <- PopulationSimulationSettings$new()
      self$settings$updateNumberOfCores(numberOfCores)
      self$settings$updateShowProgress(showProgress)
    },

    #' #' @description
    #' #' Update the `numberOfCores`
    #' #' @param numberOfCores is the number of cores to use for simulation
    #' updateNumberOfCores = function(numberOfCores) {
    #'   if (!is.null(numberOfCores)) {
    #'     validateIsInteger(numberOfCores)
    #'     validateIsOfLength(object = numberOfCores, nbElements = 1)
    #'     self$settings$numberOfCores <- numberOfCores
    #'   }
    #' },



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
            logFolder = self$workflowFolder)

          self$saveResults(
            set,
            taskResults
          )
        }
      }
    }
  )
)
