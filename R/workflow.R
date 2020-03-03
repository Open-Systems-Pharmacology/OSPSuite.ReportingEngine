#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field simulationSets list of `MeanModelSet` R6 class objects
#' @field observedData list of observed `data` and `metaData`
#' @field resultsFolder path where results are saved
#' @field reportFileName name of the Rmd report file
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    reportingEngineInfo = ReportingEngineInfo$new(),
    simulationStructures = NULL,
    workflowFolder = NULL,
    observedData = NULL,
    resultsFolder = NULL,
    reportFileName = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets names of pkml files to be used for simulations
    #' @param observedDataFile name of csv file to be used for observations
    #' @param observedMetaDataFile name of csv file to be used as dictionary for observed data
    #' @param resultsFolderName name of folder where results are saved
    #' @param reportName name of the report, Rmd, md and html versions will be created
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                              workflowFolder = file.path(getwd(), defaultFileNames$workflowFolder()),
                              resultsFolderName = defaultFileNames$resultsFolder(),
                              reportName = defaultFileNames$reportName()) {
      workflowFolderCheck <- checkExisitingPath(workflowFolder, stopIfPathExists = TRUE)
      if (!is.null(workflowFolderCheck)) {
        logWorkflow(
          message = workflowFolderCheck,
          pathFolder = getwd(),
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )
      }
      self$workflowFolder <- workflowFolder
      dir.create(self$workflowFolder)

      logWorkflow(
        message = self$reportingEngineInfo$print(),
        pathFolder = self$workflowFolder
      )

      self$reportFileName <- file.path(self$workflowFolder, paste0(reportName, ".Rmd"))

      resultsFolder <- file.path(workflowFolder, resultsFolderName)
      resultsFolderCheck <- checkExisitingPath(resultsFolder, stopIfPathExists = TRUE)
      if (!is.null(resultsFolderCheck)) {
        logWorkflow(
          message = resultsFolderCheck,
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )
      }
      self$resultsFolder <- resultsFolder
      dir.create(self$resultsFolder)

      self$simulationStructures <- list()
      # Check of Workflow inputs
      for (simulationSetIndex in 1:length(simulationSets)) {
        self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
          simulationSet = simulationSets[[simulationSetIndex]],
          workflowResultsFolder = self$resultsFolder
        )
      }
    },

    #' @description
    #' Get a vector with all the names of the tasks within the `Workflow`
    #' @return Vector of `Task` names
    getAllTasks = function() {
      # get isTaskVector as a named vector
      isTaskVector <- unlist(eapply(self, function(x) {
        isOfType(x, "Task")
      }))

      taskNames <- names(isTaskVector[as.logical(isTaskVector)])

      return(taskNames)
    },

    #' @description
    #' Get a vector with all the names of active tasks within the `Workflow`
    #' @return Vector of active `Task` names
    getActiveTasks = function() {
      return(private$getTasksWithStatus(status = TRUE))
      },

    #' @description
    #' Get a vector with all the names of inactive tasks within the `Workflow`
    #' @return Vector of inactive `Task` names
    getInactiveTasks = function() {
      return(private$getTasksWithStatus(status = FALSE))
    },
    
    #' @description
    #' Activates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the worklfow tasks to activate.
    #' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    activateTasks = function(tasks = self$getAllTasks()){
      activateWorkflowTasks(self, tasks = tasks)
    },
    
    #' @description
    #' Inactivates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the worklfow tasks to inactivate.
    #' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    inactivateTasks = function(tasks = self$getAllTasks()){
      inactivateWorkflowTasks(self, tasks = tasks)
    }
  ),
  
  private = list(
    #' @description
    #' Private method to get `Tasks` with a specific status
    #' @param status logical for `active` field of workflow tasks
    #' @return Vector of `Task` names with status
    getTasksWithStatus = function(status){
      taskNames <- self$getAllTasks()
      
      tasksWithStatus <- NULL
      for (taskName in taskNames) {
        if (self[[taskName]]$active==status) {
          tasksWithStatus <- c(tasksWithStatus, taskName)
        }
      }
      return(tasksWithStatus)
    }
  )
)
