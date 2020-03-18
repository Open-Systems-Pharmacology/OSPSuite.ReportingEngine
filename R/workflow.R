#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field simulationStructures `SimulationStructure` R6 class object managing the structure of the workflow output
#' @field workflowFolder path of the folder create by the Workflow
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
    resultsFolder = NULL,
    reportFileName = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the folder create by the Workflow
    #' @param resultsFolderName name of folder where results are saved
    #' @param reportName name of the report. Report output includes Rmd, md and html versions.
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                              workflowFolder = NULL,
                              resultsFolderName = defaultFileNames$resultsFolder(),
                              reportName = defaultFileNames$reportName()) {
      self$workflowFolder <- workflowFolder %||% defaultFileNames$workflowFolderPath()

      workflowFolderCheck <- checkExisitingPath(self$workflowFolder, stopIfPathExists = FALSE)

      # If default workflowFolder is not valid, log outcome in getwd()
      # But can we carry on if !is.null(workflowFolderCheck) is true?  Should workflow folder be set to getwd() if so?
      if (!is.null(workflowFolderCheck)) {
        logWorkflow(
          message = workflowFolderCheck,
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )
        logWorkflow(
          message = messages$warningPathIncludes(self$workflowFolder),
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )
      }

      dir.create(self$workflowFolder, showWarnings = FALSE)

      logWorkflow(
        message = self$reportingEngineInfo$print(),
        pathFolder = self$workflowFolder
      )

      self$reportFileName <- file.path(self$workflowFolder, paste0(reportName, ".Rmd"))

      resultsFolder <- file.path(self$workflowFolder, resultsFolderName)
      resultsFolderCheck <- checkExisitingPath(resultsFolder, stopIfPathExists = FALSE)
      if (!is.null(resultsFolderCheck)) {
        logWorkflow(
          message = resultsFolderCheck,
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )

        logWorkflow(
          message = messages$warningPathIncludes(resultsFolder),
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug, LogTypes$Error)
        )
      }
      self$resultsFolder <- resultsFolder
      dir.create(self$resultsFolder, showWarnings = FALSE)

      self$simulationStructures <- list()
      # Check of Workflow inputs
      for (simulationSetIndex in 1:length(simulationSets)) {
        self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
          simulationSet = simulationSets[[simulationSetIndex]],
          workflowResultsFolder = self$resultsFolder,
          workflowFolder = self$workflowFolder
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
    #' Get a vector with all the names of the plot tasks within the `Workflow`
    #' @return Vector of `PlotTask` names
    getAllPlotTasks = function() {
      # get isTaskVector as a named vector
      isPlotTaskVector <- unlist(eapply(self, function(x) {
        isOfType(x, "PlotTask")
      }))

      taskNames <- names(isPlotTaskVector[as.logical(isPlotTaskVector)])

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
    activateTasks = function(tasks = self$getAllTasks()) {
      activateWorkflowTasks(self, tasks = tasks)
    },

    #' @description
    #' Inactivates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the worklfow tasks to inactivate.
    #' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    inactivateTasks = function(tasks = self$getAllTasks()) {
      inactivateWorkflowTasks(self, tasks = tasks)
    }
  ),

  private = list(
    getTasksWithStatus = function(status) {
      taskNames <- self$getAllTasks()

      tasksWithStatus <- NULL
      for (taskName in taskNames) {
        if (self[[taskName]]$active == status) {
          tasksWithStatus <- c(tasksWithStatus, taskName)
        }
      }
      return(tasksWithStatus)
    }
  )
)
