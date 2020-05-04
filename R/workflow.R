#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field simulationStructures `SimulationStructure` R6 class object managing the structure of the workflow output
#' @field workflowFolder path of the folder create by the Workflow
#' @field taskNames Enum of task names
#' @field reportFileName name of the Rmd report file
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    simulationStructures = NULL,
    workflowFolder = NULL,
    taskNames = NULL,
    reportFileName = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the output folder created or used by the Workflow.
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                          workflowFolder) {
      
      private$.reportingEngineInfo <- ReportingEngineInfo$new()
      
      validateIsString(workflowFolder)
      validateIsOfType(c(simulationSets), "SimulationSet")
      if(!isOfType(simulationSets, "list")){simulationSets <- list(simulationSets)}
      
      allSimulationSetNames <- sapply(simulationSets,  function(set){set$simulationSetName})
      validateNoDuplicatedEntries(allSimulationSetNames)
      
      self$workflowFolder <- workflowFolder
      workflowFolderCheck <- checkExisitingPath(self$workflowFolder, stopIfPathExists = FALSE)

      if (!is.null(workflowFolderCheck)) {
        logWorkflow(
          message = workflowFolderCheck,
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug)
        )
      }
      dir.create(self$workflowFolder, showWarnings = FALSE)

      logWorkflow(
        message = private$.reportingEngineInfo$print(),
        pathFolder = self$workflowFolder
      )

      self$reportFileName <- file.path(self$workflowFolder, paste0(defaultFileNames$reportName(), ".md"))
      self$taskNames <- enum(self$getAllTasks())

      self$simulationStructures <- list()
      simulationSets <- c(simulationSets)
      for (simulationSetIndex in seq_along(simulationSets)) {
        self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
          simulationSet = simulationSets[[simulationSetIndex]],
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
      return(private$.getTasksWithStatus(status = TRUE))
    },

    #' @description
    #' Get a vector with all the names of inactive tasks within the `Workflow`
    #' @return Vector of inactive `Task` names
    getInactiveTasks = function() {
      return(private$.getTasksWithStatus(status = FALSE))
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
    },

    #' @description
    #' Print reporting engine information obtained from initiliazing a `Workflow`
    printReportingEngineInfo = function() {
      private$.reportingEngineInfo$print()
    },
    
    #' @description
    #' Print workflow list of tasks
    #' @return Task list information
    print = function() {
      tasksInfo <- list()
      for (task in self$getAllTasks()) {
        tasksInfo[[paste0("Task: '", task, "'")]] <- self[[task]]$print()
      }
      
      invisible(self)
      return(tasksInfo)
    }
  ),

  private = list(
    .reportingEngineInfo = NULL,

    .getTasksWithStatus = function(status) {
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
