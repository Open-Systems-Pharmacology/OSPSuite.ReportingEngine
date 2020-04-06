#' @title Task
#' @description  R6 class for Task settings
#' @field active logical indicating if `Task` is performed in a worklfow.
#' @field outputFolder List of output files/folders to save the task output
#' @field workflowFolder folder where workflow is run and saved
#' @field settings list of settings for task such as plot configurations
#' @field message message or title of the task
Task <- R6::R6Class(
  "Task",
  public = list(
    active = NULL,
    outputFolder = NULL,
    workflowFolder = NULL,
    settings = NULL,
    message = NULL,

    #' @description
    #' Create a `Task` object
    #' @param outputFolder task output folder to save results
    #' @param workflowFolder folder where workflow is run and saved
    #' @param settings specific settings for task (e.g. plot configurations)
    #' @param active logical indicating if `Task` is performed in a worklfow.
    #' Default value is `FALSE`
    #' @param message message of the `Task`.
    #' @return A new `Task` object
    initialize = function(outputFolder = NULL,
                          workflowFolder = getwd(),
                          settings = NULL,
                          active = FALSE,
                          message = NULL) {
      validateIsOfType(active, "logical")
      self$active <- active
      self$outputFolder <- outputFolder
      self$workflowFolder <- workflowFolder
      self$settings <- settings
      self$message <- message
    },

    #' @description
    #' Activate `Task`
    activate = function() {
      self$active <- TRUE
    },
    #' @description
    #' Inactivate `Task`
    inactivate = function() {
      self$active <- FALSE
    },
    #' @description
    #' Check if `Task` inputs exist
    #' @param inputsToCheck list of input files to check
    #' @return logical indicating if input is valid
    validateInput = function(inputsToCheck = NULL) {
      isValid <- TRUE
      for (inputToCheck in inputsToCheck) {
        if (!file.exists(inputToCheck)) {
          isValid <- FALSE
          warning(messages$errorTaskInputDoesNotExist(inputToCheck))
        }
      }
      return(isValid)
    },

    #' @description
    #' Print `Task` features
    #' @return Text of task features
    print = function() {
      taskActiveMessage <- "NOT"
      if (self$active) {
        taskActiveMessage <- ""
      }

      info <- c(
        sprintf("Task: %s", self$message %||% ""),
        sprintf("Task is %s active ", taskActiveMessage),
        sprintf("Workflow folder of task: %s", self$workflowFolder %||% ""),
        sprintf("Task output folder: %s", self$outputFolder %||% "")
      )

      invisible(self)
      return(info)
    }
  )
)

#' @title activateWorkflowTasks
#' @description activates a series of `Tasks` from a `Workflow`
#' @param workflow `MeanModelWorklfow` or `PopulationWorklfow` object
#' @param tasks names of the tasks to activate
#' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
activateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")

  for (task in tasks) {
    workflow[[task]]$activate()
  }
}

#' @title inactivateWorkflowTasks
#' @description inactivates a series of `Tasks` from a `Workflow`
#' @param workflow `MeanModelWorklfow` or `PopulationWorklfow` object
#' @param tasks names of the tasks to activate
#' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
inactivateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")

  for (task in tasks) {
    workflow[[task]]$inactivate()
  }
}