#' @title Task
#' @description  R6 class for Task settings
#' @field active logical
#' @field input List of input files/folders to use to perform tasks
#' @field output List of output files/folders to save the task output
#' @field settings class
#' @field message message or title of the task
Task <- R6::R6Class(
  "Task",
  public = list(
    active = NULL,
    input = NULL,
    output = NULL,
    settings = NULL,
    message = NULL,

    #' @description
    #' Create a `Task` object
    #' @param input list of files or folders of input
    #' @param output list of files or folders of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in a worklfow.
    #' Default value is `FALSE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    initialize = function(input = NULL,
                          output = NULL,
                          settings = NULL,
                          active = FALSE,
                          message = NULL) {
      validateIsOfType(active, "logical")
      self$active <- active
      self$input <- as.list(input)
      self$output <- as.list(output)
      self$settings <- settings
      self$message <- message
    },
    activate = function() {
      self$active <- TRUE
    },
    inactivate = function() {
      self$active <- FALSE
    },
    validateInput = function() {
      inputPaths <- sapply(self$input, identity)
      isValid <- TRUE
      for (inputToCheck in inputPaths) {
        if (!file.exists(inputToCheck)) {
          isValid <- FALSE
          warning(messages$errorTaskInputDoesNotExist(inputToCheck))
        }
      }
      return(isValid)
    },

    #' @description
    #' Print `Task` features
    #' @return List of task features
    print = function() {
      info <- list(
        "task" = self$message,
        "active" = self$active,
        "input" = sapply(self$input, identity),
        "output" = sapply(self$output, identity)
      )

      invisible(self)
      return(info)
    }
  )
)
