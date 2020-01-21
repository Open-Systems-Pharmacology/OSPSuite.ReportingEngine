#' @title Task
#' @docType class
#' @description  Task settings for Reporting Engine
#' @field active logical
#' @field input List of input files/folders to use to perform tasks
#' @field output List of output files/folders to save the task output
#' @field settings class
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize Task settings}
#' \item{activate()}{Set Task as active in workflow}
#' \item{inactivate()}{Set Task as inactive in workflow}
#' \item{print()}{Show task settings}
#' }
#' @format NULL
Task <- R6::R6Class(
  "Tasks",
  public = list(
    active = NULL,
    input = NULL,
    output = NULL,
    settings = NULL,
    message = NULL,

    initialize = function(input = NULL,
                              output = NULL,
                              settings = NULL,
                              active = TRUE,
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
