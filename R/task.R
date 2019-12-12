#' @title Task
#' @docType class
#' @description  Task settings for Reporting Engine 
#' @field active logical
#' @field input character
#' @field output character
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
                          message = NULL){
      validateIsOfType(active, "logical")
      self$active <- active
      self$input <- input
      self$output <- output
      self$settings <- settings
      self$message <- message
      
    },
    activate = function(){
      self$active <- TRUE
    },
    inactivate = function(){
      self$active <- FALSE
    },
    
    print = function(){
      print(paste0("Task: ", self$message))
      print(paste0("Task to be performed in workflow: ", self$active))
      print(paste0("Task already performed in workflow: ", !is.null(self$output)))
      invisible(self)
    }
  )
)