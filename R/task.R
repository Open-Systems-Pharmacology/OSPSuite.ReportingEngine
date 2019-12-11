#' @title Task
#' @docType class
#' @description  Task settings for Reporting Engine 
#' @field active logical
#' @field input character
#' @field output character
#' @field settings class
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize Tasks settings}
#' }
#' @format NULL
Task <- R6::R6Class(
  "Tasks",
  public = list(
    active = NULL,
    input = NULL,
    output = NULL,
    settings = NULL,
    
    initialize = function(input = NULL,
                          output = NULL, 
                          settings = NULL,
                          active = TRUE){
      validateIsOfType(active, "logical")
      self$active <- active
      self$input <- input
      self$output <- output
      self$settings <- settings
      
    },
    activate = function(){
      self$active <- TRUE
    },
    inactivate = function(){
      self$active <- FALSE
    }
  )
)