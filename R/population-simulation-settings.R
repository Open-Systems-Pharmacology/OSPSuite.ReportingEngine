#' @title PopulationSimulationSettings
#' @description  R6 class for Population Simulation Settings
#' @field numberOfCores number of cores for parallel computation
#' @field showProgress
PopulationSimulationSettings <- R6::R6Class(
  "PopulationSimulationSettings",
  public = list(

    #' @description
    #' Create a `PopulationSimulationSettings` object
    #' @param numberOfCores number of cores for parallel computation
    #' @param showProgress option to print progress of simulation
    #' @return A new `PopulationSimulationSettings` object
    initialize = function(numberOfCores = defaultSimulationNumberOfCores,
                          showProgress = FALSE) {
      self$numberOfCores <- numberOfCores
      self$showProgress <- showProgress
    }
  ),

  active = list(

    #' @description
    #' Update the `numberOfCores`
    #' @param value is the number of cores to use for simulation
    numberOfCores = function(value) {
      if (missing(value)) {
        private$.numberOfCores
      } else {
        if (!is.null(value)) {
          validateIsInteger(value)
          validateIsOfLength(object = value, nbElements = 1)
          private$.numberOfCores <- value
        }
      }
    },

    #' @description
    #' Update `showProgress`
    #' @param value is a logical input.  TRUE shows progress of simulation.
    showProgress = function(value) {
      if (missing(value)) {
        private$.showProgress
      } else {
        if (!is.null(value)) {
          validateIsLogical(value)
          private$.showProgress <- value
        }
      }
    }
  ),

  private = list(
    .numberOfCores = NULL,
    .showProgress = NULL
  )
)
