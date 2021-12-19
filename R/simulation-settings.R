#' @title SimulationSettings
#' @description  R6 class for Population Simulation Settings
#' @keywords internal
SimulationSettings <- R6::R6Class(
  "SimulationSettings",
  public = list(

    #' @description
    #' Create a `SimulationSettings` object
    #' @param numberOfCores number of cores for parallel computation
    #' @param showProgress simulation progress printed to console if TRUE
    #' @return A new `SimulationSettings` object
    initialize = function(numberOfCores = defaultSimulationNumberOfCores,
                          showProgress = FALSE) {
      self$numberOfCores <- numberOfCores
      self$showProgress <- showProgress
    }
  ),

  active = list(
    #' @field numberOfCores is the number of cores to use for simulation
    numberOfCores = function(value) {
      if (missing(value)) {
        private$.numberOfCores
      } else {
        if (!is.null(value)) {
          ospsuite.utils::validateIsInteger(value)
          ospsuite.utils::validateIsOfLength(object = value, nbElements = 1)
          private$.numberOfCores <- value
        }
      }
    },

    #' @field showProgress is a logical field  TRUE shows progress of simulation.
    showProgress = function(value) {
      if (missing(value)) {
        private$.showProgress
      } else {
        if (!is.null(value)) {
          ospsuite.utils::validateIsLogical(value)
          private$.showProgress <- value
        }
      }
    },

    #' @field allowedCores is the number of cores assigned to the user session.
    allowedCores = function(value) {
      if (missing(value)) {
        private$.allowedCores
      } else {
        if (!is.null(value)) {
          ospsuite.utils::validateIsInteger(value)
          ospsuite.utils::validateIsOfLength(object = value, nbElements = 1)
          private$.allowedCores <- value
        }
      }
    }
  ),

  private = list(
    .numberOfCores = NULL,
    .showProgress = NULL,
    .allowedCores = NULL
  )
)
