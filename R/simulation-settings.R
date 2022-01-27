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
    #' @param maxSimulationsPerCore Scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.
    #' @return A new `SimulationSettings` object
    initialize = function(numberOfCores = defaultSimulationNumberOfCores,
                          showProgress = FALSE,
                          maxSimulationsPerCore = defaultMaxSimulationsPerCore) {
      self$numberOfCores <- numberOfCores
      self$showProgress <- showProgress
      self$maxSimulationsPerCore <- maxSimulationsPerCore
    }
  ),

  active = list(
    #' @field numberOfCores is the number of cores to use for simulation
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

    #' @field showProgress is a logical field  TRUE shows progress of simulation.
    showProgress = function(value) {
      if (missing(value)) {
        private$.showProgress
      } else {
        if (!is.null(value)) {
          validateIsLogical(value)
          private$.showProgress <- value
        }
      }
    },

    #' @field maxSimulationsPerCore  Scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.
    maxSimulationsPerCore = function(value) {
      if (missing(value)) {
        private$.maxSimulationsPerCore
      } else {
        if (!is.null(value)) {
          validateIsInteger(value)
          validateIsPositive(value)
          private$.maxSimulationsPerCore <- value
        }
      }
    },

    #' @field allowedCores is the number of cores assigned to the user session.
    allowedCores = function(value) {
      if (missing(value)) {
        private$.allowedCores
      } else {
        if (!is.null(value)) {
          validateIsInteger(value)
          validateIsOfLength(object = value, nbElements = 1)
          private$.allowedCores <- value
        }
      }
    }
  ),

  private = list(
    .numberOfCores = NULL,
    .showProgress = NULL,
    .maxSimulationsPerCore = NULL,
    .allowedCores = NULL
  )
)
