#' @title SimulationTask
#' @description  R6 class for SimulationTask settings
#' @field numberOfCores number of cores for parallel computation
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    numberOfCores = NULL,

    #' @description
    #' Create a `SimulationTask` object
    #' @param numberOfCores number of cores for parallel computation
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SimulationTask` object
    initialize = function(numberOfCores = NULL,
                              ...) {
      super$initialize(...)
      self$updateNumberOfCores(numberOfCores %||% defaultSimulationNumberOfCores)
    },

    #' @description
    #' Update the `numberOfCores`
    #' @param numberOfCores is the number of cores to use for simulation
    updateNumberOfCores = function(numberOfCores) {
      if (!is.null(numberOfCores)) {
        validateIsInteger(numberOfCores)
        validateIsOfLength(object = numberOfCores, nbElements = 1)
        self$numberOfCores <- numberOfCores
      }
    }
  )
)
