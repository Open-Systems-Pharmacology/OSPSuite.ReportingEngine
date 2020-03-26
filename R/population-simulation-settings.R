#' @title PopulationSimulationSettings
#' @description  R6 class for Population Simulation Settings
#' @field numberOfCores number of cores for parallel computation
#' @field showProgress
PopulationSimulationSettings <- R6::R6Class(
  "PopulationSimulationSettings",
  public = list(
    numberOfCores = NULL,
    showProgress = NULL,

    #' @description
    #' Create a `PopulationSimulationSettings` object
    #' @param numberOfCores number of cores for parallel computation
    #' @param showProgress option to print progress of simulation
    #' @return A new `PopulationSimulationSettings` object
    initialize = function(numberOfCores = NULL,
                          showProgress = NULL) {
      self$updateNumberOfCores(numberOfCores %||% defaultSimulationNumberOfCores)
      self$showProgress <- showProgress
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
