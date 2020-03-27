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
    initialize = function(numberOfCores = defaultSimulationNumberOfCores,
                          showProgress = FALSE) {
      self$updateNumberOfCores(numberOfCores)
      self$updateShowProgress(showProgress)
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
    },

    #' @description
    #' Update `showProgress`
    #' @param showProgress is a logical input.  TRUE shows progress of simulation.
    updateShowProgress = function(showProgress) {
      if (!is.null(showProgress)) {
        validateIsLogical(showProgress)
        self$showProgress <- showProgress
      }
    }

  )
)
