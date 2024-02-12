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
                          showProgress = TRUE,
                          maxSimulationsPerCore = reEnv$defaultMaxSimulationsPerCore) {
      self$numberOfCores <- numberOfCores
      self$showProgress <- showProgress
      self$maxSimulationsPerCore <- maxSimulationsPerCore
      self$mcRepetitions <- getDefaultMCRepetitions()
      self$mcRandomSeed <- getDefaultMCRandomSeed()
    }
  ),
  active = list(
    #' @field numberOfCores is the number of cores to use for simulation
    numberOfCores = function(value) {
      if (missing(value)) {
        return(private$.numberOfCores)
      }
      validateIsInteger(value)
      validateIsOfLength(object = value, nbElements = 1)
      private$.numberOfCores <- value
    },

    #' @field showProgress is a logical field  TRUE shows progress of simulation.
    showProgress = function(value) {
      if (missing(value)) {
        return(private$.showProgress)
      }
      validateIsLogical(value)
      private$.showProgress <- value
    },

    #' @field maxSimulationsPerCore  Scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.
    maxSimulationsPerCore = function(value) {
      if (missing(value)) {
        return(private$.maxSimulationsPerCore)
      }
      validateIsInteger(value)
      validateIsPositive(value)
      private$.maxSimulationsPerCore <- value
    },

    #' @field allowedCores is the number of cores assigned to the user session.
    allowedCores = function(value) {
      if (missing(value)) {
        return(private$.allowedCores)
      }
      validateIsInteger(value)
      validateIsOfLength(object = value, nbElements = 1)
      private$.allowedCores <- value
    },
    
    #' @field mcRepetitions is the number of repetitions when performing a Monte Carlo Simulation
    mcRepetitions = function(value) {
      if (missing(value)) {
        return(private$.mcRepetitions)
      }
      validateIsInteger(value)
      validateIsOfLength(object = value, nbElements = 1)
      private$.mcRepetitions <- value
    },
    
    #' @field mcRandomSeed is the Random Seed Number when performing a Monte Carlo Simulation
    #' which allows repeatability of the simulations
    mcRandomSeed = function(value) {
      if (missing(value)) {
        return(private$.mcRandomSeed)
      }
      validateIsInteger(value)
      validateIsOfLength(object = value, nbElements = 1)
      private$.mcRandomSeed <- value
    }
  ),
  private = list(
    .numberOfCores = NULL,
    .showProgress = NULL,
    .maxSimulationsPerCore = NULL,
    .allowedCores = NULL,
    .mcRepetitions = NULL,
    .mcRandomSeed = NULL
  )
)
