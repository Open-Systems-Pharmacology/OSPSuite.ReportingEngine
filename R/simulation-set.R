#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field simulationName display name of simulation
#' @field outputs list of `Output` R6 class objects
#' @field observedDataFile name of csv file to be used for observed data
#' @field observedMetaDataFile name of csv file to be used as dictionary of the observed data
#' @field timeUnit display unit for time variable
#' @export
SimulationSet <- R6::R6Class(
  "SimulationSet",
  public = list(
    simulationSetName = NULL,
    simulationFile = NULL,
    simulationName = NULL,
    outputs = NULL,
    observedDataFile = NULL,
    observedMetaDataFile = NULL,
    timeUnit = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param outputs list of `Output` R6 class objects
    #' @param observedDataFile name of csv file to be used for observed data
    #' @param observedMetaDataFile name of csv file to be used as dictionary of the observed data
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName,
                              simulationFile,
                              simulationName = NULL,
                              outputs = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              timeUnit = "h") {
      # Test and validate the simulation object
      validateIsString(simulationSetName)
      validateIsString(simulationFile)
      validateIsFileExtension(simulationFile, "pkml")
      simulation <- ospsuite::loadSimulation(simulationFile)

      # Test and validate outputs and their paths
      validateOutputObject(c(outputs), simulation, nullAllowed = TRUE)

      # Test and validate observed data
      validateIsString(c(observedDataFile, observedMetaDataFile, timeUnit), nullAllowed = TRUE)
      if (!is.null(observedDataFile)) {
        validateObservedMetaDataFile(observedMetaDataFile, observedDataFile)
      }

      self$simulationSetName <- simulationSetName
      self$simulationFile <- simulationFile
      self$simulationName <- simulationName %||% trimFileName(simulationFile, extension = "pkml")

      self$outputs <- c(outputs)

      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile

      self$timeUnit <- timeUnit %||% "h"
    }
  )
)
