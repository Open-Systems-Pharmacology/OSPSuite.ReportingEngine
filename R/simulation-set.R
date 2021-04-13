#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field outputs list of `Output` R6 class objects
#' @field observedDataFile name of csv file to be used for observed data
#' @field observedMetaDataFile name of csv file to be used as dictionary of the observed data
#' @field timeUnit display unit for time variable
#' @field applicationRanges named list of logicals defining which Application ranges are included in
#' time profiles and residual plots when applicable
#' @export
SimulationSet <- R6::R6Class(
  "SimulationSet",
  public = list(
    simulationSetName = NULL,
    simulationFile = NULL,
    outputs = NULL,
    observedDataFile = NULL,
    observedMetaDataFile = NULL,
    timeUnit = NULL,
    applicationRanges = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param outputs list of `Output` R6 class objects
    #' @param observedDataFile name of csv file to be used for observed data
    #' @param observedMetaDataFile name of csv file to be used as dictionary of the observed data
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @param applicationRanges named list of logicals defining which application ranges are included in
    #' time profiles and residual plots when applicable. Names are available in enum `ApplicationRanges`.
    #' Providing directly a logical value will associate the value to all application ranges.
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName,
                              simulationFile,
                              outputs = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              timeUnit = "h",
                              applicationRanges = TRUE) {
      # Test and validate the simulation object
      validateIsString(simulationSetName)
      validateIsString(simulationFile)
      validateIsLogical(c(applicationRanges))
      # Is of type does not work for this test because values in list are logicals
      if (!is.logical(applicationRanges)) {
        validateIsIncluded(ApplicationRanges, names(applicationRanges), groupName = "'applicationRanges' variables")
      }
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

      self$outputs <- c(outputs)

      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile

      self$timeUnit <- timeUnit %||% "h"

      self$applicationRanges <- applicationRanges
      if (is.logical(applicationRanges)) {
        self$applicationRanges <- list(
          total = applicationRanges,
          firstApplication = applicationRanges,
          lastApplication = applicationRanges
        )
      }
    }
  )
)
