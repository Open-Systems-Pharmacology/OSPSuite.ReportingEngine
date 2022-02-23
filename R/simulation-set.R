#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field outputs list of `Output` R6 class objects
#' @field observedDataFile name of csv file to be used for observed data
#' @field observedMetaDataFile name of csv file to be used as dictionary of the observed data
#' @field timeUnit display unit for time variable
#' @field applicationRanges named list of logicals defining which Application ranges are included in
#' @field minimumSimulationEndTime is the minimum length of time for which a simulation must be run
#' @field timeOffset shift of display time in time profile plots
#' reported time profiles and residual plots when applicable
#' @export
#' @import ospsuite.utils
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
    minimumSimulationEndTime = NULL,
    timeOffset = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param outputs list of `Output` R6 class objects
    #' @param observedDataFile name of csv file to be used for observed data
    #' @param observedMetaDataFile name of csv file to be used as dictionary of the observed data
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @param applicationRanges names of application ranges to include in the report. Names are available in enum `ApplicationRanges`.
    #' @param minimumSimulationEndTime is the minimum length of time for which a simulation must be run
    #' @param timeOffset shift of display time in time profile plots
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName,
                              simulationFile,
                              outputs = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              timeUnit = "h",
                              applicationRanges = ApplicationRanges,
                              minimumSimulationEndTime = NULL,
                              timeOffset = 0) {
      # Test and validate the simulation objects
      validateIsString(simulationSetName)
      validateIsString(simulationFile)
      validateIsFileExtension(simulationFile, "pkml")
      validateIsString(c(observedDataFile, observedMetaDataFile, timeUnit), nullAllowed = TRUE)
      validateIsPositive(object = minimumSimulationEndTime, nullAllowed = TRUE)
      validateIsNumeric(timeOffset)
      # For optional input, usually null is allowed
      # but not here as it would mean that nothing would be reported
      validateIsIncluded(c(applicationRanges), ApplicationRanges)

      simulation <- ospsuite::loadSimulation(simulationFile,addToCache = FALSE)

      # Test and validate outputs and their paths
      validateOutputObject(c(outputs), simulation, nullAllowed = TRUE)

      if (!is.null(observedDataFile)) {
        validateObservedMetaDataFile(observedMetaDataFile, observedDataFile, c(outputs))
      }

      self$simulationSetName <- simulationSetName
      self$simulationFile <- simulationFile
      self$minimumSimulationEndTime <- minimumSimulationEndTime
      self$timeOffset <- timeOffset

      self$outputs <- c(outputs)

      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile

      self$timeUnit <- timeUnit %||% "h"

      self$applicationRanges <- list(
        total = isIncluded(ApplicationRanges$total, applicationRanges),
        firstApplication = isIncluded(ApplicationRanges$firstApplication, applicationRanges),
        lastApplication = isIncluded(ApplicationRanges$lastApplication, applicationRanges)
      )
    }
  )
)
