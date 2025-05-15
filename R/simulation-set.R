#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field outputs list of `Output` R6 class objects
#' @field dataSource A `DataSource` object
#' @field dataSelection character or expression used to select a subset of observed data
#' @field timeUnit display unit for time variable
#' @field applicationRanges named list of logicals defining which Application ranges are included in
#' @field minimumSimulationEndTime is the minimum length of time for which a simulation must be run
#' @field timeOffset shift of display time in time profile plots
#' @field massBalanceSettings List of mass balance settings
#' reported time profiles and residual plots when applicable
#' @export
#' @import ospsuite.utils
SimulationSet <- R6::R6Class(
  "SimulationSet",
  public = list(
    simulationSetName = NULL,
    simulationFile = NULL,
    outputs = NULL,
    dataSource = NULL,
    dataSelection = NULL,
    timeUnit = NULL,
    applicationRanges = NULL,
    minimumSimulationEndTime = NULL,
    timeOffset = NULL,
    massBalanceSettings = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param outputs list of `Output` R6 class objects
    #' @param dataSource A `DataSource` object
    #' @param dataSelection characters or expression to select subset the observed data
    #' By default, all the data is selected.
    #' When using a character array, selections are concatenated with the `&` sign
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @param applicationRanges names of application ranges to include in the report. Names are available in enum `ApplicationRanges`.
    #' @param minimumSimulationEndTime is the minimum length of time for which a simulation must be run
    #' @param timeOffset shift of display time in time profile plots
    #' @param massBalanceFile List of mass balance settings provided either as a json file
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName,
                          simulationFile,
                          outputs = NULL,
                          dataSource = NULL,
                          dataSelection = DataSelectionKeys$ALL,
                          timeUnit = "h",
                          applicationRanges = ApplicationRanges,
                          minimumSimulationEndTime = NULL,
                          timeOffset = 0,
                          massBalanceFile = NULL) {
      # Test and validate the simulation objects
      validateIsString(simulationSetName)
      validateIsString(simulationFile)
      validateIsFileExtension(simulationFile, "pkml")
      validateIsUnitFromDimension(timeUnit, "Time")

      # For optional input, usually null is allowed
      # but not here as it would mean that nothing would be reported
      validateIsIncluded(c(applicationRanges), ApplicationRanges)
      validateIsString(massBalanceFile, nullAllowed = TRUE)
      if (!isEmpty(massBalanceFile)) {
        validateIsFileExtension(massBalanceFile, "json")
        self$massBalanceSettings <- jsonlite::fromJSON(massBalanceFile, simplifyVector = FALSE)[["MassBalancePlots"]]
      }

      # Before loading the simulation, check if the file exists
      validateFileExists(simulationFile)
      simulation <- ospsuite::loadSimulation(simulationFile, addToCache = FALSE)
      validateVector(minimumSimulationEndTime, type = "numeric", valueRange = c(0, Inf), nullAllowed = TRUE)
      # Following checks require simulation info
      endTime <- max(
        minimumSimulationEndTime,
        ospsuite::toUnit(
          quantityOrDimension = "Time",
          values = simulation$outputSchema$endTime,
          targetUnit = timeUnit
        )
      )
      validateVectorRange(timeOffset, type = "numeric", valueRange = c(0, endTime))

      # Test and validate outputs and their paths
      validateOutputObject(c(outputs), simulation, nullAllowed = TRUE)
      validateDataSource(dataSource, c(outputs), nullAllowed = TRUE)
      # Warn if time offset is not 0 or in application times
      allApplicationTimes <- getApplicationTimesForSimulation(simulation, c(outputs))
      allApplicationTimes <- ospsuite::toUnit(
        quantityOrDimension = "Time",
        values = c(0, allApplicationTimes),
        targetUnit = timeUnit
      )
      checkIsIncluded(timeOffset, allApplicationTimes, groupName = "Application Times")

      self$simulationSetName <- simulationSetName
      self$simulationFile <- simulationFile
      self$minimumSimulationEndTime <- minimumSimulationEndTime
      self$timeOffset <- timeOffset
      self$outputs <- c(outputs)
      self$dataSource <- dataSource
      self$dataSelection <- translateDataSelection(dataSelection)
      self$timeUnit <- timeUnit
      self$applicationRanges <- list(
        total = isIncluded(ApplicationRanges$total, applicationRanges),
        firstApplication = isIncluded(ApplicationRanges$firstApplication, applicationRanges),
        lastApplication = isIncluded(ApplicationRanges$lastApplication, applicationRanges)
      )
    }
  )
)
