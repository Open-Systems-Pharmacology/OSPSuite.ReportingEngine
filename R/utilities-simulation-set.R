#' @title getOutputPathsInSimulationSet
#' @param simulationSet SimulationSet object or derived class
#' @return Path names of outputs in `simulationSet`
#' @export
#' @family workflow helpers
getOutputPathsInSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  return(sapply(simulationSet$outputs, function(output) {
    output$path
  }))
}

#' @title getPKParametersInSimulationSet
#' @param simulationSet SimulationSet object or derived class
#' @return Data.frame with \code{path} and \code{pkParameter} in `simulationSet`
#' @export
#' @family workflow helpers
getPKParametersInSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  pkParametersTable <- data.frame()
  for (output in simulationSet$outputs) {
    pkParametersTable <- rbind.data.frame(
      pkParametersTable,
      data.frame(
        path = output$path,
        pkParameter = getPKParametersInOutput(output),
        group = getPKParameterGroupsInOutput(output),
        stringsAsFactors = FALSE
      )
    )
  }
  return(pkParametersTable)
}

#' @title loadSimulationWithUpdatedPaths
#' @param simulationSet simulation set containing path to simulation file and pathIDs for quantities to be loaded into simulation object
#' @param loadFromCache If `TRUE`, an already loaded pkml file will not be loaded again, but the `Simulation` object will be retrieved from cache.
#' If `FALSE`, a new `Simulation` object will be created. Default value is `FALSE`.
#' @param addToCache If `TRUE`, the loaded simulation is added to cache.
#' If `FALSE`, the returned simulation only exists locally. Default is `TRUE`.
#' @return A `Simulation` object with pathIDs updated from simulationSet
#' @export
loadSimulationWithUpdatedPaths <- function(simulationSet, loadFromCache = FALSE, addToCache = TRUE) {
  simulation <- ospsuite::loadSimulation(
    filePath = simulationSet$simulationFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
  # Prevent loadSimulationWithUpdatedPaths from crashing if user did not submit any pathID
  if (!is.null(simulationSet$outputs)) {
    simulation$outputSelections$clear()
    paths <- sapply(simulationSet$outputs, function(output) {
      output$path
    })
    ospsuite::addOutputs(quantitiesOrPaths = paths, simulation = simulation)
  }

  if (is.null(simulationSet$minimumSimulationEndTime)) {
    return(simulation)
  }

  if (simulationSet$minimumSimulationEndTime > simulation$outputSchema$endTime) {
    maximalIntervalIndex <- which(sapply(simulation$outputSchema$intervals, function(x) {
      x$endTime$value
    }) == simulation$outputSchema$endTime)[1]
    simulation$outputSchema$intervals[[maximalIntervalIndex]]$endTime$setValue(value = simulationSet$minimumSimulationEndTime, unit = ospUnits$Time$min)
  }
  return(simulation)
}

#' @title loadWorkflowPopulation
#' @param simulationSet A `PopulationSimulationSet` object
#' @export
#' @import ospsuite
loadWorkflowPopulation <- function(simulationSet) {
  validateIsOfType(simulationSet, "PopulationSimulationSet")
  population <- ospsuite::loadPopulation(simulationSet$populationFile)
  simulation <- loadSimulationWithUpdatedPaths(simulationSet)

  if (!is.null(simulationSet$studyDesignFile)) {
    addStudyParameters(population, simulation, simulationSet$studyDesignFile)
  }
  return(population)
}

#' @title getSimulationTimeRanges
#' @description Get time ranges for time profile plots according to applications and user defined settings
#' @param simulation A `Simulation` object
#' @param path Field `path` from `Output` object
#' @param simulationSet A `SimulationSet` or `PopulationSimulationSet` object
#' @return Lists including `values` and `name` of time ranges.
#' Also includes logical field `keep` to define if a specific application range is kept in report.
#' @keywords internal
getSimulationTimeRanges <- function(simulation, path, simulationSet) {
  timeUnit <- simulationSet$timeUnit
  applicationRanges <- simulationSet$applicationRanges
  # Initialize output
  timeRanges <- list(
    total = list(
      name = ApplicationRanges$total,
      keep = applicationRanges[[ApplicationRanges$total]],
      values = NULL
    ),
    firstApplication = list(
      name = ApplicationRanges$firstApplication,
      keep = FALSE,
      values = NULL
    ),
    lastApplication = list(
      name = ApplicationRanges$lastApplication,
      keep = FALSE,
      values = NULL
    )
  )

  # Get applications
  applications <- simulation$allApplicationsFor(path)
  applicationTimes <- 0
  if (!isOfLength(applications, 0)) {
    applicationTimes <- sapply(applications, function(application) {
      application$startTime$value
    })
  }
  # Get all ranges of simulation ranked defined by application intervals
  simulationRanges <- c(applicationTimes, simulation$outputSchema$endTime)
  simulationRanges <- sort(ospsuite::toUnit("Time", simulationRanges, timeUnit))

  # Store number of applications and their ranges
  logDebug(messages$numberOfApplications(length(applications), path, simulation$name))
  logDebug(messages$timeRangesForSimulation(paste0(simulationRanges, collapse = "', '"), simulation$name))

  # Define ranges for output
  # Depending on expected behaviour of settings$applicationRange
  # It would be possible to set these values
  timeRanges$total$values <- c(min(simulationRanges), max(simulationRanges))

  # Flag simulationRanges prior to timeOffset
  timeOffsetFlag <- simulationRanges < simulationSet$timeOffset
  if (any(timeOffsetFlag)) {
    logError(messages$warningApplicationsBeforeTimeOffset(
      sum(timeOffsetFlag),
      paste0(simulationRanges[timeOffsetFlag], collapse = ", "),
      timeUnit,
      simulationSet$timeOffset, simulationSet$simulationSetName
    ))
  }

  # Case of multiple applications, get first and last
  if (!isOfLength(simulationRanges, 2)) {
    # First application becomes first application after timeOffset
    timeRanges$firstApplication$values <- utils::head(simulationRanges[!timeOffsetFlag], 2)
    timeRanges$lastApplication$values <- utils::tail(simulationRanges, 2)
    timeRanges$firstApplication$keep <- applicationRanges[[ApplicationRanges$firstApplication]]
    timeRanges$lastApplication$keep <- applicationRanges[[ApplicationRanges$lastApplication]]
  }

  return(timeRanges)
}
