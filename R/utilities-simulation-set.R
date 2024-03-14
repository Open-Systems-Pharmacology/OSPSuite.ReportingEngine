#' @title loadPKAnalysesFromStructureSet
#' @param structureSet A `SimulationStructure` object
#' @param to Format of the loaded output`
#' @return
#' A `PKAnalyses` object if `to="PKAnalyses"`
#' A `data.frame` if `to="data.frame"`
#' A `tibble` if `to="tibble"`
#' @import ospsuite
#' @keywords internal
loadPKAnalysesFromStructureSet <- function(structureSet, to = "PKAnalyses", useCache = FALSE) {
  if (useCache) {
    return(loadPKAnalysesFromCSV(
      filePath = structureSet$pkAnalysisResultsFileNames,
      simulation = loadSimulationWithUpdatedPaths(simulationSet = structureSet$simulationSet, loadFromCache = TRUE),
      to = to
    ))
  }
  return(loadPKAnalysesFromCSV(
    filePath = structureSet$pkAnalysisResultsFileNames,
    simulation = ospsuite::loadSimulation(structureSet$simulationSet$simulationFile),
    to = to
  ))
}

#' @title loadPKAnalysesFromCSV
#' @description Load PK analyses from a CSV file
#' Wrap the `ospsuite::importPKAnalysesFromCSV()` to provide more useful warning messages.
#' @param filePath Full path of the file containing the PK-Analyses to load
#' @param simulation A `Simulation` object
#' @param to Format of the loaded output`
#' @return
#' A `PKAnalyses` object if `to="PKAnalyses"`
#' A `data.frame` if `to="data.frame"`
#' A `tibble` if `to="tibble"`
#' @import ospsuite
#' @keywords internal
loadPKAnalysesFromCSV <- function(filePath, simulation, to = "PKAnalyses") {
  validateIsIncluded(to, c("PKAnalyses", "data.frame", "tibble"))
  withCallingHandlers(
    {
      pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
        filePath = filePath,
        simulation = simulation
      )
      pkAnalyses <- switch(to,
        "PKAnalyses" = pkAnalyses,
        "data.frame" = ospsuite::pkAnalysesToDataFrame(pkAnalyses),
        "tibble" = ospsuite::pkAnalysesToTibble(pkAnalyses)
      )
    },
    warning = function(warningCondition) {
      naParsingMessage <- grepl(
        pattern = "One or more parsing issues",
        warningCondition$message
      )
      # Usual warning are returned as is
      if (!naParsingMessage) {
        return()
      }
      # New message when NA is found in the CSV file
      warning(messages$warningNAFoundInPKAnalysisFile(filePath), call. = FALSE)
      # Remove initial warning message
      try({
        invokeRestart("muffleWarning")
      })
    }
  )
  return(pkAnalyses)
}

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
#' @param simulationSet A `SimulationSet` object
#' @return A data.frame including \code{path} and \code{pkParameter} from the `outputs` field of `simulationSet`
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
