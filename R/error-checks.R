validateIsPositive <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, c("numeric", "integer"), nullAllowed)
  if (isFALSE(object > 0)) {
    stop(
      messages$errorWrongType(getObjectNameAsString(object), class(object)[1], "positive"),
      call. = FALSE
    )
  }
}

hasPositiveValues <- function(object) {
  object <- object[!is.na(object)]
  object <- object[!is.infinite(object)]
  return(any(object > 0))
}


validateIsInRange <- function(variableName, value, lowerBound, upperBound, nullAllowed = FALSE) {
  validateIsOfLength(value, 1)
  validateIsOfLength(lowerBound, 1)
  validateIsOfLength(upperBound, 1)
  validateIsNumeric(c(value, lowerBound, upperBound), nullAllowed)
  if ((value < lowerBound) | (value > upperBound)) {
    stop(
      messages$outsideRange(variableName, value, lowerBound, upperBound),
      call. = FALSE
    )
  }
}

typeNamesFrom <- function(type) {
  if (is.character(type)) {
    return(type)
  }
  type <- c(type)
  sapply(type, function(t) t$classname)
}

validateNoDuplicatedEntries <- function(x) {
  if (any(duplicated(x))) {
    stop(
      messages$errorDuplicatedEntries(getObjectNameAsString(x)),
      call. = FALSE
    )
  }
  return(invisible())
}

# TODO: replace the function name
validateIsIncludedAndLog <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }

  if (isIncluded(values, parentValues)) {
    return(invisible())
  }
  stop(messages$errorNotIncluded(values, parentValues, groupName))
}

checkIsIncluded <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }

  if (isIncluded(values, parentValues)) {
    return(invisible())
  }
  logError(messages$errorNotIncluded(values, parentValues, groupName))
  return(invisible())
}

validateMapping <- function(mapping, data, nullAllowed = FALSE) {
  if (nullAllowed && is.null(mapping)) {
    return(invisible())
  }

  validateIsString(mapping)
  variableNames <- names(data)

  validateIsIncluded(mapping, variableNames)

  return(invisible())
}

checkExisitingPath <- function(path, stopIfPathExists = FALSE) {
  if (!dir.exists(path)) {
    return(invisible())
  }
  if (stopIfPathExists) {
    stop(messages$warningExistingPath(path), call. = FALSE)
  }
  logDebug(messages$warningExistingPath(path))
  return(invisible())
}

checkOverwriteExisitingPath <- function(path, overwrite) {
  if (!dir.exists(path)) {
    return(invisible())
  }
  logDebug(messages$warningExistingPath(path))
  if (overwrite) {
    logDebug(messages$warningOverwriting(path))
    unlink(path, recursive = TRUE)
  }
}

fileExtension <- function(file) {
  ex <- strsplit(basename(file), split = "\\.")[[1]]
  return(utils::tail(ex, 1))
}

validateIsFileExtension <- function(path, extension, nullAllowed = FALSE) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (isFileExtension(path, extension)) {
    return(invisible())
  }
  stop(messages$errorExtension(path, extension))
}

validateFileExists <- function(path, nullAllowed = FALSE) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (all(file.exists(path))) {
    return(invisible())
  }
  stop(messages$errorUnexistingFile(path[!file.exists(path)]))
}

#' Check the consistency between observed data and its dictionary.
#' Units for `dv `and `time` need to be defined at least once in either
#' the observed dataset, its dictionary or outputs
#' In case of multiple definitions, warnings will thrown and the following priorities will be applied:
#' 1. Use units from outputs
#' 2. Use units from observed dataset
#' 3. Use units from dictionary
#' @param dataSource A `DataSource` object
#' @param observedDataFile Path of observed dataset
#' @param outputs list or array of `Output` objects
#' @keywords internal
validateDataSource <- function(dataSource, outputs, nullAllowed = TRUE) {
  if (nullAllowed && any(is.null(dataSource), is.null(outputs))) {
    return(invisible())
  }
  validateIsOfType(dataSource, "DataSource")
  # Read dictionary and check that mandatory variables are included
  dictionary <- readObservedDataFile(dataSource$metaDataFile)
  if (!isIncluded(dictionaryParameters$datasetUnit, names(dictionary))) {
    dictionary[, dictionaryParameters$datasetUnit] <- NA
  }
  validateIsIncludedInDataset(c(dictionaryParameters$ID, dictionaryParameters$datasetColumn), dictionary, datasetName = "dictionary")
  validateIsIncludedAndLog(c(dictionaryParameters$timeID, dictionaryParameters$dvID), dictionary[, dictionaryParameters$ID], groupName = paste0("Column '", dictionaryParameters$ID, "'"))

  # Check that dictionary and observed data are consistent
  observedDataset <- readObservedDataFile(dataSource$dataFile)
  timeVariable <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
  dvVariable <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
  lloqVariable <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)

  checkIsIncludedInDataset(c(timeVariable, dvVariable), observedDataset, datasetName = "observed dataset")
  checkIsIncludedInDataset(lloqVariable, observedDataset, datasetName = "observed dataset", nullAllowed = TRUE)

  # Check of unit definitions:
  # - If unit is defined as a datasetColumn
  timeUnitVariable <- getDictionaryVariable(dictionary, dictionaryParameters$timeUnitID)
  dvUnitVariable <- getDictionaryVariable(dictionary, dictionaryParameters$dvUnitID)

  # - If unit is defined as a value in datasetUnit
  timeMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$timeID
  dvMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$dvID

  timeUnit <- as.character(dictionary[timeMapping, dictionaryParameters$datasetUnit])
  dvUnit <- as.character(dictionary[dvMapping, dictionaryParameters$datasetUnit])

  validateUnitDataDefinition(timeUnit, timeUnitVariable, observedDataset)
  validateUnitDataDefinition(dvUnit, dvUnitVariable, observedDataset, outputs)
  return(invisible())
}

#' Check if the provided values are included all available dimensions
#' @param values Vector of dimensions
#' @return TRUE if the values are included all available dimensions
#' @import ospsuite
#' @keywords internal
isDimension <- function(values) {
  allAvailableDimensions <- ospsuite::allAvailableDimensions()
  return(isIncluded(c(values), allAvailableDimensions))
}

validateIsDimension <- function(values, nullAllowed = FALSE) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }
  if (isDimension(values)) {
    return(invisible())
  }
  stop(messages$errorNotADimension(values))
}

isPathInSimulation <- function(paths, simulation) {
  # Add every paths to the simulation object and
  # check if all of these paths are included
  ospsuite::addOutputs(quantitiesOrPaths = paths, simulation = simulation)
  allSimulationOutputPaths <- sapply(simulation$outputSelections$allOutputs, function(output) {
    output$path
  })
  return(isIncluded(paths, allSimulationOutputPaths))
}

validateIsPathInSimulation <- function(paths, simulation, nullAllowed = FALSE) {
  if (nullAllowed && is.null(paths)) {
    return(invisible())
  }
  if (isPathInSimulation(paths, simulation)) {
    return(invisible())
  }
  invalidPaths <- paths[sapply(paths, function(path) {
    !isPathInSimulation(path, simulation)
  })]
  stop(messages$invalidOuputPath(invalidPaths, simulation$name))
}

validateOutputObject <- function(outputs, simulation, nullAllowed = FALSE) {
  if (nullAllowed && is.null(outputs)) {
    return(invisible())
  }
  validateIsOfType(c(outputs), "Output")
  # Check paths existence
  allOutputPaths <- sapply(outputs, function(output) {
    output$path
  })
  validateIsPathInSimulation(allOutputPaths, simulation)

  # Check display unit
  for (output in outputs) {
    outputQuantity <- ospsuite::getQuantity(output$path, simulation)
    validateIsUnitFromDimension(output$displayUnit, outputQuantity$dimension, nullAllowed = TRUE)
    validateIsUnitFromDimension(output$dataUnit, outputQuantity$dimension, nullAllowed = TRUE)
  }
}

isUnitFromDimension <- function(unit, dimension) {
  dimensionForUnit <- ospsuite::getDimensionForUnit(unit)
  # Units can be switched between Mass/Amount and Concentration (molar)/Concentration (mass)
  # using molar weight as an input
  # Remove molar/mass for units that can cross dimensions using molar weight
  if (isIncluded(dimension, c("Mass", "Amount"))) {
    dimension <- c("Mass", "Amount")
  }
  if (isIncluded(dimension, c("Concentration (mass)", "Concentration (molar)"))) {
    dimension <- c("Concentration (mass)", "Concentration (molar)")
  }
  if (isEmpty(dimensionForUnit)) {
    return(FALSE)
  }
  return(isIncluded(dimensionForUnit, dimension))
}

validateIsUnitFromDimension <- function(unit, dimension, nullAllowed = FALSE) {
  if (nullAllowed && is.null(unit)) {
    return(invisible())
  }
  if (isUnitFromDimension(unit, dimension)) {
    return(invisible())
  }
  stop(messages$errorUnitNotFromDimension(unit, dimension))
}

validateHasReferencePopulation <- function(workflowType, simulationSets) {
  if (isIncluded(workflowType, PopulationWorkflowTypes$parallelComparison)) {
    return(invisible())
  }
  allSimulationReferences <- sapply(simulationSets, function(set) {
    set$referencePopulation
  })

  if (isOfLength(allSimulationReferences[allSimulationReferences], 1)) {
    return(invisible())
  }
  stop(messages$warningNoReferencePopulation(workflowType))
}


validateSameOutputsBetweenSets <- function(simulationSets) {
  pkParametersTableRef <- NULL
  for (set in simulationSets) {
    pkParametersTable <- getPKParametersInSimulationSet(set)
    # In case output or pkParameters are in different orders
    pkParametersTable <- pkParametersTable[order(pkParametersTable$path, pkParametersTable$group), c("path", "group")]

    if (is.null(pkParametersTableRef)) {
      pkParametersTableRef <- pkParametersTable
      next
    }
    if (all(pkParametersTable$path == pkParametersTableRef$path)) {
      pkParametersTableTest <- NULL
      for (pkParameterIndex in seq_along(pkParametersTable$group)) {
        pkParametersTableTest[pkParameterIndex] <- isIncluded(pkParametersTable$group[pkParameterIndex], pkParametersTableRef$group[pkParameterIndex])
      }
      if (all(pkParametersTableTest)) {
        pkParametersTableRef <- pkParametersTable
        next
      }
    }
    stop(messages$errorNotSameOutputsBetweenSets(sapply(
      simulationSets, function(set) {
        set$simulationSetName
      }
    )))
  }
}


validatehasOnlyDistinctValues <- function(data, dataName = "dataset", na.rm = TRUE, nullAllowed = FALSE) {
  if (nullAllowed && is.null(data)) {
    return(invisible())
  }
  if (hasOnlyDistinctValues(data, na.rm)) {
    return(invisible())
  }
  stop(messages$errorHasNoUniqueValues(data, dataName, na.rm))
}

validateIsIncludedInDataset <- function(columnNames, dataset, datasetName = NULL, nullAllowed = FALSE) {
  if (nullAllowed && is.null(columnNames)) {
    return(invisible())
  }
  if (isIncluded(columnNames, names(dataset))) {
    return(invisible())
  }
  stop(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName), call. = FALSE, immediate. = TRUE)
}

checkIsIncludedInDataset <- function(columnNames, dataset, datasetName = NULL, nullAllowed = FALSE) {
  if (nullAllowed && is.null(columnNames)) {
    return(invisible())
  }
  if (isIncluded(columnNames, names(dataset))) {
    return(invisible())
  }
  logError(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName))
  return(invisible())
}

validateUnitDataDefinition <- function(unit, unitColumn, observedDataset, outputs = NULL) {
  # In case, value from reading from Excel/csv file is not an actual NULL
  if (any(isEmpty(unit), is.na(unit), unit %in% "")) {
    unit <- NULL
  }
  # Get dataUnit from outputs
  dataUnit <- ifNotNull(
    outputs,
    unlist(lapply(outputs, function(output) {
      output$dataUnit
    })),
    NULL
  )

  # Checks for errors/warnings
  # - No unit at all
  noUnit <- all(isEmpty(unit), isEmpty(unitColumn), isEmpty(dataUnit))
  if (noUnit) {
    logErrorThenStop(messages$errorNoDataUnit())
  }
  # - No unit defined by dictionary: all outputs need to define dataUnit
  noDictionaryUnit <- all(isEmpty(unit), isEmpty(unitColumn))
  if (noDictionaryUnit) {
    if (!isSameLength(dataUnit, outputs)) {
      logErrorThenStop(messages$errorNoDataUnitInOutputs())
    }
    return(invisible())
  }
  # If units defined in dataFile,
  # check that unitColumn refers an actual column from observed data
  checkIsIncludedInDataset(unitColumn, observedDataset, datasetName = "observed dataset", nullAllowed = TRUE)

  # Check multiple unit definitions and their consistency
  checkOutputConsistency <- all(!isEmpty(unit), !isEmpty(dataUnit))
  if (checkOutputConsistency) {
    # Error when unit is different from dataUnit
    if (!isIncluded(unit, dataUnit)) {
      logErrorThenStop(messages$errorInconsistentDataUnit())
    }
    logError(messages$warningMultipleDataUnit())
    return(invisible())
  }
  # Warning consistency between unit definitions
  warnMutlipleUnitDefinitions <- any(
    all(!isEmpty(dataUnit), !isEmpty(unitColumn)),
    all(!isEmpty(unit), !isEmpty(unitColumn))
  )
  if (warnMutlipleUnitDefinitions) {
    logError(messages$warningMultipleDataUnit())
  }
  return(invisible())
}


validateCommandStatus <- function(command, status) {
  if (status != 0) {
    stop(messages$errorCommand(command, status))
  }
  return(invisible())
}

validateHasValidParameterPathsForSensitivity <- function(paths, simulationSetName) {
  if (isEmpty(paths)) {
    stop(messages$errorNoValidParametersForSensitivityAnalysis(simulationSetName))
  }
  return(invisible())
}

validateHasParametersForSensitivity <- function(numberOfParameters) {
  if (numberOfParameters > 0) {
    return(invisible())
  }
  stop(messages$errorNoParametersForSensitivityAnalysis())
}
