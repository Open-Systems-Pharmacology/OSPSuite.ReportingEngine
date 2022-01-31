# Assert helpers ---------
# Assert helpers starting by "is" or "has" should return a logical value

# Seems it is only called once in pk parameters plot
hasPositiveValues <- function(object) {
  object <- object[!is.na(object)]
  object <- object[!is.infinite(object)]
  positiveValues <- object > 0
  return(!sum(positiveValues) == 0)
}

isPositive <- function(values, na.rm = TRUE) {
  if (na.rm) {
    values <- values[!is.na(values)]
  }
  if (isOfLength(values, 0)) {
    return(FALSE)
  }
  return(all(values > 0))
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
  if (isOfLength(dimensionForUnit, 0)) {
    return(FALSE)
  }
  return(isIncluded(dimensionForUnit, dimension))
}

#' @title isBetween
#' @description Assess if `x` is between `left` and `right` bounds.
#' Shortcut for `x >= left & x <= right` if `strict=FALSE` (default).
#' Shortcut for `x > left & x < right` if `strict=TRUE`.
#' @param x Numeric values to assess
#' @param left Numeric value(s) used as lower bound
#' @param right Numeric value(s) used as upper bound
#' @param strict Logical value defining if `x` is strictly between `left` and `right`.
#' Default value is `FALSE`.
#' @return Logical values
#' @export
#' @examples
#' isBetween(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[isBetween(x, -1, 1)]
#'
#' isBetween(x, cos(x) + 1, cos(x) - 1)
isBetween <- function(x, left, right, strict = FALSE) {
  if (strict) {
    return(x > left & x < right)
  }
  return(x >= left & x <= right)
}

isEmpty <- function(object) {
  if (isOfLength(object, 0)) {
    return(TRUE)
  }
  # Length of empty data.frames counts the columns
  if (!class(object)[1] %in% "data.frame") {
    return(FALSE)
  }
  if (nrow(object) == 0) {
    return(TRUE)
  }
  return(FALSE)
}

# Validate helpers ---------
validateIsOfTypeRE <- function(object, type, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsOfType(object, type, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), typeof(object), type),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsStringRE <- function(object, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsString(object, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), typeof(object), "character"),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsNumericRE <- function(object, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsNumeric(object, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), typeof(object), c("numeric", "integer")),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsIntegerRE <- function(object, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsInteger(object, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), typeof(object), "integer"),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsLogicalRE <- function(object, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsLogical(object, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), typeof(object), "logical"),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsIncludedRE <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsIncluded(values, parentValues, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorNotIncluded(values, parentValues, groupName),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsIncludedInDataset <- function(columnNames, dataset, nullAllowed = FALSE, datasetName = NULL, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsIncluded(columnNames, names(dataset), nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorNotIncludedInDataset(columnNames, dataset, datasetName),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsOfLengthRE <- function(object, nbElements, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsOfLength(object, nbElements)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongLength(getObjectNameAsString(object), nbElements),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

# Note that empty includes null but also numeric(0), character(0), etc.
validateIsNotEmpty <- function(object, logFolder = NULL, optionalMessage = NULL) {
  if (!isEmpty(object)) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||% paste0(getObjectNameAsString(object), " is empty"),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateIsSameLengthRE <- function(..., logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsSameLength(...)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||% messages$errorDifferentLength(...),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

validateIsPositive <- function(object, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  if (nullAllowed && is.null(object)) {
    return(invisible())
  }
  validateIsOfTypeRE(object, c("numeric", "integer"), nullAllowed = nullAllowed, logFolder = logFolder, optionalMessage = optionalMessage)
  if (isPositive(object)) {
    logMessage(
      message = optionalMessage %||%
        messages$errorWrongType(getObjectNameAsString(object), class(object)[1], "positive"),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
}

# Note that -Inf/Inf can be use for validation of greater and lower only
validateIsBetween <- function(object, left, right, strict = FALSE, nullAllowed = FALSE, objectName = NULL, logFolder = NULL, optionalMessage = NULL) {
  if (nullAllowed && is.null(object)) {
    return(invisible())
  }
  validateIsNumericRE(object, nullAllowed = nullAllowed, logFolder = logFolder, optionalMessage = optionalMessage)
  if (isTRUE(all(isBetween(values, left, right, strict)))) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||%
      messages$outsideRange(objectName %||% getObjectNameAsString(object), object, left, right),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateHasUniqueValues <- function(object, na.rm = TRUE, nullAllowed = FALSE, objectName = NULL, logFolder = NULL, optionalMessage = NULL) {
  if (nullAllowed && is.null(object)) {
    return(invisible())
  }
  if (hasUniqueValues(object, na.rm)) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||%
      messages$errorHasNoUniqueValues(object, objectName %||% getObjectNameAsString(object), na.rm),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateIsFileExtension <- function(path, extension, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (isFileExtension(path, extension)) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||% messages$errorExtension(path, extension),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateFileExists <- function(path, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (file.exists(path)) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||% messages$errorUnexistingFile(path),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

#' Check the consistency between observed data and its dictionary.
#' Units for `dv `and `time` need to be defined at least once in either
#' the observed dataset, its dictionary or outputs
#' In case of multiple definitions, warnings will thrown and the following priorities will be applied:
#' 1. Use units from outputs
#' 2. Use units from observed dataset
#' 3. Use units from dictionary
#' @param observedMetaDataFile Path of meta data file on observed dataset (also called dictionary)
#' @param observedDataFile Path of observed dataset
#' @param outputs list or array of `Output` objects
#' @keywords internal
validateObservedMetaDataFile <- function(observedMetaDataFile, observedDataFile, outputs) {
  # Check that dictionary is provided
  if (isOfLength(observedMetaDataFile, 0)) {
    logMessage(
      messages$errorObservedMetaDataFileNotProvided(observedDataFile),
      logLevel = LogLevels$Error
    )
  }
  # Read dictionary and check that mandatory variables are included
  dictionary <- readObservedDataFile(observedMetaDataFile)
  if (!isIncluded(dictionaryParameters$datasetUnit, names(dictionary))) {
    dictionary[, dictionaryParameters$datasetUnit] <- NA
  }
  validateIsIncludedInDataset(c(dictionaryParameters$ID, dictionaryParameters$datasetColumn), dictionary, datasetName = "dictionary")
  validateIsIncludedRE(c(dictionaryParameters$timeID, dictionaryParameters$dvID), dictionary[, dictionaryParameters$ID], groupName = paste0("Column '", dictionaryParameters$ID, "'"))

  # Check that dictionary and observed data are consitent
  observedDataset <- readObservedDataFile(observedDataFile)
  timeVariable <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
  dvVariable <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
  lloqVariable <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)

  checkIsIncludedInDataset(c(timeVariable, dvVariable), observedDataset, datasetName = "observed dataset")
  checkIsIncludedInDataset(lloqVariable, observedDataset, datasetName = "observed dataset", nullAllowed = TRUE)

  # Check of unit definitions:
  # 1) unit defined in outptuts
  dataUnit <- NULL
  if (!isOfLength(outputs, 0)) {
    dataUnit <- unlist(lapply(outputs, function(output) {
      output$dataUnit
    }))
  }

  # 2) If unit is defined as a datasetColumn
  timeUnitVariable <- getDictionaryVariable(dictionary, dictionaryParameters$timeUnitID)
  dvUnitVariable <- getDictionaryVariable(dictionary, dictionaryParameters$dvUnitID)

  # 3) If unit is defined as a value in datasetUnit
  timeMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$timeID
  dvMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$dvID

  timeUnit <- as.character(dictionary[timeMapping, dictionaryParameters$datasetUnit])
  dvUnit <- as.character(dictionary[dvMapping, dictionaryParameters$datasetUnit])

  validateUnitDataDefinition(timeUnit, timeUnitVariable, observedDataset)
  validateUnitDataDefinition(dvUnit, dvUnitVariable, observedDataset, dataUnit)
  return(invisible())
}

validateIsPathInSimulation <- function(paths, simulation, nullAllowed = FALSE, logFolder = NULL) {
  if (nullAllowed && is.null(paths)) {
    return(invisible())
  }
  if (isPathInSimulation(paths, simulation)) {
    return(invisible())
  }
  logMessage(
    messages$invalidOuputPath(paths, simulation$name),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateIsUnitFromDimension <- function(unit, dimension, nullAllowed = FALSE, logFolder = NULL) {
  if (nullAllowed && is.null(unit)) {
    return(invisible())
  }
  if (isUnitFromDimension(unit, dimension)) {
    return(invisible())
  }
  logMessage(
    message = messages$errorUnitNotFromDimension(unit, dimension),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateOutputObject <- function(outputs, simulation, nullAllowed = FALSE, logFolder = NULL) {
  if (nullAllowed && is.null(outputs)) {
    return(invisible())
  }
  validateIsOfTypeRE(c(outputs), "Output", logFolder = logFolder)
  # Check paths existence
  allOutputPaths <- sapply(outputs, function(output) output$path)
  validateIsPathInSimulation(allOutputPaths, simulation, logFolder = logFolder)

  # Check display unit
  for (output in outputs) {
    outputQuantity <- ospsuite::getQuantity(output$path, simulation)
    validateIsUnitFromDimension(output$displayUnit, outputQuantity$dimension, nullAllowed = TRUE, logFolder = logFolder)
  }
}

validateHasReferencePopulation <- function(workflowType, simulationSets, logFolder = NULL) {
  if (isIncluded(workflowType, PopulationWorkflowTypes$parallelComparison)) {
    return(invisible())
  }
  allSimulationReferences <- sapply(simulationSets, function(set) set$referencePopulation)

  if (isOfLength(allSimulationReferences[allSimulationReferences], 1)) {
    return(invisible())
  }
  logMessage(
    message = messages$warningNoReferencePopulation(workflowType),
    logLevel = LogLevels$Error,
    logFolder = logFolder
  )
}

validateSameOutputsBetweenSets <- function(simulationSets, logFolder = NULL) {
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
    logMessage(
      message = messages$errorNotSameOutputsBetweenSets(
        sapply(simulationSets, function(set) set$simulationSetName)
      ),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
}

validateUnitDataDefinition <- function(unit, unitColumn, observedDataset, outputs = NULL, logFolder = NULL) {
  # In case, value from reading from Excel/csv file is not an actual NULL
  if (any(isOfLength(unit, 0), is.na(unit), unit %in% "")) {
    unit <- NULL
  }
  # Case unit is defined using outputs
  dataUnit <- NULL
  if (!isOfLength(outputs, 0)) {
    dataUnit <- unlist(lapply(outputs, function(output) output$dataUnit))
  }

  # Checks for errors
  # If no unit defined at all
  if (isOfLength(c(unit, unitColumn, dataUnit), 0)) {
    logMessage(
      messages$errorNoDataUnit(),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  # If no unit defined by dictionray, all outputs need to define dataUnit
  if (isOfLength(c(unit, unitColumn), 0)) {
    if (!isSameLength(dataUnit, outputs)) {
      logMessage(
        messages$errorNoDataUnitInOutputs(),
        logLevel = LogLevels$Error,
        logFolder = logFolder
      )
    }
    return(invisible())
  }
  # Checks for warnings
  # Only one of unit, unitColumn and dataUnit should be defined
  # in the case dataUnit was defined, code has already returned
  if (!isOfLength(c(unit, unitColumn, dataUnit), 1)) {
    logMessage(
      messages$warningMultipleDataUnit(),
      logLevel = LogLevels$Warning,
      logFolder = logFolder
    )
  }
  # If defined, check that unitColumn refers an actual column from observed data
  checkIsIncludedInDataset(unitColumn, observedDataset, datasetName = "observed dataset", nullAllowed = TRUE, logFolder = logFolder)

  return(invisible())
}

validateCommandStatus <- function(command, status) {
  if (status != 0) {
    logMessage(
      messages$errorCommand(command, status),
      logLevel = LogLevels$Error
    )
  }
  return(invisible())
}

#-------- Warning functions ---------
# Different from null as it also includes character(0) or data.frame without any rows
checkIsNotEmpty <- function(object, logFolder = NULL, optionalMessage = NULL) {
  if (!isEmpty(object)) {
    return(invisible())
  }
  logMessage(
    message = optionalMessage %||% paste0(getObjectNameAsString(object), " is empty"),
    logLevel = LogLevels$Warning,
    logFolder = logFolder
  )
  return(invisible())
}

checkFileExists <- function(path, nullAllowed = FALSE, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    validateFileExists(path, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||% messages$errorUnexistingFile(path),
      logLevel = LogLevels$Warning,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

checkIsIncluded <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    validateIsIncluded(values, parentValues, nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorNotIncluded(values, parentValues, groupName),
      logLevel = LogLevels$Warning,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

checkIsIncludedInDataset <- function(columnNames, dataset, nullAllowed = FALSE, datasetName = NULL, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    # imported from ospsuite.utils
    validateIsIncluded(columnNames, names(dataset), nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorNotIncludedInDataset(columnNames, dataset, datasetName),
      logLevel = LogLevels$Warning,
      logFolder = logFolder
    )
  }
  )
  return(invisible())
}

checkHasUniqueValues <- function(object, na.rm = TRUE, nullAllowed = FALSE, objectName = NULL, logFolder = NULL, optionalMessage = NULL) {
  tryCatch({
    validateHasUniqueValues(object, na.rm = na.rm, nullAllowed = nullAllowed)
  },
  error = function(e) {
    logMessage(
      message = optionalMessage %||%
        messages$errorHasNoUniqueValues(object, objectName %||% getObjectNameAsString(object), na.rm),
      logLevel = LogLevels$Error,
      logFolder = logFolder
    )
  }
  )
}
