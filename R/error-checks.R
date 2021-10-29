isSameLength <- function(...) {
  args <- list(...)
  nrOfLengths <- length(unique(lengths(args)))

  return(nrOfLengths == 1)
}

#' Check if the provided object has nbElements elements
#'
#' @param object An object or a list of objects
#' @param nbElements number of elements that are supposed in object
#'
#' @return TRUE if the object or all objects inside the list have nbElements.
#' Only the first level of the given list is considered.
#' @keywords internal
isOfLength <- function(object, nbElements) {
  return(length(object) == nbElements)
}

validateIsOfLength <- function(object, nbElements) {
  if (isOfLength(object, nbElements)) {
    return(invisible())
  }
  logErrorThenStop(messages$errorWrongLength(object, nbElements))
}

#' Check if the provided object is of certain type
#'
#' @param object An object or a list of objects
#' @param type String  representation or Class of the type that should be checked for
#'
#' @return TRUE if the object or all objects inside the list are of the given type.
#' Only the first level of the given list is considered.
#' @keywords internal
isOfType <- function(object, type) {
  if (is.null(object)) {
    return(FALSE)
  }

  type <- typeNamesFrom(type)

  inheritType <- function(x) inherits(x, type)

  if (inheritType(object)) {
    return(TRUE)
  }
  object <- c(object)

  all(sapply(object, inheritType))
}

validateIsOfType <- function(object, type, nullAllowed = FALSE) {
  if (nullAllowed && is.null(object)) {
    return(invisible())
  }

  if (isOfType(object, type)) {
    return(invisible())
  }
  # Name of the variable in the calling function
  objectName <- getObjectNameAsString(object)
  objectTypes <- typeNamesFrom(type)
  # When called from validateIsString... objectName is "object"
  # Need to get the name from parent frame
  if (isIncluded(
    as.character(sys.call(-1)[[1]]),
    c("validateIsString", "validateIsLogical", "validateIsPositive", "validateIsNumeric", "validateIsInteger")
  )) {
    objectName <- deparse(substitute(object, sys.frame(-1)))
  }
  logErrorThenStop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
}

validateIsInteger <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, c("numeric", "integer"), nullAllowed)

  if (isFALSE(object %% 1 == 0)) {
    logErrorThenStop(messages$errorWrongType(getObjectNameAsString(object), class(object)[1], "integer"))
  }
}

validateIsPositive <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, c("numeric", "integer"), nullAllowed)

  if (isFALSE(object > 0)) {
    logErrorThenStop(messages$errorWrongType(getObjectNameAsString(object), class(object)[1], "positive"))
  }
}

hasPositiveValues <- function(object) {
  object <- object[!is.na(object)]
  object <- object[!is.infinite(object)]
  positiveValues <- object > 0
  return(!sum(positiveValues) == 0)
}


validateIsInRange <- function(variableName, value, lowerBound, upperBound, nullAllowed = FALSE) {
  validateIsOfLength(value, 1)
  validateIsOfLength(lowerBound, 1)
  validateIsOfLength(upperBound, 1)
  validateIsNumeric(c(value, lowerBound, upperBound), nullAllowed)
  if ((value < lowerBound) | (value > upperBound)) {
    logErrorThenStop(messages$outsideRange(variableName, value, lowerBound, upperBound))
  }
}

validateEnumValue <- function(enum, value) {
  if (value %in% names(enum)) {
    return(invisible())
  }

  logErrorThenStop(messages$errorValueNotInEnum(enum, value))
}

typeNamesFrom <- function(type) {
  if (is.character(type)) {
    return(type)
  }
  type <- c(type)
  sapply(type, function(t) t$classname)
}

validateIsString <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "character", nullAllowed)
}

validateIsNumeric <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, c("numeric", "integer"), nullAllowed)
}

validateIsLogical <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "logical", nullAllowed)
}

validateIsSameLength <- function(...) {
  if (isSameLength(...)) {
    return(invisible())
  }
  # Name of the variable in the calling function
  objectName <- getObjectNameAsString(list(...))

  # Name of the arguments
  argnames <- sys.call()
  arguments <- paste(lapply(argnames[-1], as.character), collapse = ", ")

  logErrorThenStop(messages$errorDifferentLength(arguments))
}

validateNoDuplicatedEntries <- function(x) {
  if (any(duplicated(x))) {
    logErrorThenStop(messages$errorDuplicatedEntries(getObjectNameAsString(x)))
  }
  return(invisible())
}

#' Check if the provided object is included in a parent object
#'
#' @param values Vector of values
#' @param parentValues Vector of values
#'
#' @return TRUE if the values are inside the parent values.
#' @keywords internal
isIncluded <- function(values, parentValues) {
  if (is.null(values)) {
    return(FALSE)
  }

  return(as.logical(min(values %in% parentValues)))
}

validateIsIncluded <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL, logFolder = NULL) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }

  if (isIncluded(values, parentValues)) {
    return(invisible())
  }
  if (is.null(logFolder)) {
    stop(messages$errorNotIncluded(values, parentValues, groupName))
  }
  logErrorThenStop(messages$errorNotIncluded(values, parentValues, groupName), logFolder)
}

checkIsIncluded <- function(values, parentValues, nullAllowed = FALSE, groupName = NULL, logFolder = NULL) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }

  if (isIncluded(values, parentValues)) {
    return(invisible())
  }
  if (is.null(logFolder)) {
    warning(messages$errorNotIncluded(values, parentValues, groupName), call. = FALSE, immediate. = TRUE)
    return(invisible())
  }
  logWorkflow(
    message = messages$errorNotIncluded(values, parentValues, groupName),
    pathFolder = logFolder,
    logTypes = c(LogTypes$Debug, LogTypes$Error)
  )
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
    logErrorThenStop(messages$warningExistingPath(path))
  }

  warning(messages$warningExistingPath(path))
}

checkOverwriteExisitingPath <- function(path, overwrite) {
  if (!dir.exists(path)) {
    return(invisible())
  }
  warning(messages$warningExistingPath(path))
  if (overwrite) {
    warning(messages$warningOverwriting(path))
    unlink(path, recursive = TRUE)
  }
}

fileExtension <- function(file) {
  ex <- strsplit(basename(file), split = "\\.")[[1]]
  return(utils::tail(ex, 1))
}

#' Check if the provided path has required extension
#'
#' @param file file or path name to be checked
#' @param extension extension of the file required after "."
#'
#' @return TRUE if the path includes the extension
#' @keywords internal
isFileExtension <- function(file, extension) {
  extension <- c(extension)
  file_ext <- fileExtension(file)
  file_ext %in% extension
}

validateIsFileExtension <- function(path, extension, nullAllowed = FALSE) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (isFileExtension(path, extension)) {
    return(invisible())
  }
  logErrorThenStop(messages$errorExtension(path, extension))
}

#' Log the error with a message and then stop, displaying same message.
#'
#' @param message message to display and then log
#' @param logFolderPath path where logs are saved
#' @keywords internal
logErrorThenStop <- function(message, logFolderPath = getwd()) {
  logWorkflow(
    message = message,
    pathFolder = logFolderPath,
    logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error)
  )
  stop(message, call. = FALSE)
}

#' Log the error with a message
#' @param message message to display and then log
#' @param logFolderPath path where logs are saved
#' @keywords internal
logErrorMessage <- function(message, logFolderPath = getwd()) {
  logWorkflow(
    message = message,
    pathFolder = logFolderPath,
    logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error)
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
    stop(messages$errorObservedMetaDataFileNotProvided(observedDataFile))
  }
  # Read dictionary and check that mandatory variables are included
  dictionary <- readObservedDataFile(observedMetaDataFile)
  if (!isIncluded(dictionaryParameters$datasetUnit, names(dictionary))) {
    dictionary[, dictionaryParameters$datasetUnit] <- NA
  }
  validateIsIncludedInDataset(c(dictionaryParameters$ID, dictionaryParameters$datasetColumn), dictionary, datasetName = "dictionary")
  validateIsIncluded(c(dictionaryParameters$timeID, dictionaryParameters$dvID), dictionary[, dictionaryParameters$ID], groupName = paste0("Column '", dictionaryParameters$ID, "'"))

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

  logErrorThenStop(messages$errorNotADimension(values))
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
  logErrorThenStop(message = messages$invalidOuputPath(paths, simulation$name))
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
  if (isOfLength(dimensionForUnit, 0)) {
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

validateHasReferencePopulation <- function(workflowType, simulationSets, logFolder = NULL) {
  if (isIncluded(workflowType, PopulationWorkflowTypes$parallelComparison)) {
    return(invisible())
  }
  allSimulationReferences <- sapply(simulationSets, function(set) {
    set$referencePopulation
  })

  if (isOfLength(allSimulationReferences[allSimulationReferences], 1)) {
    return(invisible())
  }
  if (is.null(logFolder)) {
    stop(messages$warningNoReferencePopulation(workflowType))
  }
  logErrorThenStop(messages$warningNoReferencePopulation(workflowType), logFolder)
}


validateSameOutputsBetweenSets <- function(simulationSets, logFolder = NULL) {
  pkParametersTableRef <- NULL
  for (set in simulationSets) {
    pkParametersTable <- getPKParametersInSimulationSet(set)
    # In case output or pkParameters are in different orders
    pkParametersTable <- pkParametersTable[order(pkParametersTable$path, pkParametersTable$pkParameter), ]

    if (is.null(pkParametersTableRef)) {
      pkParametersTableRef <- pkParametersTable
      next
    }


    if (all(pkParametersTable$path == pkParametersTableRef$path)) {
      pkParametersTableTest <- NULL
      for (pkParameterIndex in seq_along(pkParametersTable$pkParameter)) {
        pkParametersTableTest[pkParameterIndex] <- isIncluded(pkParametersTable$pkParameter[pkParameterIndex], pkParametersTableRef$pkParameter[pkParameterIndex])
      }

      if (all(pkParametersTableTest)) {
        pkParametersTableRef <- pkParametersTable
        next
      }
    }


    if (is.null(logFolder)) {
      stop(messages$errorNotSameOutputsBetweenSets(sapply(simulationSets, function(set) {
        set$simulationSetName
      })))
    }
    logErrorThenStop(messages$errorNotSameOutputsBetweenSets(sapply(simulationSets, function(set) {
      set$simulationSetName
    })), logFolder)
  }
}

hasUniqueValues <- function(data, na.rm = TRUE) {
  # na.rm is the usual tidyverse input to remove NA values
  if (na.rm) {
    data <- data[!is.na(data)]
  }
  return(!any(duplicated(data)))
}

validateHasUniqueValues <- function(data, dataName = "dataset", na.rm = TRUE, nullAllowed = FALSE) {
  if (nullAllowed && is.null(data)) {
    return(invisible())
  }
  if (hasUniqueValues(data, na.rm)) {
    return(invisible())
  }
  stop(messages$errorHasNoUniqueValues(data, dataName, na.rm))
}

validateIsIncludedInDataset <- function(columnNames, dataset, datasetName = NULL, nullAllowed = FALSE, logFolder = NULL) {
  if (nullAllowed && is.null(columnNames)) {
    return(invisible())
  }
  if (isIncluded(columnNames, names(dataset))) {
    return(invisible())
  }
  if (is.null(logFolder)) {
    stop(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName), call. = FALSE, immediate. = TRUE)
  }
  logErrorThenStop(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName))
}

checkIsIncludedInDataset <- function(columnNames, dataset, datasetName = NULL, nullAllowed = FALSE, logFolder = NULL) {
  if (nullAllowed && is.null(columnNames)) {
    return(invisible())
  }
  if (isIncluded(columnNames, names(dataset))) {
    return(invisible())
  }
  #TODO this check should be ion logWorkflow and not in the caller!!
  if (is.null(logFolder)) {
    warning(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName), call. = FALSE, immediate. = TRUE)
    return(invisible())
  }
  logWorkflow(
    message = messages$errorNotIncludedInDataset(columnNames, dataset, datasetName),
    pathFolder = logFolder,
    logTypes = c(LogTypes$Debug, LogTypes$Error)
  )
}

validateUnitDataDefinition <- function(unit, unitColumn, observedDataset, outputs = NULL) {
  # In case, value from reading from Excel/csv file is not an actual NULL
  if (any(isOfLength(unit, 0), is.na(unit), unit %in% "")) {
    unit <- NULL
  }
  # Case unit is defined using outputs
  dataUnit <- NULL
  if (!isOfLength(outputs, 0)) {
    dataUnit <- unlist(lapply(outputs, function(output) {
      output$dataUnit
    }))
  }

  # Checks for errors
  # If no unit defined at all
  if (isOfLength(c(unit, unitColumn, dataUnit), 0)) {
    stop(messages$errorNoDataUnit())
  }
  # If no unit defined by dictionray, all outputs need to define dataUnit
  if (isOfLength(c(unit, unitColumn), 0)) {
    if (!isSameLength(dataUnit, outputs)) {
      stop(messages$errorNoDataUnitInOutputs())
    }
    return(invisible())
  }
  # Checks for warnings
  # Only one of unit, unitColumn and dataUnit should be defined
  # in the case dataUnit was defined, code has already returned
  if (!isOfLength(c(unit, unitColumn, dataUnit), 1)) {
    warning(messages$warningMultipleDataUnit())
  }
  # If defined, check that unitColumn refers an actual column from observed data
  checkIsIncludedInDataset(unitColumn, observedDataset, datasetName = "observed dataset", nullAllowed = TRUE)

  return(invisible())
}
