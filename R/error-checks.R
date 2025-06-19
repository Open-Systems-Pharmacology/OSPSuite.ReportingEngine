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
  if (any(value < lowerBound, value > upperBound)) {
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

checkFileExists <- function(path, nullAllowed = FALSE) {
  if (nullAllowed && is.null(path)) {
    return(invisible())
  }
  if (all(file.exists(path))) {
    return(invisible())
  }
  warning(messages$errorUnexistingFile(path[!file.exists(path)]))
}

#' Check the consistency between observed data and its dictionary.
#' Units for `dv `and `time` need to be defined at least once in either
#' the observed dataset, its dictionary or outputs
#' In case of multiple definitions, warnings will thrown and the following priorities will be applied:
#' 1. Use units from outputs
#' 2. Use units from observed dataset
#' 3. Use units from dictionary
#' @param dataSource A `DataSource` object
#' @param outputs list or array of `Output` objects
#' @param nullAllowed Logical defining if `NULL` input is allowed
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
  if (isIncluded(dimension, c("AUC (mass)", "AUC (molar)"))) {
    dimension <- c("AUC (mass)", "AUC (molar)")
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

#' @title validateNoDuplicate
#' @description
#' Leverage `ospsuite.utils::validateHasOnlyDistinctValues()` to
#' validate that a vector has only distinct values and display a useful message.
#' @param values An array to validate
#' @param variableName Name of variable that can be used to display a useful message
#' @param na.rm logical indicating if `NA` values should be removed before the check
#' @param nullAllowed logical indicating if `NULL` values should be allowed
#' @import  ospsuite.utils
#' @keywords internal
validateNoDuplicate <- function(values, variableName = NULL, na.rm = TRUE, nullAllowed = FALSE) {
  if (nullAllowed && is.null(values)) {
    return(invisible())
  }
  if (hasOnlyDistinctValues(values, na.rm = na.rm)) {
    return(invisible())
  }
  stop(
    messages$errorHasNoUniqueValues(values, variableName, na.rm = na.rm),
    call. = FALSE
  )
}

checkNoDuplicate <- function(values, variableName = NULL, na.rm = TRUE, nullAllowed = FALSE) {
  tryCatch(
    {
      validateNoDuplicate(
        values = values,
        variableName = variableName,
        na.rm = na.rm,
        nullAllowed = nullAllowed
      )
    },
    error = function(e) {
      warning(e$message, call. = FALSE)
    }
  )
  return(invisible())
}

excelCheckNoDuplicate <- function(values, variableName = NULL) {
  excelMessage <- tryCatch(
    {
      validateNoDuplicate(
        values = values,
        variableName = variableName
      )
    },
    error = function(e) {
      e$message
    }
  )
  return(excelMessage)
}

validateIsIncludedInDataset <- function(columnNames, dataset, datasetName = NULL, nullAllowed = FALSE) {
  if (nullAllowed && is.null(columnNames)) {
    return(invisible())
  }
  if (isIncluded(columnNames, names(dataset))) {
    return(invisible())
  }
  stop(messages$errorNotIncludedInDataset(columnNames, dataset, datasetName), call. = FALSE)
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

checkPKParameterExists <- function(pkParameter, pkParameterName, pkRatioMapping) {
  if (!isEmpty(pkParameter)) {
    return(TRUE)
  }
  warning(messages$pkParameterNotFound(pkParameterName, pkRatioMapping), call. = FALSE)
  return(FALSE)
}

checkPKRatioObservedVariable <- function(variableName, observedData) {
  if (isIncluded(variableName, names(observedData))) {
    return(TRUE)
  }
  warning(
    messages$errorNotIncludedInDataset(
      variableName,
      observedData,
      datasetName = "PK Ratio Dataset"
    ),
    call. = FALSE
  )
  return(FALSE)
}

checkPKRatioObservedRecord <- function(selectedRow, observedDataRecordId) {
  if (isOfLength(selectedRow, 1)) {
    return(TRUE)
  }
  warning(
    messages$warningPKRatioMultipleObservedRows(
      length(selectedRow),
      observedDataRecordId
    ),
    call. = FALSE
  )
  return(FALSE)
}

validateGuestParameters <- function(guestParameters, pkParameters) {
  # Repurpose validateIsIncluded
  # and update error message
  tryCatch(
    {
      validateIsIncluded(guestParameters, pkParameters)
    },
    error = function(e) {
      stop(messages$errorParametersNotIncludedInDDI(setdiff(guestParameters, pkParameters)), call. = FALSE)
    }
  )
  return(invisible())
}

checkLLOQValues <- function(lloq, structureSet) {
  if (any(is.infinite(lloq))) {
    warning(
      messages$warningHasInfiniteValues(
        n = sum(is.infinite(lloq)),
        datasetName = structureSet$simulationSet$dataSource$dataFile
      ),
      call. = FALSE
    )
    lloq[is.infinite(lloq)] <- NA
  }
  # If no negative value, return lloq
  if (isEmpty(which(lloq <= 0))) {
    return(lloq)
  }
  warning(
    messages$negativeDataRemoved(length(which(lloq <= 0))),
    call. = FALSE
  )
  lloq[lloq <= 0 & !is.na(lloq)] <- NA
  return(lloq)
}

#' @title checkIsSamePopulation
#' @description Check if 2 simulation sets use the same population.
#' Same population is identified as
#' 1- same population file and 2- same study design file (if defined)
#' @param simulationSet A `PopulationSimulationSet` object
#' @param referenceSet A `PopulationSimulationSet` object
#' @return A logical
#' @keywords internal
checkIsSamePopulation <- function(simulationSet, referenceSet) {
  isSamePopulation <- all(
    simulationSet$populationFile %in% referenceSet$populationFile,
    any(
      simulationSet$studyDesignFile %in% referenceSet$studyDesignFile,
      all(
        isEmpty(simulationSet$studyDesignFile),
        isEmpty(referenceSet$studyDesignFile)
      )
    )
  )
  return(isSamePopulation)
}

validateMoleculesFromCompounds <- function(molecules, compoundNames) {
  compoundsInMolecules <- sapply(molecules, function(molecule) {
    molecule$name
  })
  isMoleculesFromCompounds <- all(compoundsInMolecules %in% compoundNames)

  if (isMoleculesFromCompounds) {
    return()
  }

  pathsNotFromCompounds <- sapply(
    molecules[!(compoundsInMolecules %in% compoundNames)],
    function(molecule) {
      molecule$path
    }
  )

  stop(
    paste0(
      "The following molecule paths were not from the selected compounds (",
      paste(compoundNames, collapse = ", "),
      "): ",
      paste(pathsNotFromCompounds, collapse = ", ")
    )
  )
}

checkMoleculesAlreadyIncluded <- function(moleculePaths, previousMoleculePaths) {
  if (!isIncluded(moleculePaths, previousMoleculePaths)) {
    return()
  }
  warning(
    paste0(
      "The following molecule paths were included multiple times in the mass balance: ",
      paste(moleculePaths[moleculePaths %in% previousMoleculePaths], collapse = ", ")
    )
  )
  return()
}

#' @title isLoadedSimulation
#' @description
#' Check that the simulation has been loaded
#' @param simulation A `Simulation` object
#' @return Logical indicating if the `simulation` exists
#' @export
isLoadedSimulation <- function(simulation) {
  return(isOfType(simulation, "Simulation"))
}

#' @title isLoadedPopulation
#' @description
#' Check that the population has been loaded
#' @param population A `Population` object
#' @return Logical indicating if the `population` exists
#' @export
isLoadedPopulation <- function(population) {
  return(isOfType(population, "Population"))
}

#' @title isLoadedPackage
#' @description
#' Check that a R package has been successfully loaded
#' @param packageName Name of the package
#' @export
isLoadedPackage <- function(packageName) {
  return(isIncluded(packageName, .packages()))
}

#' @title validateHasRunOnAllCores
#' @description
#' Validate if all cores executed an mpi.remote.exec command successfully.
#' @param coreResults list of logical results returned by each core after an mpi.remote.exec command is complete
#' @param inputName Name of the input to be loaded
#' @param inputType Type of input to be loaded
#' @param runType Type of run executed on `{Rmpi}` cores
#' @keywords internal
validateHasRunOnAllCores <- function(coreResults, inputName, inputType, runType = "load") {
  hasRunOnAllCores <- all(sapply(coreResults, identity))
  if (hasRunOnAllCores) {
    return(invisible())
  }
  # If specific cores have not run, returns only their results in error message
  coresNotRun <- which(!sapply(coreResults, identity))
  inputName <- highlight(inputName)
  if (isSameLength(inputName, coreResults)) {
    inputName <- paste(inputName[coresNotRun], collapse = ", ")
  }
  stop(
    switch(runType,
      "load" = messages$errorNotLoadedOnCores(paste(inputType, inputName)),
      "task" = messages$errorNotCompletedOnCores(paste(inputType, inputName))
    ),
    call. = FALSE
  )
}

#' @title checkHasRunOnAllCores
#' @description
#' Check if all cores executed an mpi.remote.exec command successfully.
#' @inheritParams validateHasRunOnAllCores
#' @keywords internal
checkHasRunOnAllCores <- function(coreResults, inputName, inputType, runType = "load") {
  tryCatch(
    {
      validateHasRunOnAllCores(
        coreResults = coreResults,
        inputName = inputName,
        inputType = inputType,
        runType = runType
      )
    },
    error = function(e) {
      warning(e$message, call. = FALSE)
    }
  )
  return(invisible())
}

#' @title checkSamePopulationIds
#' @description
#' Check if PK Analyses with same population actually use the same IndividualIds
#' @param setIds A vector of IndividualIds for a simulation set
#' @param referenceSetIds A vector of IndividualIds for the reference simulation set
#' @param setName Name of simulation set for warning message
#' @param referenceSetName Name of the reference simulation set for warning message
#' @keywords internal
checkSamePopulationIds <- function(setIds,
                                   referenceSetIds,
                                   setName,
                                   referenceSetName) {
  tryCatch(
    {
      validateIsIncluded(referenceSetIds, setIds)
    },
    error = function(e) {
      missingIds <- setdiff(referenceSetIds, setIds)
      warning(messages$warningPKAnalysesMissingIds(missingIds, setName), call. = FALSE)
    }
  )
  tryCatch(
    {
      validateIsIncluded(setIds, referenceSetIds)
    },
    error = function(e) {
      missingIds <- setdiff(setIds, referenceSetIds)
      warning(messages$warningPKAnalysesMissingIds(missingIds, referenceSetName), call. = FALSE)
    }
  )
  return(invisible())
}

#' @title checkMetaDataIsConsistent
#' @description
#' Check consistency of metadata units, dimensions and residuals scale
#' @param metaData A data.frame summarizing meta data of multiple `SimulationSet` and `Output` objects
#' @keywords internal
checkMetaDataIsConsistent <- function(metaData) {
  groupId <- unique(metaData$group)
  if (!isOfLength(unique(metaData$unit), 1)) {
    warning(
      messages$inconsistentMetaData(
        values = metaData$unit,
        id = groupId,
        dataType = "units"
      ),
      call. = FALSE
    )
  }
  if (!isOfLength(unique(metaData$residualScale), 1)) {
    warning(
      messages$inconsistentMetaData(
        values = metaData$residualScale,
        id = groupId,
        dataType = "residualScale"
      ),
      call. = FALSE
    )
  }
  return()
}

#' @title checkPKParameterEndTime
#' @description
#' Check consistency of PK Parameter end time
#' @param pkParameterName PK Parameter name. See `ospsuite::allPKParameterNames()` for more details
#' @param endTime Qualification `EndTime` field
#' @keywords internal
checkPKParameterEndTime <- function(pkParameterName, endTime) {
  if (isIncluded(pkParameterName, "AUC")) {
    logInfo(messages$ddiAUCSelection(endTime))
    return(invisible(NULL))
  }
  parameterFromSimTime <- any(
    grepl(pattern = "tEnd", pkParameterName),
    grepl(pattern = "tDLast", pkParameterName),
    grepl(pattern = "tD1", pkParameterName)
  )
  if (all(isEmpty(endTime), parameterFromSimTime)) {
    logInfo(messages$infEndTime(pkParameterName))
    return(invisible(NULL))
  }
  extrapolatedParameter <- isIncluded(
    pkParameterName,
    c("AUC_inf", "AUC_inf_norm", "CL", "Thalf", "MRT", "Vd", "Vss", "FractionAucLastToInf")
  )
  if (all(!isEmpty(endTime), extrapolatedParameter)) {
    warning(messages$warningNumericEndTime(pkParameterName, endTime), call. = FALSE)
    return(invisible(NULL))
  }
  return(invisible(NULL))
}
