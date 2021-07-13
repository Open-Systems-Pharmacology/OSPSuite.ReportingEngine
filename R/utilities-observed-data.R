#' @title readObservedDataFile
#' @description
#' Read observed data file with Nonmem format.
#' Can read csv files as well as
#' @param fileName name of file to be read
#' @param header logical indicating if data has a header
#' @param encoding encoding of the file
#' @return data.frame containing observed data
#' @export
readObservedDataFile <- function(fileName,
                                 header = TRUE,
                                 encoding = "UTF-8") {
  extension <- fileExtension(fileName)
  # For some cases where data was derived from Excel,
  # <U+FEFF> is included in first variable name and needs to be removed
  forbiddenCharacters <- "\ufeff"

  if (extension %in% "csv") {
    observedData <- read.csv(fileName,
      header = header,
      check.names = FALSE,
      encoding = encoding
    )
    variableNames <- names(observedData)
    variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
    names(observedData) <- variableNames
    return(observedData)
  }

  observedData <- read.table(fileName,
    header = header,
    check.names = FALSE,
    encoding = encoding
  )
  variableNames <- names(observedData)
  variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
  names(observedData) <- variableNames
  return(observedData)
}

#' @title evalDataFilter
#' @description
#' Evaluate a data filter by converting the variable names of the data.frame
#' into names of variables to be evaluated in the filter expression.
#' @param data data.frame containing observed data
#' @param filterExpression expression
#' character string filter to be applied
#' @return vector of logicals corresponding to the evaluation of the filter
#' @export
evalDataFilter <- function(data, filterExpression) {
  variableNames <- names(data)
  expressionList <- lapply(
    variableNames,
    function(variableName) {
      parse(text = paste0(variableName, '<- data[,"', variableName, '"]'))
    }
  )

  for (dataExpression in expressionList) {
    eval(dataExpression)
  }

  return(eval(filterExpression))
}


dictionaryParameters <- list(
  ID = "ID",
  nonmenColumn = "nonmenColumn",
  nonmemUnit = "nonmemUnit",
  timeID = "time",
  dvID = "dv",
  lloqID = "lloq",
  timeUnitID = "time_unit",
  dvUnitID = "dv_unit"
)

getDictionaryVariable <- function(dictionary, variableID) {
  variableMapping <- dictionary[, dictionaryParameters$ID] %in% variableID
  variableName <- as.character(dictionary[variableMapping, dictionaryParameters$nonmenColumn])
  if (isOfLength(variableName, 0)) {
    return()
  }
  return(variableName)
}

#' @title loadObservedDataFromSimulationSet
#' @description
#' Load observed data and its dataMapping from a simulationSet
#' @param simulationSet A `SimulationSet` object
#' @param logFolder folder where the logs are saved
#' @return list of data and dataMapping
loadObservedDataFromSimulationSet <- function(simulationSet, logFolder) {
  validateIsOfType(simulationSet, "SimulationSet")
  # Observed data and dictionary are already checked when creating the simulationSet
  # No observed data return NULL
  if (isOfLength(simulationSet$observedDataFile, 0)) {
    return()
  }

  re.tStoreFileMetadata(access = "read", filePath = simulationSet$observedDataFile)
  observedDataset <- readObservedDataFile(simulationSet$observedDataFile)
  re.tStoreFileMetadata(access = "read", filePath = simulationSet$observedMetaDataFile)
  dictionary <- readObservedDataFile(simulationSet$observedMetaDataFile)

  # Enforce nonmemUnit column to exist
  if (!isIncluded(dictionaryParameters$nonmemUnit, names(dictionary))) {
    dictionary[, dictionaryParameters$nonmemUnit] <- NA
  }
  # Use dictionary to map the data and get the unit
  # Note that lloqColumn, timeUnitColumn and dvUnitColumn can be NULL
  timeColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
  dvColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
  lloqColumn <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)
  timeUnitColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeUnitID)
  dvUnitColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvUnitID)

  # Units: convert the observed data into base unit
  # Get values of unit column using nonmemUnit
  timeMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$timeID
  timeUnit <- as.character(dictionary[timeMapping, dictionaryParameters$nonmemUnit])
  if (!any(is.na(timeUnit), isIncluded(timeUnit, ""))) {
    timeUnitColumn <- "timeUnit"
    observedDataset[, timeUnitColumn] <- timeUnit
  }
  dvMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$dvID
  dvUnit <- as.character(dictionary[dvMapping, dictionaryParameters$nonmemUnit])
  if (!any(is.na(dvUnit), isIncluded(dvUnit, ""))) {
    dvUnitColumn <- "dvUnit"
    observedDataset[, dvUnitColumn] <- dvUnit
  }

  # Parse the data.frame with the appropriate columns and ensure units are "character" type
  observedDataset[, timeUnitColumn] <- as.character(observedDataset[, timeUnitColumn])
  observedDataset[, dvUnitColumn] <- as.character(observedDataset[, dvUnitColumn])

  # Convert observed data to base unit,
  # as.numeric needs to be enforced because toBaseUnit could think values are integer and crash
  for (timeUnit in unique(observedDataset[, timeUnitColumn])) {
    selectedRows <- observedDataset[, timeUnitColumn] %in% timeUnit
    observedDataset[selectedRows, timeColumn] <- ospsuite::toBaseUnit(
      "Time",
      as.numeric(observedDataset[selectedRows, timeColumn]),
      timeUnit
    )
  }
  # Initialize a dimension column for dV
  observedDataset$dimension <- NA
  for (dvUnit in unique(observedDataset[, dvUnitColumn])) {
    dvDimension <- ospsuite::getDimensionForUnit(dvUnit)
    if (isOfLength(dvDimension, 0)) {
      logWorkflow(
        message = paste0("In loadObservedDataFromSimulationSet: unit '", dvUnit, "' is unknown."),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )
      next
    }
    selectedRows <- observedDataset[, dvUnitColumn] %in% dvUnit
    observedDataset[selectedRows, dvColumn] <- ospsuite::toBaseUnit(
      dvDimension,
      as.numeric(observedDataset[selectedRows, dvColumn]),
      dvUnit
    )
    observedDataset$dimension[selectedRows] <- dvDimension
    if (isOfLength(lloqColumn, 0)) {
      next
    }
    # Case where dictionary defined an lloq column missing from dataset
    if (!isIncluded(lloqColumn, names(observedDataset))) {
      logWorkflow(
        message = paste0("lloq variable '", lloqColumn, "' defined in dictionary is missing from observed dataset"),
        pathFolder = logFolder,
        logTypes = LogTypes$Debug
      )
      lloqColumn <- NULL
      next
    }

    observedDataset[selectedRows, lloqColumn] <- ospsuite::toBaseUnit(
      ospsuite::getDimensionForUnit(dvUnit),
      as.numeric(observedDataset[selectedRows, lloqColumn]),
      dvUnit
    )
  }
  # Create a dataMapping variable
  # Dimension will be used to find which base unit is in the data
  dataMapping <- list(
    time = timeColumn,
    dv = dvColumn,
    lloq = lloqColumn,
    dimension = "dimension"
  )

  return(list(
    data = observedDataset,
    dataMapping = dataMapping
  ))
}

#' @title getObservedDataFromOutput
#' @description
#' Get selected observed data from an Output object
#' @param output An `Output` object
#' @param data A data.frame
#' @param dataMapping A list mapping the variable of data
#' @param molWeight Molar weight for unit conversion of dependent variable
#' @param timeUnit time unit for unit conversion of time
#' @param logFolder folder where the logs are saved
#' @return list of data and lloq data.frames
getObservedDataFromOutput <- function(output, data, dataMapping, molWeight, timeUnit, logFolder) {
  if (isOfLength(output$dataSelection, 0)) {
    return()
  }

  selectedRows <- evalDataFilter(data, output$dataSelection)
  logWorkflow(
    message = paste0("Output '", output$path, "'. Number of selected observations: ", sum(selectedRows)),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  # Get dimensions of observed data
  dvDimensions <- unique(as.character(data[selectedRows, dataMapping$dimension]))
  outputConcentration <- data[selectedRows, dataMapping$dv]
  if (!isOfLength(output$displayUnit, 0)) {
    for (dvDimension in dvDimensions) {
      if (is.na(dvDimension)) {
        next
      }
      dvSelectedRows <- data[selectedRows, dataMapping$dimension] %in% dvDimension
      outputConcentration[dvSelectedRows] <- ospsuite::toUnit(
        dvDimension,
        outputConcentration[dvSelectedRows],
        output$displayUnit,
        molWeight = molWeight
      )
    }
  }
  outputData <- data.frame(
    "Time" = ospsuite::toUnit("Time", data[selectedRows, dataMapping$time], timeUnit),
    "Concentration" = outputConcentration,
    "Legend" = paste0("Observed data ", output$dataDisplayName),
    "Path" = output$path
  )
  if (isOfLength(dataMapping$lloq, 0)) {
    return(list(data = outputData, lloq = NULL))
  }

  lloqConcentration <- data[selectedRows, dataMapping$lloq]
  if (!isOfLength(output$displayUnit, 0)) {
    for (dvDimension in dvDimensions) {
      if (is.na(dvDimension)) {
        next
      }
      dvSelectedRows <- data[selectedRows, dataMapping$dimension] %in% dvDimension
      lloqConcentration[dvSelectedRows] <- ospsuite::toUnit(
        dvDimension,
        lloqConcentration[dvSelectedRows],
        output$displayUnit,
        molWeight = molWeight
      )
    }
  }
  lloqOutput <- data.frame(
    "Time" = ospsuite::toUnit("Time", data[selectedRows, dataMapping$time], timeUnit),
    "Concentration" = lloqConcentration,
    "Legend" = "LLOQ",
    "Path" = output$path
  )
  return(list(data = outputData, lloq = lloqOutput))
}


#' @title getObservedDataFromConfigurationPlan
#' @description
#' Get selected observed data from a `ConfigurationPlan` object
#' @param observedDataId Identifier of observed data
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find the data
#' @param logFolder folder where the logs are saved
#' @return list of including data and metaData to perform time profile plot
getObservedDataFromConfigurationPlan <- function(observedDataId, configurationPlan, logFolder) {
  observedDataFile <- configurationPlan$getObservedDataPath(observedDataId)
  observedData <- readObservedDataFile(observedDataFile)
  observedMetaData <- parseObservationsDataFrame(observedData)
  
  # In qualification workflow, observed data expected as:
  # Column 1: Time
  # Column 2: Observed variable
  # Column 3: uncertainty around observed variable
  numberOfColumns <- ncol(observedData)
  numberOfRows <- nrow(observedData)

  # Log the description of the observed data for debugging
  logWorkflow(
    message = paste0("Observed data Id '", observedDataId, "' included ", numberOfColumns, " columns and ", numberOfRows, " rows"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  return(list(
    data = observedData,
    metaData = observedMetaData
  ))
}
