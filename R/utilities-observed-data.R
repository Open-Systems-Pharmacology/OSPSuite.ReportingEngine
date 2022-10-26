#' @title getReaderFunction
#' @description
#' Get most appropriate reader function by guessing file separator
#' File separator is guessed by checking number of fields/columns along lines
#' @param fileName Name of file to be read
#' @param nlines Number of lines to look at for checking consistency within file.
#' Note that, unlike `read.csv`, `count.fields` does not have options for handling encoding or escape characters.
#' Thus, using `nlines` as low as possible reduces the chances of inconsistent column widths caused by such characters.
#' @return Reader function such as `read.csv`
#' @keywords internal
getReaderFunction <- function(fileName, nlines = 2) {
  # Define mapping between reader functions and their separator
  # Default for read.csv is comma separator and point decimal
  # Default for read.csv2 is semicolon separator and comma decimal
  # Default for read.table is white space separator and point decimal
  readerMapping <- data.frame(
    sep = c(",", ";", ""),
    functionName = c("read.csv", "read.csv2", "read.table"),
    stringsAsFactors = FALSE
  )
  # For csv files, do not include white space separator
  if (isFileExtension(fileName, "csv")) {
    readerMapping <- readerMapping[1:2, ]
  }

  # Keep separator that would provides the most fields/columns
  sepWidth <- sapply(
    readerMapping$sep,
    function(sep) {
      # if count.fields notices an odd number of ' or ",
      # it will return NA for the line (example, "St John's" -> NA)
      # which needs to be removed from the count
      max(count.fields(fileName, sep = sep, comment.char = "", quote = '\"'), na.rm = TRUE)
    }
  )
  # which.max returns the first max value.
  # Thus, read.csv will be used in priority if same number of columns
  # are identified by each method
  readerMapping <- readerMapping[which.max(sepWidth), ]

  # Assess if selected separator reads a consistent number of fields/columns along lines
  fields <- count.fields(fileName, sep = readerMapping$sep, comment.char = "", quote = '\"')
  fields <- fields[!is.na(fields)]
  consistentFields <- isOfLength(unique(fields), 1)

  # If selected separator leads to inconsistent number of columns,
  # Throw a meaningful error message before error happens in read.table or later
  if (!consistentFields) {
    stop(messages$errorInconsistentFields(fileName))
  }

  # match.fun get actual function from function name
  return(match.fun(readerMapping$functionName))
}

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
  validateFileExists(fileName)
  # Get function with the most appropriate reading defaults
  readObservedData <- getReaderFunction(fileName)
  observedData <- readObservedData(
    fileName,
    header = header,
    check.names = FALSE,
    encoding = encoding,
    stringsAsFactors = FALSE
  )

  # For some cases where data was derived from Excel,
  # <U+FEFF> is included in first variable name and needs to be removed
  forbiddenCharacters <- "\ufeff"
  variableNames <- names(observedData)
  variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
  names(observedData) <- variableNames
  return(observedData)
}

#' @title getSelectedData
#' @description
#' Get selected data 
#' The function leverage `dplyr::filter` to select the data
#' @param data A data.frame 
#' @param dataSelection Character string or expression evaluated to select data
#' The enum helper `DataSelectionKeys` provides keys for selected all or none of the data
#' @return A data.frame of selected data
#' @export
#' @import dplyr
#' @seealso DataSelectionKeys
#' @examples
#' data <- data.frame(
#' x = seq(0,9),
#' y = seq(10,19),
#' mdv = c(1,1, rep(0, 8)),
#' groups = rep(c("A", "B"), 5)
#' )
#' 
#' # Select all the data
#' getSelectedData(data, DataSelectionKeys$ALL)
#' 
#' # Select no data
#' getSelectedData(data, DataSelectionKeys$NONE)
#' 
#' # Select data from group A
#' getSelectedData(data, "groups %in% 'A'")
#' 
#' # Remove missing dependent variable (mdv)
#' getSelectedData(data, "mdv == 0")
#' 
getSelectedData <- function(data, dataSelection) {
  if(isEmpty(dataSelection)){
    return(data[FALSE,])
  }
  if(isIncluded(dataSelection, DataSelectionKeys$ALL)){
    return(data)
  }
  if(isIncluded(dataSelection, c(DataSelectionKeys$NONE, "", "()"))){
    return(data[FALSE,])
  }
  if(isOfType(dataSelection, "expression")){
    return(data %>% dplyr::filter(eval(dataSelection)))
  }
  return(data %>% dplyr::filter(eval(parse(text = dataSelection))))
}

#' @title getSelectedRows
#' @description
#' Get selected rows from data and its selection
#' The function leverage `dplyr::filter` to select the rows
#' @param data A data.frame 
#' @param dataSelection Character string or expression evaluated to select data
#' The enum helper `DataSelectionKeys` provides keys for selected all or none of the data
#' @return A data.frame of selected data
#' @export
#' @import dplyr
#' @seealso DataSelectionKeys
#' @examples
#' data <- data.frame(
#' x = seq(0,9),
#' y = seq(10,19),
#' mdv = c(1,1, rep(0, 8)),
#' groups = rep(c("A", "B"), 5)
#' )
#' 
#' # Select all the rows
#' getSelectedRows(data, DataSelectionKeys$ALL)
#' 
#' # Select no row
#' getSelectedRows(data, DataSelectionKeys$NONE)
#' 
#' # Select rows from group A
#' getSelectedData(data, "groups %in% 'A'")
#' 
#' # Get rows of missing dependent variable (mdv)
#' getSelectedRows(data, "mdv == 0")
#' 
getSelectedRows <- function(data, dataSelection) {
  if(isEmpty(dataSelection)){
    return(FALSE)
  }
  if(isIncluded(dataSelection, DataSelectionKeys$ALL)){
    return(TRUE)
  }
  if(isIncluded(dataSelection, c(DataSelectionKeys$NONE, "", "()"))){
    return(FALSE)
  }
  if(isOfType(dataSelection, "expression")){
    selectedData <- data %>% 
      dplyr::mutate(rows = 1:n()) %>% 
      dplyr::filter(eval(dataSelection))
    return(selectedData$rows)
  }
  selectedData <- data %>% 
    dplyr::mutate(rows = 1:n()) %>% 
    dplyr::filter(eval(parse(text = dataSelection)))
  return(selectedData$rows)
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
  .Deprecated("getSelectedRows")
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
  datasetColumn = "datasetColumn",
  datasetUnit = "datasetUnit",
  timeID = "time",
  dvID = "dv",
  lloqID = "lloq",
  timeUnitID = "time_unit",
  dvUnitID = "dv_unit"
)

getDictionaryVariable <- function(dictionary, variableID) {
  variableMapping <- dictionary[, dictionaryParameters$ID] %in% variableID
  variableName <- as.character(dictionary[variableMapping, dictionaryParameters$datasetColumn])
  if (isOfLength(variableName, 0)) {
    return()
  }
  return(variableName)
}

#' @title loadObservedDataFromSimulationSet
#' @description
#' Load observed data and its dataMapping from a simulationSet
#' @param simulationSet A `SimulationSet` object
#' @return list of data and dataMapping
#' @keywords internal
loadObservedDataFromSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  # Observed data and dictionary are already checked when creating the simulationSet
  # No observed data return NULL
  if (isEmpty(simulationSet$observedDataFile)) {
    return()
  }

  re.tStoreFileMetadata(access = "read", filePath = simulationSet$observedDataFile)
  observedDataset <- readObservedDataFile(simulationSet$observedDataFile)
  observedDataset <- getSelectedData(observedDataset, simulationSet$dataSelection)
  re.tStoreFileMetadata(access = "read", filePath = simulationSet$observedMetaDataFile)
  dictionary <- readObservedDataFile(simulationSet$observedMetaDataFile)

  # Enforce datasetUnit column to exist
  if (!isIncluded(dictionaryParameters$datasetUnit, names(dictionary))) {
    dictionary[, dictionaryParameters$datasetUnit] <- NA
  }
  # Use dictionary to map the data and get the unit
  # Note that lloqColumn, timeUnitColumn and dvUnitColumn can be NULL
  timeColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeID)
  dvColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvID)
  lloqColumn <- getDictionaryVariable(dictionary, dictionaryParameters$lloqID)
  timeUnitColumn <- getDictionaryVariable(dictionary, dictionaryParameters$timeUnitID)
  dvUnitColumn <- getDictionaryVariable(dictionary, dictionaryParameters$dvUnitID)

  # Units: convert the observed data into base unit
  # Get values of unit column using datasetUnit
  timeMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$timeID
  timeUnit <- as.character(dictionary[timeMapping, dictionaryParameters$datasetUnit])
  if (!any(is.na(timeUnit), isIncluded(timeUnit, ""))) {
    timeUnitColumn <- "timeUnit"
    observedDataset[, timeUnitColumn] <- timeUnit
  }
  dvMapping <- dictionary[, dictionaryParameters$ID] %in% dictionaryParameters$dvID
  dvUnit <- as.character(dictionary[dvMapping, dictionaryParameters$datasetUnit])
  if (!any(is.na(dvUnit), isIncluded(dvUnit, ""))) {
    dvUnitColumn <- "dvUnit"
    observedDataset[, dvUnitColumn] <- dvUnit
  }

  # Parse the data.frame with the appropriate columns and ensure units are "character" type
  observedDataset[, timeUnitColumn] <- as.character(observedDataset[, timeUnitColumn])
  observedDataset[, dvUnitColumn] <- as.character(observedDataset[, dvUnitColumn])

  # If unit was actually defined using output objects, overwrite current dvUnit
  for (output in simulationSet$outputs) {
    if (isEmpty(output$dataUnit)) {
      next
    }
    selectedRows <- getSelectedRows(observedDataset, output$dataSelection)
    observedDataset[selectedRows, dvUnitColumn] <- output$dataUnit
  }

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
    if (isEmpty(dvDimension)) {
      logDebug(messages$unknownUnitInObservedData(dvUnit))
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
      logDebug(messages$lloqColumnNotFound(lloqColumn))
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
#' @param structureSet A `SimulationStructure` object
#' @return list of data and lloq data.frames
#' @keywords internal
getObservedDataFromOutput <- function(output, data, dataMapping, molWeight, structureSet) {
  # If no observed data nor data selected, return empty dataset
  if (isEmpty(data)) {
    return()
  }
  selectedData <- getSelectedData(data, output$dataSelection)
  logDebug(messages$selectedObservedDataForPath(output$path, nrow(selectedData)))
  if (isEmpty(selectedData)) {
    return()
  }

  # Get dimensions of observed data
  dvDimensions <- unique(as.character(selectedData[, dataMapping$dimension]))
  outputConcentration <- selectedData[, dataMapping$dv]
  if (!isEmpty(output$displayUnit)) {
    for (dvDimension in dvDimensions) {
      if (is.na(dvDimension)) {
        next
      }
      dvSelectedRows <- selectedData[, dataMapping$dimension] %in% dvDimension
      outputConcentration[dvSelectedRows] <- ospsuite::toUnit(
        dvDimension,
        outputConcentration[dvSelectedRows],
        output$displayUnit,
        molWeight = molWeight
      )
    }
  }
  outputData <- data.frame(
    "Time" = ospsuite::toUnit(
      "Time", 
      selectedData[, dataMapping$time], 
      structureSet$simulationSet$timeUnit
      ),
    "Concentration" = outputConcentration,
    "Legend" = captions$plotGoF$observedLegend(
      simulationSetName = structureSet$simulationSet$simulationSetName, 
      descriptor = structureSet$simulationSetDescriptor, 
      pathName = output$dataDisplayName
    ),
    "Path" = output$path
  )
  if (isEmpty(dataMapping$lloq)) {
    return(list(data = outputData, lloq = NULL))
  }

  lloqConcentration <- selectedData[, dataMapping$lloq]
  if (!isEmpty(output$displayUnit)) {
    for (dvDimension in dvDimensions) {
      if (is.na(dvDimension)) {
        next
      }
      dvSelectedRows <- selectedData[, dataMapping$dimension] %in% dvDimension
      lloqConcentration[dvSelectedRows] <- ospsuite::toUnit(
        dvDimension,
        lloqConcentration[dvSelectedRows],
        output$displayUnit,
        molWeight = molWeight
      )
    }
  }
  lloqOutput <- data.frame(
    "Time" = ospsuite::toUnit(
      "Time", 
      selectedData[, dataMapping$time], 
      structureSet$simulationSet$timeUnit
    ),
    "Concentration" = lloqConcentration,
    "Legend" = captions$plotGoF$lloqLegend(
      simulationSetName = structureSet$simulationSet$simulationSetName, 
      descriptor = structureSet$simulationSetDescriptor, 
      pathName = output$dataDisplayName
    ),
    "Path" = output$path
  )
  return(list(data = outputData, lloq = lloqOutput))
}


#' @title getObservedDataFromConfigurationPlan
#' @description
#' Get selected observed data from a `ConfigurationPlan` object
#' @param observedDataId Identifier of observed data
#' @param configurationPlan A `ConfigurationPlan` object that includes methods to find the data
#' @return list of including data and metaData to perform time profile plot
#' @keywords internal
getObservedDataFromConfigurationPlan <- function(observedDataId, configurationPlan) {
  observedDataFile <- configurationPlan$getObservedDataPath(observedDataId)
  observedData <- readObservedDataFile(observedDataFile)
  observedMetaData <- parseObservationsDataFrame(observedData)

  # In qualification workflow, observed data expected as:
  # Column 1: Time
  # Column 2: Observed variable
  # Column 3: uncertainty around observed variable
  logDebug(messages$sizeForObservedDataId(observedDataId, ncol(observedData), nrow(observedData)))

  return(list(
    data = observedData,
    metaData = observedMetaData
  ))
}

#' @title isObservedData
#' @description
#' Check if a configuration plan quantitiy path corresponds to observed data
#' @param path A quantitiy path from the configuration plan
#' For instance, "S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)"
#' or "Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc"
#' @return A logical checking if path corresponds to observed data
#' @import ospsuite
#' @examples
#' \dontrun{
#' isObservedData("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)")
#' # > FALSE
#' isObservedData("Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc")
#' # > TRUE
#' }
#' @keywords internal
isObservedData <- function(path) {
  pathArray <- ospsuite::toPathArray(path)
  isIncluded(pathArray[2], "ObservedData")
}

#' @title getObservedDataIdFromPath
#' @description
#' Get an observed dataset id from a configuration plan quantity path
#' @param path A quantitiy path from the configuration plan
#' For instance, "S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)"
#' or "Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc"
#' @return A string corresponding to the `id` of a configuration plan observed dataset
#' @import ospsuite
#' @examples
#' \dontrun{
#' getObservedDataIdFromPath("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma")
#' # > NULL
#' getObservedDataIdFromPath("Midazolam 600mg SD|ObservedData|Plasma|Rifampin|Conc")
#' # > "Midazolam 600mg SD"
#' }
#' @keywords internal
getObservedDataIdFromPath <- function(path) {
  if (!isObservedData(path)) {
    return(NULL)
  }
  pathArray <- ospsuite::toPathArray(path)
  return(pathArray[1])
}

#' @title translateDataSelection
#' @description
#' Translate `dataSelection` input by user into characters/expression understood by `getSelectedData`
#' @param dataSelection characters or expression to select subset the observed data
#' @return characters or expression to select subset the observed data
#' @keywords internal
translateDataSelection <- function(dataSelection){
  validateIsOfType(dataSelection, c("character", "expression"), nullAllowed = TRUE)
  if (!isOfType(dataSelection, "character")) {
    return(dataSelection)
  }
  # If any selection include None, do not select anything
  if (isIncluded(DataSelectionKeys$NONE, dataSelection)) {
    return(FALSE)
  }
  # By removing "" string from dataSelection
  # If "" is the only value provided, dataSelection isEmpty
  # If multiple values provided, concatenate the remaining selections
  dataSelection <- dataSelection[!(dataSelection %in% "")]
  # When concatenating, ALL won't be understood by dplyr
  # Needs to be replaced by true to select all data
  dataSelection[dataSelection %in% DataSelectionKeys$ALL] <- TRUE
  # Concatenate selections using & and brackets
  dataSelection <- paste(dataSelection, collapse = ") & (")
  return(paste0("(", dataSelection, ")"))
}