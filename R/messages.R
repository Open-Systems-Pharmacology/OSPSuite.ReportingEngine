messages <- list(
  errorWrongType = function(objectName, type, expectedType, optionalMessage = NULL) {
    expectedTypeMsg <- paste0(expectedType, collapse = ", or ")
    paste0(
      callingFunction(), "argument '", objectName,
      "' is of type '", type, "', but expected '", expectedTypeMsg, "'!", optionalMessage
    )
  },

  errorDifferentLength = function(objectNames, optionalMessage = NULL) {
    paste0(
      callingFunction(), "Arguments '", objectNames,
      "' must have the same length, but they don't!", optionalMessage
    )
  },

  errorDuplicatedEntries = function(objectNames, optionalMessage = NULL) {
    paste(objectNames, "contains duplicated elements.")
  },

  errorWrongLength = function(object, nbElements, optionalMessage = NULL) {
    paste0(
      callingFunction(), "Object should be of length '", nbElements, "', but is of length '", length(object), "' instead. ", optionalMessage
    )
  },

  errorEnumNotAllNames = "The enumValues has some but not all names assigned. They must be all assigned or none assigned",

  errorValueNotInEnum = function(enum, value) {
    paste0(callingFunction(), "Value '", value, "' is not in defined enumeration values: '", paste0(names(enum), collapse = ", "), "'.")
  },

  errorNotIncluded = function(values, parentValues, groupName = NULL) {
    if (ospsuite.utils::isOfLength(values, 1)) {
      if (!is.null(groupName)) {
        return(paste0(callingFunction(), "'", values, "' is not included in ", groupName, "."))
      }
      return(paste0(callingFunction(), "'", values, "' is not included in expected values: '", paste0(parentValues, collapse = "', '"), "'."))
    }
    if (!is.null(groupName)) {
      return(paste0(callingFunction(), "Values '", paste0(values, collapse = "', '"), "' are not all included in ", groupName, "."))
    }
    paste0(callingFunction(), "Values '", paste0(values, collapse = "', '"), "' are not all included in expected values: '", paste0(parentValues, collapse = "', '"), "'.")
  },

  errorNotIncludedInDataset = function(columnNames, dataset, datasetName = NULL) {
    if (ospsuite.utils::isOfLength(columnNames, 1)) {
      return(paste0(callingFunction(), "'", columnNames, "' column is not present in ", datasetName, " columns: '", paste0(names(dataset), collapse = "', '"), "'."))
    }
    paste0(callingFunction(), "'", paste0(columnNames, collapse = "', '"), "' columns are not all present in ", datasetName, " columns: '", paste0(names(dataset), collapse = "', '"), "'.")
  },

  warningExistingPath = function(existingPath) {
    paste0(callingFunction(), "Path: '", existingPath, "' already exists.")
  },

  warningPathIncludes = function(path) {
    paste0(
      callingFunction(), "Path: '", path, "' includes the following files and directories which may be used or overwritten: '",
      paste0(list.files(path, include.dirs = TRUE), collapse = "', '"),
      "'."
    )
  },

  warningOverwriting = function(overwrittenPath) {
    paste0(callingFunction(), "Overwriting path: '", overwrittenPath, "'.")
  },

  errorExtension = function(path, extension) {
    paste0(callingFunction(), "File extension: '", paste0(extension, collapse = "', '."), "' is required. File path was : '", path, "'.")
  },

  errorTaskInputDoesNotExist = function(input) {
    paste0(callingFunction(), "Task input file '", input, "' does not exist.")
  },

  errorObservedMetaDataFileNotProvided = function(observedDataFile) {
    paste0(
      callingFunction(), "Argument 'observedMetaDataFile' is required when argument 'observedDataFile' is defined.",
      "\nPlease provide a dictionary for observedDataFile '", observedDataFile, "'."
    )
  },

  errorNoDataUnit = function() {
    paste0(
      callingFunction(),
      "No definition provided for units of observed dataset.\n",
      "Please provide units for both '", dictionaryParameters$timeID, "' and '", dictionaryParameters$dvID, "' using methods available among the methods below:\n",
      "1) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetUnit, "' for ID '", dictionaryParameters$timeID, "' and '", dictionaryParameters$dvID, "'\n",
      "2) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetColumn, "' for ID '", dictionaryParameters$timeUnitID, "' and '", dictionaryParameters$dvUnitID, "'\n",
      "3) Define units for '", dictionaryParameters$dvID, "' in every 'Output' object using the field 'dataUnit'"
      )
  },

  errorNoDataUnitInOutputs = function() {
    paste0(
      callingFunction(),
      "Units for '", dictionaryParameters$dvID, "' were not defined in every 'Output' object, and neither defined using the dictionary\n",
      "Please provide units for both '", dictionaryParameters$timeID, "' and '", dictionaryParameters$dvID, "' using methods available among the methods below:\n",
      "1) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetUnit, "' for ID '", dictionaryParameters$timeID, "' and '", dictionaryParameters$dvID, "'\n",
      "2) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetColumn, "' for ID '", dictionaryParameters$timeUnitID, "' and '", dictionaryParameters$dvUnitID, "'\n",
      "3) Define units for '", dictionaryParameters$dvID, "' in 'Output' objects using the field 'dataUnit'"
      )
  },

  warningMultipleDataUnit = function() {
    paste0(
      callingFunction(),
      "Multiple definitions provided for units of observed dataset.\n",
      "Among the methods available below, the definition for units will use in priority method 3), then 2) and then 1).\n",
      "1) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetUnit, "' for ID '", dictionaryParameters$timeID, "' and '", dictionaryParameters$dvID, "'\n",
      "2) Define units in dictionary by filling the column '",
      dictionaryParameters$datasetColumn, "' for ID '", dictionaryParameters$timeUnitID, "' and '", dictionaryParameters$dvUnitID, "'\n",
      "3) Define units for '", dictionaryParameters$dvID, "' in every 'Output' object using the field 'dataUnit'"

    )
  },

  errorNoParametersForSensitivityAnalysis = function() {
    paste0(callingFunction(), "No variable parameters found for sensitivity analysis.")
  },

  warningNoFinitePKParametersForSomeIndividuals = function(pkParameter, output, simulationSetName) {
    paste0(callingFunction(), "PK parameter '", pkParameter, "' of output path '", output, "' could not be computed for some individuals in simulation set '", simulationSetName, "'.  See PK parameter calculation results.  Sensitivity analysis for this output and PK parameter combination will not be performed for simulation set '", simulationSetName, "'.")
  },

  warningPopulationSensitivityPlotsNotAvailable = function() {
    paste(callingFunction(), "Population sensitivity plots not available for the selected PK parameters.")
  },

  warningPopulationSensitivityPlotsNotAvailableForPKParameterOutputSimulationSet = function(pkParameter, output, simulationSetName) {
    paste0(callingFunction(), "No sensitivity analysis results found for PK parameter '", pkParameter, "' of output path '", output, "' in simulation set '", simulationSetName, "'.  No population sensitivity analysis plots will be generated for this output and PK parameter combination for simulation set '", simulationSetName, "'.")
  },

  errorNoValidParametersForSensitivityAnalysis = function(simulationSetName) {
    paste(callingFunction(), "No valid variable parameter paths for sensitivity analysis of simulation set", simulationSetName, ".")
  },

  warningIgnoringInvalidParametersForSensitivityAnalysis = function(invalidParameterPaths, simulationSetName) {
    paste(callingFunction(), "Ignoring parameters", paste0("'", invalidParameterPaths, "'", collapse = ", "), "in sensitivity analysis for simulation set", simulationSetName, ".")
  },

  warningNoReferencePopulation = function(workflowType) {
    paste0(callingFunction(), "Workflow type '", workflowType, "' requires one unique reference population, but either none or multiple reference populations provided.")
  },

  errorNotADimension = function(values) {
    paste0(callingFunction(), "Expected a dimension. Check that '", paste0(values, collapse = "', '."), "' is included in ospsuite::allAvailableDimensions().")
  },

  invalidOuputPath = function(path, simName) {
    paste0(callingFunction(), "'", path, "' is an invalid output path for simulation '", simName, "'.")
  },

  outsideRange = function(variableName, value, lowerBound, upperBound) {
    paste0(callingFunction(), variableName, " has value ", value, ", which lies outside the allowable range [", lowerBound, ",", upperBound, "].")
  },

  errorUnitNotFromDimension = function(unit, dimension) {
    paste0(callingFunction(), "Unit '", paste0(unit, collapse = "', '"), "' is not included in available units for dimension: '", paste0(dimension, collapse = "', '"), "'.")
  },

  warningLogScaleNoPositiveData = function(variableName) {
    paste0(callingFunction(), variableName, " does not include any positive data. Logarithmic scale plot cannot be output")
  },

  errorNotSameOutputsBetweenSets = function(setNames) {
    paste0(callingFunction(), "Simulation sets '", paste0(setNames, collapse = "', '"), "' require same outputs and PK parameters.  Verify the outputs and PK parameters of simulation sets using the function: 'getPKParametersInSimulationSet'.")
  },

  errorHasNoUniqueValues = function(data, dataName = "dataset", na.rm = TRUE) {
    if (na.rm) {
      data <- data[!is.na(data)]
    }
    return(paste0(callingFunction(), "Values '", paste0(data[duplicated(data)], collapse = "', '"), "' in ", dataName, " are not unique"))
  },

  dataIncludedInTimeRange = function(finalSize, initialSize, timeRange, timeUnit, dataType) {
    paste0(
      finalSize, " ", dataType,
      " data were included in the analysis between ", min(timeRange), " and ", max(timeRange), " ", timeUnit,
      ". Initial size was ", initialSize, "."
    )
  }
)

callingFunction <- function() {
  callingFunctions <- sys.calls()
  callingFunction <- deparse(sys.call(-length(callingFunctions) + 1)[[1]])
  return(paste0("In ", callingFunction, ":\n"))
}
