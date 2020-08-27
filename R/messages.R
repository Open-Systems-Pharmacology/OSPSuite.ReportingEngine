messages <- list(
  errorWrongType = function(objectName, type, expectedType, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    expectedTypeMsg <- paste0(expectedType, collapse = ", or ")

    paste0(
      callingFunction, ": argument '", objectName,
      "' is of type '", type, "', but expected '", expectedTypeMsg, "'!", optionalMessage
    )
  },

  errorDifferentLength = function(objectNames, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    paste0(
      callingFunction, ": Arguments '", objectNames,
      "' must have the same length, but they don't!", optionalMessage
    )
  },

  errorDuplicatedEntries = function(objectNames, optionalMessage = NULL) {
    paste(objectNames, "contains duplicated elements.")
  },

  errorWrongLength = function(object, nbElements, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]
    paste0(
      callingFunction, ": Object should be of length '", nbElements, "', but is of length '", length(object), "' instead. ", optionalMessage
    )
  },

  errorEnumNotAllNames = "The enumValues has some but not all names assigned. They must be all assigned or none assigned",

  errorValueNotInEnum = function(enum, value) {
    paste0("Value '", value, "' is not in defined enumeration values: '", paste0(names(enum), collapse = ", "), "'.")
  },

  errorNotIncluded = function(values, parentValues, groupName = NULL) {
    if (!is.null(groupName)) {
      return(paste0("Values '", paste0(values, collapse = "', '"), "' are not all included in ", groupName, "."))
    }
    paste0("Values '", paste0(values, collapse = "', '"), "' are not all included in parent values: '", paste0(parentValues, collapse = "', '"), "'.")
  },

  warningExistingPath = function(existingPath) {
    paste0("Path: '", existingPath, "' already exists.")
  },

  warningPathIncludes = function(path) {
    paste0(
      "Path: '", path, "' includes the following files and directories which may be used or overwritten: '",
      paste0(list.files(path, include.dirs = TRUE), collapse = "', '"),
      "'."
    )
  },

  warningOverwriting = function(overwrittenPath) {
    paste0("Overwriting path: '", overwrittenPath, "'.")
  },

  errorExtension = function(path, extension) {
    paste0("File extension: '", paste0(extension, collapse = "', '."), "' is required. File path was : '", path, "'.")
  },

  errorTaskInputDoesNotExist = function(input) {
    paste0("Task input file '", input, "' does not exist.")
  },

  errorObservedMetaDataFileNotProvided = function(observedDataFile) {
    paste0(
      "observedDataFile '", observedDataFile, "' was input but not observedMetaDataFile.",
      "\nobservedMetaDataFile is required when using observedDataFile."
    )
  },

  errorNoParametersForSensitivityAnalysis = function() {
    paste0("No variable parameters found for sensitivity analysis.")
  },

  errorNoValidParametersForSensitivityAnalysis = function(simulationSetName) {
    paste("No valid variable parameter paths for sensitivity analysis of simulation set", simulationSetName, ".")
  },

  warningIgnoringInvalidParametersForSensitivityAnalysis = function(invalidParameterPaths, simulationSetName) {
    paste("Ignoring parameters", paste0("'", invalidParameterPaths, "'", collapse = ", "), "in sensitivity analysis for simulation set", simulationSetName, ".")
  },

  warningNoReferencePopulation = function(workflowType) {
    paste0("Workflow type '", workflowType, "' requires one unique reference population, but either none or multiple reference populations provided.")
  },

  errorNotADimension = function(values) {
    paste0("Expected a dimension. Check that '", paste0(values, collapse = "', '."), "' is included in ospsuite::allAvailableDimensions().")
  },

  invalidOuputPath = function(path, simName) {
    paste0("'", path, "' is an invalid output path for simulation '", simName, "'.")
  },

  outsideRange = function(variableName, value, lowerBound, upperBound) {
    paste0(variableName, " has value ", value, ", which lies outside the allowable range [", lowerBound, ",", upperBound, "].")
  },

  errorUnitNotFromDimension = function(unit, dimension) {
    paste0("Unit '", paste0(unit, collapse = "', '"), "' is not included in available units for dimension: '", paste0(dimension, collapse = "', '"), "'.")
  },

  warningLogScaleNoPositiveData = function(variableName) {
    paste0(variableName, " does not include any positive data. Logarithmic scale plot cannot be output")
  },
  
  errorNotSameOutputsBetweenSets = function(setNames) {
    paste0("Simulation sets '", paste0(setNames, collapse = "', '"), "' require same outputs and PK parameters.")
  }
)
