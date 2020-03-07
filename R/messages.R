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

  errorNotIncluded = function(values, parentValues) {
    paste0("Values '", paste0(values, collapse = ", "), "' are not in included in parent values: '", paste0(parentValues, collapse = ", "), "'.")
  },

  warningExistingPath = function(existingPath) {
    paste0("Path: '", existingPath, "' already exists.")
  },

  warningOverwriting = function(overwrittenPath) {
    paste0("Overwriting path: '", overwrittenPath, "'.")
  },

  errorExtension = function(path, extension) {
    paste0("File extension: '.", extension, "' is required. File name was : '", path, "'.")
  },

  errorTaskInputDoesNotExist = function(input) {
    paste0("Task input file '", input, "' does not exist.")
  },

  errorObservedMetaDataFileNotProvided = function() {
    '"ObservedDataFile" provided while "ObservedMetaDataFile" is "NULL", path of ObservedMetaDataFile need to be input'
  }
)
