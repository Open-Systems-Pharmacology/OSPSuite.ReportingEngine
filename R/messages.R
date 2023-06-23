messages <- list(
  #----- Error messages ----
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
    if (isOfLength(values, 1)) {
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
    if (isOfLength(columnNames, 1)) {
      return(paste0(callingFunction(), "'", columnNames, "' column is not present in ", datasetName, " columns: '", paste0(names(dataset), collapse = "', '"), "'."))
    }
    paste0(callingFunction(), "'", paste0(columnNames, collapse = "', '"), "' columns are not all present in ", datasetName, " columns: '", paste0(names(dataset), collapse = "', '"), "'.")
  },
  errorUnexistingFile = function(path) {
    paste0(callingFunction(), "File(s): '", paste0(path, collapse = "', '"), "' do not exist.")
  },
  errorExtension = function(path, extension) {
    paste0(callingFunction(), "File extension: '", paste0(extension, collapse = "', '."), "' is required. File path was : '", path, "'.")
  },
  errorTaskInputDoesNotExist = function(input) {
    paste0(callingFunction(), "Task input file(s) '", paste0(input, collapse = "', '."), "' do not exist.")
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
      messages$dataUnitMethods()
    )
  },
  errorNoDataUnitInOutputs = function() {
    paste0(
      callingFunction(),
      "Units for '", highlight(dictionaryParameters$dvID),
      "' were neither defined in dictionary, nor in all 'Output' objects\n",
      messages$dataUnitMethods()
    )
  },
  errorInconsistentDataUnit = function() {
    paste0(
      callingFunction(),
      "Units for '", highlight(dictionaryParameters$dvID),
      "' were defined in both dictionary and 'Output' objects but were not consistent.\n",
      messages$dataUnitMethods()
    )
  },
  errorWrongColumnTypeInDataFile = function(fileName, columnName, expectedType) {
    paste0("Column '", columnName, "', imported from file '", fileName, "', is not of the expected type '", expectedType, "'.")
  },
  errorRunSimulationsNotSuccessful = function(fileName, simulationSetName) {
    paste0("Simulation of model file '", fileName, "' from simulation set '", simulationSetName, "' was not completed successfully.")
  },
  errorNoParametersForSensitivityAnalysis = function() {
    paste0(callingFunction(), "No variable parameters found for sensitivity analysis.")
  },
  errorInconsistentFields = function(fileName) {
    paste0("Function 'readObservedDataFile' could not get a consistent number of columns for '", fileName, "'.")
  },
  errorNoValidParametersForSensitivityAnalysis = function(simulationSetName) {
    paste(callingFunction(), "No valid variable parameter paths for sensitivity analysis of simulation set", simulationSetName, ".")
  },
  invalidOuputPath = function(path, simName) {
    if (isOfLength(path, 1)) {
      return(paste0(callingFunction(), "'", path, "' is an invalid output path for simulation '", simName, "'."))
    }
    paths <- paste0(path, collapse = "', '")
    return(paste0(callingFunction(), "'", paths, "' are invalid output paths for simulation '", simName, "'."))
  },
  outsideRange = function(variableName, value, lowerBound, upperBound) {
    paste0(callingFunction(), variableName, " has value ", value, ", which lies outside the allowable range [", lowerBound, ",", upperBound, "].")
  },
  errorUnitNotFromDimension = function(unit, dimension) {
    paste0(callingFunction(), "Unit '", paste0(unit, collapse = "', '"), "' is not included in available units for dimension: '", paste0(dimension, collapse = "', '"), "'.")
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
  errorCommand = function(command, status) {
    paste0("Command : '", command, "' returned Error Status ", status)
  },
  ggsaveError = function(fileName, simulationSetName = NULL, errorMessage) {
    paste0(
      "Figure '", fileName, "'",
      ifNotNull(simulationSetName, paste0(" from simulation set '", simulationSetName, "'"), ""),
      " could not be saved.\nIn ggsave, ", errorMessage
    )
  },
  errorNotLoadedOnCores = function(objectNames) {
    paste0(objectNames, " NOT loaded successfully on all cores")
  },
  errorNotCompletedOnCores = function(objectNames) {
    paste0(objectNames, " NOT completed successfully on all cores")
  },
  errorMolecularWeightRequired = function(path) {
    paste0("Molecular weight not found but required for observed data Id '", path, "' in Time Profile plot.")
  },
  errorParametersNotIncludedInDDI = function(parameterNames){
    paste0(
      "PK parameters '", highlight(paste(parameterNames, collapse = "', '")),
      "' defined in ", highlight("GuestDelta"), " field but not in ", 
      highlight("PKParameters"), " field")
  },
  #----- Warning messages ----
  warningExistingPath = function(existingPath) {
    paste0(callingFunction(), "Path: '", existingPath, "' already exists.")
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
  warningPathIncludes = function(path) {
    paste0(
      callingFunction(), "Path: '", path, "' includes the following files and directories which may be used or overwritten: '",
      paste0(list.files(path, include.dirs = TRUE), collapse = "', '"),
      "'."
    )
  },
  warningMultipleDataUnit = function() {
    paste0(
      callingFunction(),
      "Multiple definitions provided for units of observed dataset.\n",
      messages$dataUnitMethods()
    )
  },
  warningOverwriting = function(overwrittenPath) {
    paste0(callingFunction(), "Overwriting path: '", overwrittenPath, "'.")
  },
  warningLogScaleNoPositiveData = function(variableName) {
    paste0(callingFunction(), highlight(variableName), " does not include any positive data. Logarithmic scale plot cannot be output")
  },
  warningNoReferencePopulation = function(workflowType) {
    paste0(callingFunction(), "Workflow type '", workflowType, "' requires one unique reference population, but either none or multiple reference populations provided.")
  },
  warningIgnoringInvalidParametersForSensitivityAnalysis = function(invalidParameterPaths, simulationSetName) {
    paste(callingFunction(), "Ignoring parameters", paste0("'", invalidParameterPaths, "'", collapse = ", "), "in sensitivity analysis for simulation set", simulationSetName, ".")
  },
  warningPKParameterNotEnoughData = function(pkParameter, path) {
    paste0(highlight(pkParameter), " for ", highlight(path), ": not enough available data to perform plot.")
  },
  warningApplicationsBeforeTimeOffset = function(applicationLength, timeRanges, timeUnit, timeOffset, simulationSetName) {
    paste0(
      highlight(applicationLength), " application(s) at ", highlight(timeRanges), " ", timeUnit,
      " were identified before '", highlight("timeOffset"), "' defined at ", timeOffset, timeUnit,
      " for simulation set '", highlight(simulationSetName), "'."
    )
  },
  warningPKRatioMultipleObservedRows = function(numberOfRows, observedId) {
    paste0(
      "In PK Ratio Plots, ", highlight(numberOfRows),
      " data record(s) found for ObservedDataRecordId '", highlight(observedId), "'"
    )
  },
  warningRightAxisNotAvailable = function(curveName) {
    paste0(
      "Right Axis not available yet in ", highlight("tlf"), " package,\n",
      highlight(curveName), "could not be added"
    )
  },
  warningNoAxesSettings = function(plotName, plotType = NULL) {
    if (is.null(plotType)) {
      return(paste0("No axes settings defined for plot: '", highlight(plotName), "'"))
    }
    return(paste0(
      "In ", highlight(plotType), ",\n",
      "No axes settings defined for plot: '", highlight(plotName), "'"
    ))
  },
  warningErrorAssumedArithmetic = function() {
    paste0(
      "Unit provided to observed data values ",
      "but not to its errors/SD while values are lower than 1.\n",
      "errors/SD were assumed arithmetic with same unit as observed data values"
    )
  },
  warningTooManyAxes = function(){
    "Qualification TimeProfile is unable to display data for all 3 axes. Only Y and Y2 axes were kept."
  },
  
  warningHasInfiniteValues = function(n, datasetName = NULL){
    paste0(
      "Data ", 
      ifNotNull(datasetName, paste0("from '", highlight(datasetName), "' "), ""),
      "included ", highlight(n), " infinite values"
    )
  },
  
  pkParameterNotFound = function(pkParameterName, pkRatioMapping){
    paste0(
      "PK Parameter '", highlight(pkParameterName), "' not found for ",
      "Project '", highlight(pkRatioMapping$Project), 
      "' and Simulation '", highlight(pkRatioMapping$Simulation), "'"
    )
  },

  #----- Info messages ----
  runStarting = function(runName, subRun = NULL) {
    if (is.null(subRun)) {
      return(paste0("Starting run of ", highlight(runName)))
    }
    return(paste0("Starting run of ", highlight(runName), " for ", highlight(subRun)))
  },
  runCompleted = function(elapsedTime, runName, subRun = NULL) {
    if (is.null(subRun)) {
      return(paste0(highlight(runName), " completed in ", elapsedTime))
    }
    return(paste0(highlight(runName), " for ", highlight(subRun), " completed in ", elapsedTime))
  },
  loadingSimulations = function() {
    paste0("Loading simulations on ", highlight(workflowName))
  },
  subsetsCreated = function(numberOfSubsets, numberOfSimulations) {
    paste0("Splitting simulations for parallel run:", highlight(numberOfSimulations), " simulations split into ", highlight(numberOfSubsets), " subsets")
  },

  #----- Debug messages ----
  dataIncludedInTimeRange = function(finalSize, timeRange, timeUnit, dataType) {
    paste0(
      finalSize, " ", dataType,
      " data were included in the analysis between ", min(timeRange), " and ", max(timeRange), " ", timeUnit, "."
    )
  },
  unknownUnitInObservedData = function(dvUnit) {
    paste0("In loadObservedDataFromSimulationSet: unit '", dvUnit, "' is unknown.")
  },
  lloqColumnNotFound = function(lloqColumn) {
    paste0("lloq column '", lloqColumn, "' defined in dictionary is not present in the dataset columns")
  },
  selectedObservedDataForPath = function(path, selectedRows) {
    paste0("Output '", path, "'. Number of selected observations: ", sum(selectedRows))
  },
  sizeForObservedDataId = function(observedDataId, numberOfColumns, numberOfRows) {
    paste0("Observed data Id '", observedDataId, "' included ", numberOfColumns, " columns and ", numberOfRows, " rows")
  },
  numberOfApplications = function(applicationLength, path, simulationName) {
    paste0(
      "'", applicationLength, "' applications identified for path '",
      path, "' in simulation '", simulationName, "'"
    )
  },
  timeRangesForSimulation = function(timeRanges, simulationName) {
    paste0("Time ranges for '", simulationName, "' timeRanges")
  },
  simulationLoaded = function(simulationFile) {
    paste0("Simulation file '", simulationFile, "' successfully loaded")
  },
  loadedOnCores = function(objectNames) {
    paste0(objectNames, " successfully loaded on all cores")
  },
  completedOnCores = function(objectNames) {
    paste0(objectNames, " successfully completed on all cores")
  },
  negativeDataRemoved = function(n) {
    paste0(n, " negative or null values were removed from logarithmic plot")
  },
  dataUnitMethods = function() {
    paste0(
      "Units for both dependent variable (dv) and time are required when using observed data.\n",
      "Please make sure that a unit definition does not use multiple methods or that the methods define consistent units.\n",
      highlight("Available methods for units definition of observed data:\n"),
      "\t1) Define units directly in dictionary (metaDataFile) by providing the units in column '",
      highlight(dictionaryParameters$datasetUnit), "' of your dictionary for ID(s) '",
      highlight(dictionaryParameters$timeID), "' and/or '",
      highlight(dictionaryParameters$dvID), "'\n",
      "\t2) Define units in your dataFile by including dedicated columns.\n\t",
      "Then, provide the corresponding column name in the column '",
      highlight(dictionaryParameters$datasetColumn),
      "' of your dictionary for ID(s) '",
      highlight(dictionaryParameters$timeUnitID), "' and/or '",
      highlight(dictionaryParameters$dvUnitID), "'\n",
      "\t3) Only for '", highlight(dictionaryParameters$dvID),
      "', define units in field '", highlight("dataUnit"),
      "' when creating an '", highlight("Output"), "' object."
    )
  },
  monteCarloChecking = function(analyticalSolutionMessage, medianPKRatioStatistics, n, seed){
    c(
      analyticalSolutionMessage,
      paste0(
        "Monte Carlo solution for ", paste0(tail(names(medianPKRatioStatistics), 4), collapse = ", "),
        " resulted in ", paste0(tail(unlist(medianPKRatioStatistics), 4), collapse = ", "),
        " respectively."
        ),
      paste0("Number of repetitions was set to: ", n), 
      paste0("Random seed number was set to: ", seed) 
    )
    }
)


#----- Helper functions ----
callingFunction <- function() {
  callingFunctions <- sys.calls()
  callingFunction <- deparse(sys.call(-length(callingFunctions) + 1)[[1]])
  return(paste0("In ", callingFunction, ":\n"))
}

#' @title highlight
#' @description Highlight text in console
#' @keywords internal
highlight <- function(text) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    return(crayon::cyan$bold$italic(as.character(text)))
  }
  return(as.character(text))
}

#' @title removeHighlight
#' @description Remove highlight in text
#' @keywords internal
removeHighlight <- function(text) {
  if (!requireNamespace("crayon", quietly = TRUE)) {
    return(text)
  }
  return(crayon::strip_style(text))
}
