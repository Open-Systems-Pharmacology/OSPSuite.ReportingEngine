#' @title StandardExcelSheetNames
#' @description Enum defining the standard names for the Excel sheets of template
#' @keywords internal
StandardExcelSheetNames <- ospsuite::enum(c(
  "Documentation",
  "Workflow and Tasks",
  "SimulationSets",
  "Userdef PK Parameter",
  "PK Parameter",
  "SensitivityParameter",
  "tpDictionary"
))

#' @title createWorkflowFromExcelInput
#' @description Creates an R script for running mean model or population workflows from an Excel file
#' @param excelFile name of the Excel file from which the R script is created
#' @param workflowFile name of the R script file created from the Excel file
#' @param removeComments logical to remove comments and information in `workflowFile`
#' @return An R script of name `workflowFile` to be run
#' @export
createWorkflowFromExcelInput <- function(excelFile, workflowFile = "workflow.R", removeComments = FALSE) {
  validateIsFileExtension(excelFile, c("xls", "xlsx"))
  validateIsFileExtension(workflowFile, "R")
  validateIsLogical(removeComments)

  scriptContent <- NULL
  # Initialize warnings and errors
  scriptWarnings <- ExcelMessaging$new("warnings")
  scriptErrors <- ExcelMessaging$new("errors")

  inputSections <- readxl::excel_sheets(excelFile)
  # Check for mandotory input sections
  validateIsIncluded(c(StandardExcelSheetNames$`Workflow and Tasks`, StandardExcelSheetNames$SimulationSets),
    inputSections,
    groupName = paste0("Sheet names of '", excelFile, "'")
  )

  if (isIncluded(StandardExcelSheetNames$`Documentation`, inputSections)) {
    scriptDocumentation <- getScriptDocumentation(excelFile)
    scriptContent <- c(scriptContent, scriptDocumentation)
  }

  scriptContent <- c(
    scriptContent,
    '# Load package "ospsuite.reportingengine" (require() function install the package if not installed yet)',
    "require(ospsuite.reportingengine)", ""
  )

  userDefPKParametersContent <- NULL
  if (isIncluded(StandardExcelSheetNames$`Userdef PK Parameter`, inputSections)) {
    userDefPKParametersTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Userdef PK Parameter`)
    validateIsIncluded(c("Name", "Standard PK parameter", "Display Unit"), names(userDefPKParametersTable))
    userDefPKParametersInfo <- getUserDefPKParametersContent(userDefPKParametersTable)
    userDefPKParametersContent <- userDefPKParametersInfo$content
    scriptWarnings$messages[["User Defined PK Parameters"]] <- userDefPKParametersInfo$warnings
    scriptErrors$messages[["User DefinedPK Parameters"]] <- userDefPKParametersInfo$errors
  }

  pkParametersContent <- NULL
  if (isIncluded(StandardExcelSheetNames$`PK Parameter`, inputSections)) {
    pkParametersTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`PK Parameter`)
    validateIsIncluded(c("Name", "Display name", "Unit"), names(pkParametersTable))
    pkParametersInfo <- getPKParametersContent(pkParametersTable)
    pkParametersContent <- pkParametersInfo$content
    scriptWarnings$messages[["PK Parameters"]] <- pkParametersInfo$warnings
    scriptErrors$messages[["PK Parameters"]] <- pkParametersInfo$errors
  }

  if (isIncluded(StandardExcelSheetNames$`Workflow and Tasks`, inputSections)) {
    workflowTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Workflow and Tasks`)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(workflowTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(workflowTable)[2])

    workflowInfo <- getWorkflowContent(workflowTable = workflowTable, excelFile = excelFile)
    workflowContent <- workflowInfo$content
    plotFormatContent <- workflowInfo$plotFormatContent
    activitySpecificContent <- workflowInfo$activitySpecificContent
    scriptWarnings$messages[["Workflow and Tasks"]] <- workflowInfo$warnings
    scriptErrors$messages[["Workflow and Tasks"]] <- workflowInfo$errors
  }

  simulationSetContent <- NULL
  if (isIncluded(StandardExcelSheetNames$SimulationSets, inputSections)) {
    simulationSetTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$SimulationSets)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(simulationSetTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(simulationSetTable)[2])

    simulationSetInfo <- getSimulationSetContent(excelFile, simulationSetTable, workflowMode = workflowInfo$workflowMode)
    simulationSetContent <- simulationSetInfo$content
    scriptWarnings$messages[["Simulation Sets"]] <- simulationSetInfo$warnings
    scriptErrors$messages[["Simulation Sets"]] <- simulationSetInfo$errors
  }

  outputContent <- NULL
  for (output in simulationSetInfo$outputInfo) {
    outputInfo <- getOutputContent(excelFile, output)
    outputContent <- c(outputContent, outputInfo$content)
    scriptWarnings$messages[[output$sheetName]] <- outputInfo$warnings
    scriptErrors$messages[[output$sheetName]] <- outputInfo$errors
  }

  runWorkflowContent <- c("", "workflow$runWorkflow()", "")

  scriptContent <- c(
    scriptContent,
    activitySpecificContent,
    plotFormatContent,
    userDefPKParametersContent,
    pkParametersContent,
    outputContent,
    simulationSetContent,
    workflowContent,
    runWorkflowContent
  )

  # Use styler to beautify and standardize the formatof the output file
  scriptContent <- styler:::style_text(scriptContent)

  if (removeComments) {
    scriptContent <- removeCommentsFromWorkflowContent(scriptContent)
  }

  # Write the script
  fileObject <- file(workflowFile, encoding = "UTF-8")
  write(scriptContent, file = fileObject, sep = "\n")
  close(fileObject)

  # This chunk of code aims at printing warnings and potential errors
  cat(paste0("Script '", workflowFile, "' successfully created\n\n"))
  # The weird characters are only meant to write the content in bold
  cat("\033[1m# Warnings\033[22m\n")
  scriptWarnings$displayMessage()
  cat("\033[1m# Errors\033[22m\n")
  scriptErrors$displayMessage()

  return(invisible(workflowFile))
}

getScriptDocumentation <- function(excelFile, colSep = "\t") {
  docContent <- NULL
  # Check behaviour for empty Documentation sheet
  suppressMessages(
    docTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Documentation`, col_names = FALSE)
  )
  if (isOfLength(docTable, 0)) {
    return(docContent)
  }
  for (lineIndex in 1:nrow(docTable)) {
    docContent <- c(
      docContent,
      paste0("# ", paste0(docTable[lineIndex, ], collapse = colSep))
    )
  }
  docContent <- c(docContent, "")
  return(docContent)
}

#' @title getPKParametersInfoContent
#' @description Creates a character vector to be written in a workflow .R script defining `pkParameters` input of `Output` object.
#' @param excelFile name of the Excel file from which the R script is created
#' @param pkParametersSheet name of the Excel sheet that defines the `pkParameters` information
#' @return Character vector defining `pkParameters` input of `Output` object
#' @keywords internal
getPKParametersInfoContent <- function(excelFile, pkParametersSheet) {
  pkParametersContent <- NULL
  pkParametersWarnings <- NULL
  pkParametersErrors <- NULL
  pkParametersTable <- readxl::read_excel(excelFile, sheet = pkParametersSheet)
  validateIsIncluded("Name", names(pkParametersTable)[1])

  # Check for duplicate PK parameters as input of Output object
  if (!hasUniqueValues(pkParametersTable$Name)) {
    pkParametersWarnings <- messages$errorHasNoUniqueValues(pkParametersTable$Name,
      dataName = paste0("selected PK parameters from Excel sheet '", pkParametersSheet, "'")
    )
  }
  # If none of the PK Parameters are updated, their names can directly be used as is
  if (all(is.na(pkParametersTable$`Display name`)) && all(is.na(pkParametersTable$Unit))) {
    pkParametersContent <- c(
      paste0("pkParameters <- c('", paste0(pkParametersTable$Name, collapse = "', '"), "')"),
      ""
    )
    return(list(
      content = pkParametersContent,
      warnings = pkParametersWarnings,
      errors = pkParametersErrors
    ))
  }

  # Check for duplicate PK parameter display names as input of Output object
  if (!hasUniqueValues(pkParametersTable$`Display name`)) {
    pkParametersErrors <- messages$errorHasNoUniqueValues(pkParametersTable$`Display name`,
      dataName = paste0("display names of selected PK parameters from Excel sheet '", pkParametersSheet, "'")
    )
  }

  for (pkParameterIndex in seq_along(pkParametersTable$Name)) {
    pkParameter <- pkParametersTable$Name[pkParameterIndex]
    displayName <- pkParametersTable$`Display name`[pkParameterIndex]
    displayUnit <- pkParametersTable$Unit[pkParameterIndex]
    displayNameContent <- NULL
    displayUnitContent <- NULL
    # Any prevent crash from missing "Unit" or "Display name" column
    if (any(!is.na(displayName))) {
      displayNameContent <- paste0(", displayName = '", displayName, "'")
    }
    if (any(!is.na(displayUnit))) {
      displayUnitContent <- paste0(", displayUnit = '", displayUnit, "'")
    }
    pkParametersContent <- c(
      pkParametersContent,
      paste0(
        "pk", pkParameter, " <- PkParameterInfo$new(pkParameter = '", pkParameter, "'",
        displayNameContent, displayUnitContent, ")"
      )
    )
  }
  pkParametersContent <- c(
    pkParametersContent,
    "",
    paste0(
      "pkParameters <- c(pk",
      paste0(pkParametersTable$Name, collapse = ", pk"),
      ")"
    ),
    ""
  )
  return(list(
    content = pkParametersContent,
    warnings = pkParametersWarnings,
    errors = pkParametersErrors
  ))
}

#' @title getOutputContent
#' @description Creates a character vector to be written in a workflow .R script defining `Output` object.
#' @param excelFile name of the Excel file from which the R script is created
#' @param outputInfo list of information about `Output` object. Include sheetName, dataSelection and dataDisplayName
#' @return Character vector defining the `Output` object
#' @keywords internal
getOutputContent <- function(excelFile, outputInfo) {
  outputContent <- NULL
  outputWarnings <- NULL
  outputErrors <- NULL
  allOutputPaths <- NULL
  allDisplayNames <- NULL
  allDataDisplayNames <- NULL
  outputErrors <- NULL

  outputTable <- readxl::read_excel(excelFile, sheet = outputInfo$sheetName)
  validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(outputTable)[1])
  validateIsIncluded(WorkflowMandatoryVariables$Description, names(outputTable)[2])

  outputNames <- getOutputNames(excelFile, outputInfo$sheetName, outputInfo$simulationSetName)

  for (outputIndex in seq_along(outputNames)) {
    pkParametersOutputContent <- NULL
    pkParametersSheet <- getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$pkParameters)
    if (!is.na(pkParametersSheet)) {
      validateIsIncluded(pkParametersSheet, readxl::excel_sheets(excelFile), groupName = paste0("Sheet names of '", excelFile, "'"))
      pkParametersInfo <- getPKParametersInfoContent(excelFile, pkParametersSheet)
      pkParametersContent <- pkParametersInfo$content
      outputWarnings <- c(outputWarnings, pkParametersInfo$warnings)
      outputErrors <- c(outputErrors, pkParametersInfo$errors)
      outputContent <- c(outputContent, pkParametersContent)
      # The line break within paste0 is needed to ensure a nice structure in the output script
      pkParametersOutputContent <- paste0(",
                                          pkParameters = pkParameters")
    }

    # Add dataDisplayName and pkParameters
    dataSelection <- concatenateDataSelection(c(
      outputInfo$dataSelection,
      getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$dataSelection)
    ))
    dataDisplayName <- concatenateDataDisplayName(c(
      outputInfo$dataDisplayName,
      getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$dataDisplayName)
    ))
    outputContent <- c(
      outputContent,
      paste0("# Create an Output object of name '", outputNames[outputIndex], "' defining which path, its PK parameters and their display to use in a simulation set"),
      "# The 'dataSelection' input is a character expression selecting which observed data to be compared to the path",
      "# The 'pkParameters' input can be a character array using defined PK Parameter names or an array of PkParameterInfo objects",
      paste0(
        outputNames[outputIndex], " <- Output$new(
          path = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$path), ",
          displayName = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayName), ",
          displayUnit = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayUnit), ",
          dataSelection = ", dataSelection, ",
          dataUnit = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$dataUnit), ",
          dataDisplayName = ", dataDisplayName,
        pkParametersOutputContent,
        ")"
      ),
      ""
    )

    allOutputPaths <- c(allOutputPaths, getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$path))
    allDisplayNames <- c(allDisplayNames, getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayName))
    allDataDisplayNames <- c(allDataDisplayNames, dataDisplayName)
  }
  # Check for duplicate output paths, display names and data display names
  if (!hasUniqueValues(allOutputPaths)) {
    outputWarnings <- c(
      outputWarnings,
      messages$errorHasNoUniqueValues(gsub("'", "", allOutputPaths),
        dataName = paste0("paths of Outputs defined in Excel sheet '", outputInfo$sheetName, "'")
      )
    )
  }
  if (!hasUniqueValues(allDisplayNames)) {
    outputErrors <- c(
      outputErrors,
      messages$errorHasNoUniqueValues(gsub("'", "", allDisplayNames),
        dataName = paste0("path display names of Outputs defined in Excel sheet '", outputInfo$sheetName, "'")
      )
    )
  }
  # TO DO: Check if no values are flagged because NAs are removed
  if (!hasUniqueValues(allDataDisplayNames)) {
    outputErrors <- c(
      outputErrors,
      messages$errorHasNoUniqueValues(gsub("'", "", allDataDisplayNames),
        dataName = paste0("data display names of Outputs defined in Excel sheet '", outputInfo$sheetName, "'")
      )
    )
  }

  return(list(
    content = outputContent,
    warnings = outputWarnings,
    errors = outputErrors
  ))
}

#' @title getSimulationSetContent
#' @description Creates a character vector to be written in a workflow .R script defining `SimulationSet` objects.
#' @param excelFile name of the Excel file from which the R script is created
#' @param simulationTable Data.frame read from the Excel sheet "SimulationSets
#' @param workflowMode Either `PopulationWorkflow` or `MeanModelWorkflow`
#' @return Character vector defining the `SimulationSet` objects
#' @keywords internal
getSimulationSetContent <- function(excelFile, simulationTable, workflowMode) {
  simulationSetContent <- NULL
  outputSheets <- NULL
  simulationSetWarnings <- NULL
  simulationSetErrors <- NULL

  simulationSetNames <- names(simulationTable[, 3:ncol(simulationTable)])
  simulationSetNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = simulationSetNames)

  simulationType <- getSimulationSetType(workflowMode)

  allOutputInfo <- list()
  for (simulationIndex in seq_along(simulationSetNames)) {
    outputInfo <- list(
      dataSelection = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$dataSelection),
      dataDisplayName = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$dataDisplayName),
      sheetName = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$outputs),
      simulationSetName = simulationSetNames[simulationIndex]
    )
    outputNames <- paste0(getOutputNames(excelFile, outputInfo$sheetName, simulationSetNames[simulationIndex]), collapse = ", ")
    allOutputInfo[[simulationIndex]] <- outputInfo

    # Function for dictionary
    dictionaryType <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryType)
    dictionaryLocation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryLocation)
    dictionaryLocation <- getFileLocationFromType(
      location = dictionaryLocation,
      type = dictionaryType,
      excelFile = excelFile
    )
    # Throw an error if file already exists and sheet tried to overwrite it
    # The simulation set input will still have the aimed dictionary file path
    if (is.null(dictionaryLocation)) {
      simulationSetErrors <- c(
        simulationSetErrors,
        paste0("Dictionary file '", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryLocation), ".csv' already exists and was not overwritten")
      )
      dictionaryLocation <- paste0("'", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryLocation), ".csv'")
    }

    # MeanModelWorkflow doesn't use population fields, which are set to NULL
    populationFileContent <- NULL
    populationNameContent <- NULL
    referencePopulationContent <- NULL
    studyDesignFileContent <- NULL
    if (isIncluded(workflowMode, "PopulationWorkflow")) {
      referencePopulation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$referencePopulation)
      populationFile <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$populationFile)
      populationName <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$populationName)

      studyDesignType <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignType)
      studyDesignLocation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignLocation)
      studyDesignLocation <- getFileLocationFromType(
        location = studyDesignLocation,
        type = studyDesignType,
        excelFile = excelFile
      )
      # Same behaviour as dictionary:
      # Throw an error if file already exists and sheet tried to overwrite it
      # The simulation set input will still have the aimed study design file path
      if (is.null(studyDesignLocation)) {
        simulationSetErrors <- c(
          simulationSetErrors,
          paste0("Study design file '", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignLocation), ".csv' already exists and was not overwritten")
        )
        studyDesignLocation <- paste0("'", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignLocation), ".csv'")
      }

      # These contents breaks lines to beautify final workflow script
      referencePopulationContent <- paste0("referencePopulation = ", referencePopulation, ",
                                           ")
      populationFileContent <- paste0("populationFile = ", populationFile, ",
                                      ")
      populationNameContent <- paste0("populationName = ", populationName, ",
                                      ")
      studyDesignFileContent <- paste0("studyDesignFile = ", studyDesignLocation, ",
                                       ")
    }

    simulationSetContent <- c(
      simulationSetContent,
      paste0("# Create a SimulationSet object of name '", simulationSetNames[simulationIndex], "' defining which simulation, its Output objects, observed dataset and potentially its population to use in the workflow"),
      "# Note that workflows of type 'MeanModelWorkflow' require SimulationSet objects, while workflows of type 'PopulationWorkflow' require PopulationSimulationSet objects",
      paste0(
        simulationSetNames[simulationIndex],
        " <- ", simulationType, "$new(
    simulationSetName = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationSetName), ",
        ", referencePopulationContent, populationFileContent, populationNameContent, studyDesignFileContent,
        "simulationFile = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationFile), ",
    outputs = c(", outputNames, "),
    observedDataFile = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$observedDataFile), ",
    observedMetaDataFile = ", dictionaryLocation, ",
    timeUnit = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$timeUnit), ")"
      ),
      ""
    )
    # Check that pkml file has correct extension
    simulationFile <- gsub("'", "", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationFile))
    if (!isFileExtension(simulationFile, "pkml")) {
      simulationSetErrors <- c(simulationSetErrors, paste0("In simulation set '", simulationSetNames[simulationIndex], "', ", messages$errorExtension(simulationFile, "pkml")))
    }
  }
  simulationSetContent <- c(
    simulationSetContent,
    "",
    "simulationSets <- list(", paste0(simulationSetNames, collapse = ", "), ")",
    ""
  )

  return(list(
    content = simulationSetContent,
    simulationSetNames = simulationSetNames,
    outputInfo = unique(allOutputInfo),
    warnings = simulationSetWarnings,
    errors = simulationSetErrors
  ))
}

#' @title getWorkflowContent
#' @description Creates a character vector to be written in a workflow .R script defining `Workflow` object
#' @param workflowTable Data.frame read from the Excel sheet "Workflow and Tasks"
#' @param excelFile name of the Excel file from which the R script is created
#' @return Character vector defining the `Workflow` object
#' @keywords internal
getWorkflowContent <- function(workflowTable, excelFile) {
  workflowMode <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$`Workflow Mode`)
  workflowTypeContent <- NULL
  workflowContent <- NULL
  activateTaskContent <- NULL
  workflowWarnings <- NULL
  workflowErrors <- NULL

  if (workflowMode == "PopulationWorkflow") {
    workflowType <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$`Population Workflow type`)
    workflowTypeContent <- paste0("workflowType = ", workflowType, ", ")
  }
  workflowFolder <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$workflowFolder)
  createWordReport <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$createWordReport)
  simulationSetDescriptor <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$simulationSetDescriptor)
  workflowContent <- paste0("workflow <- ", workflowMode, "$new(", workflowTypeContent, "simulationSets = simulationSets, workflowFolder = ", workflowFolder, ", createWordReport = ", createWordReport, ", simulationSetDescriptor = ", simulationSetDescriptor, ")")

  # Activate tasks as defined by user
  isActive <- lapply(AllAvailableTasks, function(x) {
    FALSE
  })
  for (taskName in AllAvailableTasks) {
    activeTaskName <- getIdentifierInfo(workflowTable, 1, taskName)
    if (isOfLength(activeTaskName, 0)) {
      next
    }
    if (workflowMode == "PopulationWorkflow" & isIncluded(taskName, c("plotAbsorption", "plotMassBalance"))) {
      workflowWarnings <- c(
        workflowWarnings,
        paste0("Task '", taskName, "' defined as active, was not printed because '", taskName, "' is not available for '", workflowMode, "'.")
      )
      next
    }
    if (workflowMode == "MeanModelWorkflow" & isIncluded(taskName, "plotDemography")) {
      workflowWarnings <- c(
        workflowWarnings,
        paste0("Task '", taskName, "' defined as active, was not printed because '", taskName, "' is not available for '", workflowMode, "'.")
      )
      next
    }
    activateTaskContent <- c(
      activateTaskContent,
      paste0("workflow$activateTasks(", activeTaskName, ")")
    )
    isActive[[taskName]] <- TRUE
  }

  workflowContent <- c(
    paste0("# Create a ", workflowMode, " object of name 'workflow' defining which tasks to perform on its list of simulation sets"),
    workflowContent,
    "",
    "# The workflow method $inactivateTasks() prevents running tasks that could be active by default (such as 'simulate')",
    "workflow$inactivateTasks()",
    activateTaskContent,
    ""
  )

  # Optional fields: task settings
  for (optionalSettingName in names(OptionalSettings)) {
    settingValue <- getIdentifierInfo(workflowTable, 1, optionalSettingName)
    if (is.na(settingValue)) {
      next
    }
    if (isIncluded(optionalSettingName, "calculateSensitivity: variableParameterPaths")) {
      settingValue <- getSensitivityVariableParameterPaths(excelFile, sensitivityParametersSheet = settingValue)
    }
    settingContent <- paste0(OptionalSettings[[optionalSettingName]], settingValue)
    workflowContent <- c(
      workflowContent,
      "",
      "# The following code chunk defines tasks optional settings",
      settingContent
    )
  }

  # Optional field: plot format
  plotFormatContent <- NULL
  plotFormat <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$plotFormat)
  if (!isIncluded(plotFormat, "NULL")) {
    plotFormatContent <- c(
      paste0("# Figures exported by the workflow will be saved as ", plotFormat, " files"),
      paste0("setPlotFormat(", plotFormat, ")")
    )
  }

  # Optional field: Activity specific code
  activitySpecificContent <- NULL
  activitySpecificCode <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$activitySpecificCode)
  if (!is.na(activitySpecificCode)) {
    activitySpecificContent <- c(
      paste0("# Load the function ", activitySpecificCode, " from the file '", activitySpecificCode, ".R'"),
      paste0('source("', activitySpecificCode, '.R")')
    )
    workflowContent <- c(
      workflowContent,
      "",
      paste0("# Update 'workflow' (workflow is an R6 object, so updates from ", activitySpecificCode, " does not need to be returned as a new variable)"),
      paste0(activitySpecificCode, "(workflow)")
    )
  }

  return(list(
    workflowMode = workflowMode,
    content = workflowContent,
    plotFormatContent = plotFormatContent,
    activitySpecificContent = activitySpecificContent,
    warnings = workflowWarnings,
    errors = workflowErrors
  ))
}

WorkflowMandatoryVariables <- ospsuite::enum(c(
  "Code Identifier",
  "Description"
))

OptionalSettings <- list(
  "simulate: numberOfCores" = "workflow$simulate$settings$numberOfCores <- ",
  "simulate: showProgress" = "workflow$simulate$settings$showProgress <- ",
  "calculateSensitivity: numberOfCores" = "workflow$calculateSensitivity$settings$numberOfCores <- ",
  "calculateSensitivity: showProgress" = "workflow$calculateSensitivity$settings$showProgress <- ",
  "calculateSensitivity: quantileVec" = "workflow$calculateSensitivity$settings$quantileVec <- ",
  "calculateSensitivity: variationRange" = "workflow$calculateSensitivity$settings$variationRange <- ",
  "calculateSensitivity: variableParameterPaths" = "workflow$calculateSensitivity$settings$variableParameterPaths <- ",
  "plotSensitivity: maximalParametersPerSensitivityPlot" = "workflow$plotSensitivity$settings$maximalParametersPerSensitivityPlot <- ",
  "plotSensitivity: totalSensitivityThreshold" = "workflow$plotSensitivity$settings$totalSensitivityThreshold <- ",
  "plotSensitivity: xAxisFontSize" = "workflow$plotSensitivity$settings$xAxisFontSize <- ",
  "plotSensitivity: yAxisFontSize" = "workflow$plotSensitivity$settings$yAxisFontSize <- "
)

WorkflowCodeIdentifiers <- ospsuite::enum(c(
  "Workflow Mode",
  "Population Workflow type",
  "workflowFolder",
  "createWordReport",
  "simulationSetDescriptor",
  "plotFormat",
  "simulate",
  "calculatePKParameters",
  "calculateSensitivity",
  "plotTimeProfilesAndResiduals",
  "plotPKParameters",
  "plotSensitivity",
  "plotDemography",
  "plotAbsorption",
  "plotMassBalance",
  "activitySpecificCode",
  names(OptionalSettings)
))

SimulationCodeIdentifiers <- ospsuite::enum(c(
  "simulationSetName",
  "simulationFile",
  "outputs",
  "observedDataFile",
  "DictionaryType",
  "DictionaryLocation",
  "dataSelection",
  "dataDisplayName",
  "timeUnit",
  "timeOffset",
  "populationFile",
  "populationName",
  "referencePopulation",
  "plotReferenceObsData",
  "StudyDesignType",
  "StudyDesignLocation"
))


OutputCodeIdentifiers <- ospsuite::enum(c(
  "path",
  "displayName",
  "displayUnit",
  "dataSelection",
  "dataUnit",
  "dataDisplayName",
  "pkParameters"
))

UserDefPKParametersOptionalSettings <- list(
  "StartTime [min]" = " $startTime <- ",
  "StartApplicationIndex" = " $startApplicationIndex <- ",
  "StartTimeOffset [min]" = " $startTimeOffset <- ",
  "EndTime [min]" = " $endTime <- ",
  "EndApplicationIndex" = " $endApplicationIndex <- ",
  "EndTimeOffset [min]" = " $endTimeOffset <- ",
  "NormalizationFactor" = " $normalizationFactor <- ",
  "ConcentrationThreshold [µmol/l]" = " $concentrationThreshold <- "
)

#' @title getIdentifierInfo
#' @description Get and format the information from a data.frame matching a certain `simulationIndex` column and `codeId` line
#' @param workflowTable Data.frame read from one of the available Excel sheets
#' @param simulationIndex Column to read after removing "Code Identifier" and "Description"
#' @param codeId Line to read in the data.frame corresponding to a specific value of "Code Identifier"
#' @return Information from a data.frame matching a certain `simulationIndex` column and `codeId` line
#' @keywords internal
getIdentifierInfo <- function(workflowTable, simulationIndex, codeId) {
  validateIsOfType(workflowTable, "data.frame")
  validateIsInteger(simulationIndex)
  validateIsIncluded(codeId, c(WorkflowCodeIdentifiers, SimulationCodeIdentifiers, OutputCodeIdentifiers))

  # Which is necessary because of NAs
  workflowID <- which(workflowTable[, WorkflowMandatoryVariables$`Code Identifier`] == codeId)
  # Shift of columns because Code identifier and Description are part of the table
  workflowInfo <- as.character(workflowTable[workflowID, simulationIndex + 2])

  # For tasks return the task name if it is activated
  if (isIncluded(codeId, AllAvailableTasks)) {
    # If input is not included in 1, TRUE or true, assume task is not activated
    if (isIncluded(workflowInfo, c("Yes", "YES", "1", "TRUE", "true"))) {
      return(paste0('"', codeId, '"'))
    }
    return()
  }
  # For info of type sheet, return directly sheet name
  if (isIncluded(codeId, c(
    WorkflowCodeIdentifiers$`Workflow Mode`,
    WorkflowCodeIdentifiers$activitySpecificCode,
    SimulationCodeIdentifiers$outputs,
    SimulationCodeIdentifiers$DictionaryType,
    SimulationCodeIdentifiers$DictionaryLocation,
    SimulationCodeIdentifiers$StudyDesignType,
    SimulationCodeIdentifiers$StudyDesignLocation,
    SimulationCodeIdentifiers$dataSelection,
    SimulationCodeIdentifiers$dataDisplayName,
    OutputCodeIdentifiers$pkParameters,
    OutputCodeIdentifiers$dataSelection,
    OutputCodeIdentifiers$dataDisplayName,
    names(OptionalSettings)
  ))) {
    return(workflowInfo)
  }
  # readxl::read_excel returns na for missing values which need to be passed on as NULL to simulation sets
  if (is.na(workflowInfo)) {
    return("NULL")
  }
  # For info about reference population, return logical value as character
  if (isIncluded(codeId, c(SimulationCodeIdentifiers$referencePopulation, WorkflowCodeIdentifiers$createWordReport))) {
    # Will return false if input is not included in 1, TRUE or true
    return(as.character(isIncluded(workflowInfo, c("Yes", "YES", "1", "TRUE", "true"))))
  }
  # For any other info, it needs to be returned in between quotes
  return(paste0("'", workflowInfo, "'"))
}

#' @title getOutputNames
#' @description Get the names of `Output` objets
#' @param excelFile name of the Excel file from which the R script is created
#' @param outputSheet name of sheet defining an `Output` object
#' @param simulationSetName name of simulation set
#' @return Name of `Output` object
#' @keywords internal
getOutputNames <- function(excelFile, outputSheet, simulationSetName) {
  outputTable <- readxl::read_excel(excelFile, sheet = outputSheet)
  outputNames <- paste(simulationSetName, names(outputTable)[3:ncol(outputTable)], sep = ".")
  # Remove spaces in the name
  outputNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = outputNames)
  return(outputNames)
}

#' @title getSensitivityVariableParameterPaths
#' @description Get the variable paths for sensitivity analysis settings
#' @param excelFile name of the Excel file from which the R script is created
#' @param sensitivityParametersSheet name of sheet defining the parameters to vary
#' @return Text of vector of paths to vary
#' @keywords internal
getSensitivityVariableParameterPaths <- function(excelFile, sensitivityParametersSheet) {
  sensitivityParametersTable <- readxl::read_excel(excelFile, sheet = sensitivityParametersSheet)
  sensitivityParametersContent <- paste0("c('", paste0(sensitivityParametersTable$Path, collapse = "', '"), "')")
  return(sensitivityParametersContent)
}

getObservedMetaDataFile <- function(excelFile, observedMetaDataSheet, format = "csv") {
  if (is.na(observedMetaDataSheet)) {
    return(observedMetaDataSheet)
  }
  observedMetaDataFilename <- paste0(observedMetaDataSheet, ".", format)
  # Read sheet from excel file
  # Caution: Study Design file can have multiple columns with same header name and special characters
  # To prevent R to modify them giving R valid unique names, col_name need to be set to FALSE in the sequel
  # Besides, default .name_repair will send warnings which is silented by overwriting with base function make.names
  observedMetaDataTable <- readxl::read_excel(excelFile, sheet = observedMetaDataSheet, col_names = FALSE, .name_repair = ~ make.names(.x))
  # Return NULL in order to throw an error if dictionary already exists
  if (file.exists(observedMetaDataFilename)) {
    return(NULL)
  }
  # Save the data dictionary as a csv file, missing values stay missing (na = ""),
  # Column names need to be removed from the previous step, write.csv is replaced by write.table with option col.names = FALSE,
  # row numbers are also removed (row.names = FALSE), file uses UTF-8 encoding (fileEncoding = "UTF-8")
  write.table(observedMetaDataTable, file = observedMetaDataFilename, na = "", row.names = FALSE, col.names = FALSE, sep = ",", fileEncoding = "UTF-8")
  return(observedMetaDataFilename)
}

#' @title getPKParametersContent
#' @description Creates a character vector to be written in a workflow .R script updating the PKParameters objects
#' @param pkParametersTable Data.frame read from the Excel sheet "PK Parameters"
#' @return A list of script content, associated with its potential warnings and errors for updating the PKParameters objects
#' @keywords internal
getPKParametersContent <- function(pkParametersTable) {
  pkParametersContent <- NULL
  pkParametersWarnings <- NULL
  pkParametersErrors <- NULL
  if (nrow(pkParametersTable) == 0) {
    return(pkParametersContent)
  }
  # Check for duplicate PK parameters as input of Output object
  if (!hasUniqueValues(pkParametersTable$Name)) {
    pkParametersWarnings <- c(
      pkParametersWarnings,
      messages$errorHasNoUniqueValues(pkParametersTable$Name, dataName = "PK parameters update")
    )
  }
  # Check for duplicate PK parameter display names as input of Output object
  if (!hasUniqueValues(pkParametersTable$`Display name`)) {
    pkParametersWarnings <- c(
      pkParametersWarnings,
      messages$errorHasNoUniqueValues(pkParametersTable$`Display name`, dataName = "PK parameters display names")
    )
  }

  for (parameterIndex in seq(1, nrow(pkParametersTable))) {
    pkParametersContent <- c(
      pkParametersContent,
      paste0('updatePKParameter("', pkParametersTable$Name[parameterIndex], '", displayName = "', pkParametersTable$`Display name`[parameterIndex], '", displayUnit = "', pkParametersTable$Unit[parameterIndex], '")')
    )
  }
  pkParametersContent <- c(
    '# Update PK parameters display names and units using "ospsuite" package.',
    "# This method should be preferred as it centralizes the names and units among the different paths.",
    pkParametersContent,
    ""
  )
  return(list(
    content = pkParametersContent,
    warnings = pkParametersWarnings,
    errors = pkParametersErrors
  ))
}

#' @title getSimulationSetType
#' @description Output the SimulationSet object matching the appropriate workflow type
#' @param workflowMode Either `PopulationWorkflow` or `MeanModelWorkflow`
#' @return `PopulationSimulationSet` or `SimulationSet` as character
#' @keywords internal
getSimulationSetType <- function(workflowMode) {
  validateIsIncluded(workflowMode, c("MeanModelWorkflow", "PopulationWorkflow"))
  simulationType <- "SimulationSet"
  if (isIncluded(workflowMode, "PopulationWorkflow")) {
    simulationType <- paste0("Population", simulationType)
  }
  return(simulationType)
}

#' @title getFileLocationFromType
#' @description Get the right path text and copy the sheet content as a file if type is SHEET
#' @param location Path of the file if type is FILE or sheetname if type is SHEET
#' @param type Location type: either "SHEET" or "FILE"
#' @param excelFile name of the Excel file from which the R script is created
#' @return Character of location to provide
#' @keywords internal
getFileLocationFromType <- function(location, type, excelFile) {
  validateIsIncluded(type, c("SHEET", "FILE"))
  if (isIncluded(type, "SHEET")) {
    location <- getObservedMetaDataFile(excelFile, location)
  }
  if (is.null(location)) {
    return(location)
  }
  if (is.na(location)) {
    return("NULL")
  }
  return(paste0("'", location, "'"))
}

#' @title concatenateDataSelection
#' @description Concatenate inputs for dataSelection
#' @param inputs Vector of inputs to concatenate
#' @param sep Separator character corresponding to logical &
#' @return Character of concatenated inputs
#' @keywords internal
concatenateDataSelection <- function(inputs, sep = ") & (") {
  validateIsString(inputs)
  # No data selection to concatenate
  if (isOfLength(inputs, 0)) {
    return("NULL")
  }
  # Deal with NONE and ALL inputs

  # Assume NA = NONE
  inputs[is.na(inputs)] <- "NONE"

  # If all are NONE then return NONE
  if (all(inputs == "NONE")) {
    return('"NONE"')
  }

  # If only some are NONE, remove any NONE
  if (isIncluded("NONE", inputs)) {
    inputs <- inputs[!(inputs == "NONE")]
  }

  # Assume ALL in any is ALL in all
  if (isIncluded("ALL", inputs)) {
    return('"ALL"')
  }
  return(paste0("'(", paste0(inputs, collapse = sep), ")'"))
}

#' @title concatenateDataDisplayName
#' @description Concatenate inputs for dataDisplayName
#' @param inputs Vector of inputs to concatenate
#' @param sep Separator for display names
#' @return Character of concatenated inputs
#' @keywords internal
concatenateDataDisplayName <- function(inputs, sep = " - ") {
  validateIsString(inputs)
  # Remove NAs
  inputs <- inputs[!is.na(inputs)]
  if (isOfLength(inputs, 0)) {
    return("NULL")
  }
  return(paste0("'", paste0(inputs, collapse = sep), "'"))
}


#' @title removeCommentsFromWorkflowContent
#' @description Remove comments from workflow file content
#' @param workflowContent Character vector with content of the workflow file
#' @param commentPattern Character starting a comment
#' @return Workflow content without its comments
#' @keywords internal
removeCommentsFromWorkflowContent <- function(workflowContent, commentPattern = "#") {
  return(workflowContent[!grepl(commentPattern, workflowContent)])
}


#' @title getUserDefPKParametersContent
#' @description Creates a character vector to be written in a workflow .R script updating the PKParameters objects
#' @param userDefPKParametersTable Data.frame read from the Excel sheet "PK Parameters"
#' @return A list of script content, associated with its potential warnings and errors for updating the PKParameters objects
#' @keywords internal
getUserDefPKParametersContent <- function(userDefPKParametersTable) {
  userDefPKParametersContent <- NULL
  userDefPKParametersWarnings <- NULL
  userDefPKParametersErrors <- NULL
  if (nrow(userDefPKParametersTable) == 0) {
    return(userDefPKParametersContent)
  }
  # Check for duplicate PK parameters as input of Output object
  if (!hasUniqueValues(userDefPKParametersTable$Name)) {
    userDefPKParametersErrors <- c(
      userDefPKParametersErrors,
      messages$errorHasNoUniqueValues(userDefPKParametersTable$Name, dataName = "User Defined PK parameters")
    )
  }

  # User defined parameters currently need to be set in 2 steps:
  # 1- create the parameter
  # 2- set some of its fields
  for (parameterIndex in seq(1, nrow(userDefPKParametersTable))) {
    userDefPKParametersContent <- c(
      userDefPKParametersContent,
      paste0("# Create user defined parameter ", userDefPKParametersTable$Name[parameterIndex], " and then set its properties"),
      "# The dimension of the PK parameter can only be defined from 'displayUnit' at the time of the PK parameter creation",
      paste0(
        userDefPKParametersTable$Name[parameterIndex], ' <- addUserDefinedPKParameter(name = "', userDefPKParametersTable$Name[parameterIndex], '",
             standardPKParameter = StandardPKParameter$', userDefPKParametersTable$`Standard PK parameter`[parameterIndex], ',
        displayUnit = "', userDefPKParametersTable$`Display Unit`[parameterIndex], '")'
      )
    )

    columnNames <- names(UserDefPKParametersOptionalSettings)
    for (userDefPKParameterIndex in seq_along(UserDefPKParametersOptionalSettings)) {
      userDefinedSetting <- userDefPKParametersTable[parameterIndex, columnNames[userDefPKParameterIndex]]
      # If setting is not defined (column does not exist) or not filled
      if (isOfLength(userDefinedSetting, 0)) {
        next
      }
      if (is.na(userDefinedSetting)) {
        next
      }

      userDefPKParametersContent <- c(
        userDefPKParametersContent,
        paste0(
          userDefPKParametersTable$Name[parameterIndex],
          UserDefPKParametersOptionalSettings[[userDefPKParameterIndex]],
          userDefinedSetting
        )
      )
    }
  }

  userDefPKParametersContent <- c(userDefPKParametersContent, "")

  return(list(
    content = userDefPKParametersContent,
    warnings = userDefPKParametersWarnings,
    errors = userDefPKParametersErrors
  ))
}
