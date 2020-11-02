StandardExcelSheetNames <- enum(c(
  "_Documentation",
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

  inputSections <- readxl::excel_sheets(excelFile)
  # Check for mandotory input sections
  validateIsIncluded(c(StandardExcelSheetNames$`Workflow and Tasks`, StandardExcelSheetNames$SimulationSets),
    inputSections,
    groupName = paste0("Sheet names of '", excelFile, "'")
  )

  if (isIncluded(StandardExcelSheetNames$`_Documentation`, inputSections)) {
    scriptDocumentation <- getScriptDocumentation(excelFile)
    scriptContent <- c(scriptContent, scriptDocumentation)
  }

  scriptContent <- c(scriptContent, "require(ospsuite.reportingengine)", "")

  if (isIncluded(StandardExcelSheetNames$`Userdef PK Parameter`, inputSections)) {}

  pkParametersContent <- NULL
  if (isIncluded(StandardExcelSheetNames$`PK Parameter`, inputSections)) {
    pkParametersTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`PK Parameter`)
    validateIsIncluded(c("Name", "Display name", "Unit"), names(pkParametersTable))
    pkParametersContent <- getPKParametersContent(pkParametersTable)
  }

  if (isIncluded(StandardExcelSheetNames$`Workflow and Tasks`, inputSections)) {
    workflowTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Workflow and Tasks`)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(workflowTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(workflowTable)[2])

    workflowInfo <- getWorkflowContent(workflowTable = workflowTable, excelFile = excelFile)
    workflowContent <- workflowInfo$workflowContent
  }

  simulationSetContent <- NULL
  if (isIncluded(StandardExcelSheetNames$SimulationSets, inputSections)) {
    simulationSetTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$SimulationSets)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(simulationSetTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(simulationSetTable)[2])

    simulationSetInfo <- getSimulationSetContent(excelFile, simulationSetTable, workflowMode = workflowInfo$workflowMode)
    simulationSetContent <- simulationSetInfo$simulationSetContent
  }

  outputContent <- NULL
  for (outputInfo in simulationSetInfo$outputInfo) {
    outputInfo <- getOutputContent(excelFile, outputInfo)
    outputContent <- c(outputContent, outputInfo)
  }

  runWorkflowContent <- c("", "workflow$runWorkflow()", "")

  scriptContent <- c(
    scriptContent,
    pkParametersContent,
    outputContent,
    simulationSetContent,
    workflowContent,
    runWorkflowContent
  )

  # Use styler to beautify and standardize the formatof the output file
  scriptContent <- styler:::style_text(scriptContent)
  
  if(removeComments){
    scriptContent <- removeCommentsFromWorkflowContent(scriptContent)
  }
  
  # Write the script
  fileObject <- file(workflowFile, encoding = "UTF-8")
  write(scriptContent, file = fileObject, sep = "\n")
  close(fileObject)

  print(paste0("Script '", workflowFile, "' successfully created"))
  return(invisible(workflowFile))
}

getScriptDocumentation <- function(excelFile, colSep = "\t") {
  docContent <- NULL
  # Check behaviour for empty Documentation sheet
  suppressMessages(
    docTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`_Documentation`, col_names = FALSE)
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
getPKParametersInfoContent <- function(excelFile, pkParametersSheet) {
  pkParametersContent <- NULL
  pkParametersTable <- readxl::read_excel(excelFile, sheet = pkParametersSheet)
  validateIsIncluded("Name", names(pkParametersTable)[1])

  if (all(is.na(pkParametersTable$`Display name`)) && all(is.na(pkParametersTable$Unit))) {
    pkParametersContent <- c(
      paste0(
        "pkParameters <- c('",
        paste0(pkParametersTable$Name, collapse = "', '"),
        "')"
      ),
      ""
    )
    return(pkParametersContent)
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
  return(pkParametersContent)
}

#' @title getOutputContent
#' @description Creates a character vector to be written in a workflow .R script defining `Output` object.
#' @param excelFile name of the Excel file from which the R script is created
#' @param outputInfo list of information about `Output` object. Include sheetName, dataSelection and dataDisplayName
#' @return Character vector defining the `Output` object
getOutputContent <- function(excelFile, outputInfo) {
  outputContent <- NULL
  outputTable <- readxl::read_excel(excelFile, sheet = outputInfo$sheetName)
  validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(outputTable)[1])
  validateIsIncluded(WorkflowMandatoryVariables$Description, names(outputTable)[2])

  outputNames <- getOutputNames(excelFile, outputInfo$sheetName)

  for (outputIndex in seq_along(outputNames)) {
    pkParametersOutputContent <- NULL
    pkParametersSheet <- getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$pkParameters)
    if (!is.na(pkParametersSheet)) {
      validateIsIncluded(pkParametersSheet, readxl::excel_sheets(excelFile), groupName = paste0("Sheet names of '", excelFile, "'"))
      pkParametersContent <- getPKParametersInfoContent(excelFile, pkParametersSheet)
      outputContent <- c(
        outputContent,
        pkParametersContent
      )
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
      paste0(
        outputNames[outputIndex], " <- Output$new(
          path = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$path), ", 
          displayName = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayName), ", 
          displayUnit = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayUnit), ", 
          dataSelection = ", dataSelection, ",
          dataDisplayName = ", dataDisplayName,
        pkParametersOutputContent,
        ")"
      ),
      ""
    )
  }
  return(outputContent)
}

#' @title getSimulationSetContent
#' @description Creates a character vector to be written in a workflow .R script defining `SimulationSet` objects.
#' @param excelFile name of the Excel file from which the R script is created
#' @param simulationTable Data.frame read from the Excel sheet "SimulationSets
#' @param workflowMode Either `PopulationWorkflow` or `MeanModelWorkflow`
#' @return Character vector defining the `SimulationSet` objects
getSimulationSetContent <- function(excelFile, simulationTable, workflowMode) {
  simulationSetContent <- NULL
  outputSheets <- NULL

  simulationSetNames <- names(simulationTable[, 3:ncol(simulationTable)])
  simulationSetNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = simulationSetNames)

  simulationType <- getSimulationSetType(workflowMode)

  allOutputInfo <- list()
  for (simulationIndex in seq_along(simulationSetNames)) {
    outputInfo <- list(
      dataSelection = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$dataSelection),
      dataDisplayName = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$dataDisplayName),
      sheetName = getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$outputs)
    )
    outputNames <- paste0(getOutputNames(excelFile, outputInfo$sheetName), collapse = ", ")
    allOutputInfo[[simulationIndex]] <- outputInfo

    # Function for dictionary
    dictionaryType <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryType)
    dictionaryLocation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$DictionaryLocation)
    dictionaryLocation <- getFileLocationFromType(
      location = dictionaryLocation,
      type = dictionaryType,
      excelFile = excelFile
    )

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

      referencePopulationContent <- paste0("referencePopulation = ", referencePopulation, ", ")
      populationFileContent <- paste0("populationFile = ", populationFile, ", ")
      populationNameContent <- paste0("populationName = ", populationName, ", ")
      studyDesignFileContent <- paste0("studyDesignFile = ", studyDesignLocation, ", ")
    }

    simulationSetContent <- c(
      simulationSetContent,
      paste0(
        simulationSetNames[simulationIndex],
        " <- ", simulationType, "$new(
    simulationSetName = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationSetName), ", ",
        referencePopulationContent, populationFileContent, populationNameContent, studyDesignFileContent, "
    simulationFile = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationFile), ", 
    simulationName = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationName), ", 
    outputs = c(", outputNames, "), 
    observedDataFile = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$observedDataFile), ", 
    observedMetaDataFile = ", dictionaryLocation, ")"
      ),
      ""
    )
  }
  simulationSetContent <- c(
    simulationSetContent,
    "",
    "simulationSets <- list(", paste0(simulationSetNames, collapse = ", "), ")",
    ""
  )

  return(list(
    simulationSetContent = simulationSetContent,
    simulationSetNames = simulationSetNames,
    outputInfo = unique(allOutputInfo)
  ))
}

#' @title getWorkflowContent
#' @description Creates a character vector to be written in a workflow .R script defining `Workflow` object
#' @param workflowTable Data.frame read from the Excel sheet "Workflow and Tasks"
#' @param excelFile name of the Excel file from which the R script is created
#' @return Character vector defining the `Workflow` object
getWorkflowContent <- function(workflowTable, excelFile) {
  workflowMode <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$`Workflow Mode`)
  workflowTypeContent <- NULL
  if (workflowMode == "PopulationWorkflow") {
    workflowType <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$`Population Workflow type`)
    workflowTypeContent <- paste0("workflowType = ", workflowType, ", ")
  }
  workflowFolder <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$workflowFolder)
  createWordReport <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$createWordReport)

  workflowContent <- NULL

  workflowContent <- paste0("workflow <- ", workflowMode, "$new(", workflowTypeContent, "simulationSets = simulationSets, workflowFolder = ", workflowFolder, ", createWordReport = ", createWordReport, ")")

  activateTaskContent <- NULL
  isActive <- lapply(AllAvailableTasks, function(x) {
    FALSE
  })
  for (taskName in AllAvailableTasks) {
    activeTaskName <- getIdentifierInfo(workflowTable, 1, taskName)
    if (isOfLength(activeTaskName, 0)) {
      next
    }
    activateTaskContent <- c(
      activateTaskContent,
      paste0("workflow$activateTasks(", activeTaskName, ")")
    )
    isActive[[taskName]] <- TRUE
  }

  workflowContent <- c(
    workflowContent,
    "",
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
    workflowContent <- c(workflowContent, settingContent)
  }

  return(list(workflowMode = workflowMode, workflowContent = workflowContent))
}

WorkflowMandatoryVariables <- enum(c(
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

WorkflowCodeIdentifiers <- enum(c(
  "Workflow Mode",
  "Population Workflow type",
  "workflowFolder",
  "createWordReport",
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
  names(OptionalSettings)
))

SimulationCodeIdentifiers <- enum(c(
  "simulationSetName",
  "simulationFile",
  "simulationName",
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


OutputCodeIdentifiers <- enum(c(
  "path",
  "displayName",
  "displayUnit",
  "dataSelection",
  "dataDisplayName",
  "pkParameters"
))

#' @title getIdentifierInfo
#' @description Get and format the information from a data.frame matching a certain `simulationIndex` column and `codeId` line
#' @param workflowTable Data.frame read from one of the available Excel sheets
#' @param simulationIndex Column to read after removing "Code Identifier" and "Description"
#' @param codeId Line to read in the data.frame corresponding to a specific value of "Code Identifier"
#' @return Information from a data.frame matching a certain `simulationIndex` column and `codeId` line
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
#' @return Name of `Output` object
getOutputNames <- function(excelFile, outputSheet) {
  outputTable <- readxl::read_excel(excelFile, sheet = outputSheet)
  outputNames <- paste(outputSheet, names(outputTable)[3:ncol(outputTable)], sep = "")
  # Remove spaces in the name
  outputNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = outputNames)
  return(outputNames)
}

#' @title getSensitivityVariableParameterPaths
#' @description Get the variable paths for sensitivity analysis settings
#' @param excelFile name of the Excel file from which the R script is created
#' @param sensitivityParametersSheet name of sheet defining the parameters to vary
#' @return Text of vector of paths to vary
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
  observedMetaDataTable <- readxl::read_excel(excelFile, sheet = observedMetaDataSheet)
  # Save the data dictionary as a csv file, missing values stay missing (na = ""),
  # row numbers are not printed in the file (row.names = FALSE), file uses UTF-8 encoding (fileEncoding = "UTF-8")
  write.csv(observedMetaDataTable, file = observedMetaDataFilename, na = "", row.names = FALSE, fileEncoding = "UTF-8")
  return(observedMetaDataFilename)
}

#' @title getPKParametersContent
#' @description Creates a character vector to be written in a workflow .R script updating the PKParameters objects
#' @param pkParametersTable Data.frame read from the Excel sheet "PK Parameters"
#' @return Character vector updating the PKParameters objects
getPKParametersContent <- function(pkParametersTable) {
  pkParametersContent <- NULL
  if (nrow(pkParametersTable) == 0) {
    return(pkParametersContent)
  }
  for (parameterIndex in seq(1, nrow(pkParametersTable))) {
    pkParametersContent <- c(
      pkParametersContent,
      paste0('updatePKParameter("', pkParametersTable$Name[parameterIndex], '", displayName = "', pkParametersTable$`Display name`[parameterIndex], '", displayUnit = "', pkParametersTable$Unit[parameterIndex], '")')
    )
  }
  pkParametersContent <- c(pkParametersContent, "")
  return(pkParametersContent)
}

#' @title getSimulationSetType
#' @description Output the SimulationSet object matching the appropriate workflow type
#' @param workflowMode Either `PopulationWorkflow` or `MeanModelWorkflow`
#' @return `PopulationSimulationSet` or `SimulationSet` as character
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
getFileLocationFromType <- function(location, type, excelFile) {
  validateIsIncluded(type, c("SHEET", "FILE"))
  if (isIncluded(type, "SHEET")) {
    location <- getObservedMetaDataFile(excelFile, location)
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
concatenateDataSelection <- function(inputs, sep = " & ") {
  validateIsString(inputs)

  # Account for ALL and NONE
  inputs[inputs == "ALL"] <- "TRUE"
  if (isIncluded("NONE", inputs)) {
    inputs <- "NONE"
  }
  # Remove NAs from expression
  inputs <- inputs[!is.na(inputs)]
  if (isOfLength(inputs, 0)) {
    return("NULL")
  }
  return(paste0("'", paste0(inputs, collapse = sep), "'"))
}

#' @title concatenateDataDisplayName
#' @description Concatenate inputs for dataDisplayName
#' @param inputs Vector of inputs to concatenate
#' @param sep Separator for display names
#' @return Character of concatenated inputs
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
removeCommentsFromWorkflowContent <- function(workflowContent, commentPattern = "#") {
  return(workflowContent[!grepl(commentPattern, workflowContent)])
}