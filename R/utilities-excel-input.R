StandardExcelSheetNames <- enum(c(
  "Documentation",
  "MeanModelSimulation",
  "PopulationSimulation",
  "SensitivityParameter"
))

#' @title createWorkflowFromExcelInput
#' @description Creates an R script for running mean model or population workflows from an Excel file
#' @param excelFile name of the Excel file from which the R script is created
#' @param workflowFile name of the R script file created from the Excel file
#' @param workflowFolder name of the folder in which the wrokflow results will be stored
#' @return An R script of name `workflowFile` to be run
#' @export
createWorkflowFromExcelInput <- function(excelFile, workflowFile = "workflow.R", workflowFolder = "Output") {
  validateIsFileExtension(excelFile, c("xls", "xlsx"))
  validateIsFileExtension(workflowFile, "R")

  scriptContent <- NULL

  inputSections <- readxl::excel_sheets(excelFile)

  if (isIncluded(StandardExcelSheetNames$Documentation, inputSections)) {
    scriptDocumentation <- getScriptDocumentation(excelFile)
    scriptContent <- c(scriptContent, scriptDocumentation)
  }

  scriptContent <- c(scriptContent, "require(ospsuite.reportingengine)", "")

  if (isIncluded(StandardExcelSheetNames$MeanModelSimulation, inputSections)) {
    meanModelTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$MeanModelSimulation)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(meanModelTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(meanModelTable)[2])

    simulationSetInfo <- getMeanModelSimulationSetContent(excelFile, meanModelTable)

    workflowContent <- getMeanModelWorkflowContent(
      simulationSetNames = simulationSetInfo$simulationSetNames,
      workflowFolder = workflowFolder
    )
  }

  if (isIncluded(StandardExcelSheetNames$PopulationSimulation, inputSections)) {
    populationTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$PopulationSimulation)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(populationTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(populationTable)[2])

    simulationSetInfo <- getPopulationSimulationSetContent(excelFile, populationTable)

    workflowContent <- getPopulationWorkflowContent(
      workflowTable = populationTable,
      simulationSetNames = simulationSetInfo$simulationSetNames,
      workflowFolder = workflowFolder
    )
  }

  outputContent <- NULL
  for (outputSheet in simulationSetInfo$outputSheets) {
    outputInfo <- getOutputContent(excelFile, outputSheet)
    outputContent <- c(outputContent, outputInfo)
  }
  
  taskContent <- ""
  
  runWorkflowContent <- "workflow$runWorkflow()"

  scriptContent <- c(
    scriptContent,
    outputContent,
    simulationSetInfo$simulationSetContent,
    workflowContent,
    taskContent,
    runWorkflowContent
  )
  
  # Write the script
  fileObject <- file(workflowFile, encoding = "UTF-8")
  write(scriptContent, file = fileObject, sep = "\n")
  close(fileObject)
  
  return(workflowFile)
}

getScriptDocumentation <- function(excelFile, colSep = "\t") {
  docContent <- NULL
  # Check behaviour for empty Documentation sheet
  suppressMessages(
    docTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$Documentation, col_names = FALSE)
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

getOutputContent <- function(excelFile, outputSheet) {
  outputContent <- NULL
  outputTable <- readxl::read_excel(excelFile, sheet = outputSheet)
  validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(outputTable)[1])
  # validateIsIncluded(WorkflowMandatoryVariables$Description, names(meanModelTable)[2])

  outputNames <- getOutputNames(excelFile, outputSheet)

  for (outputIndex in seq_along(outputNames)) {
    outputContent <- c(
      outputContent,
      paste0(
        outputNames[outputIndex], " <- Output$new(
          path = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$path), ", 
          displayName = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$reportName), ", 
          displayUnit = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$displayUnit), ", 
          dataSelection = ", getIdentifierInfo(outputTable, outputIndex, OutputCodeIdentifiers$dataSelection), "
        )"
      ),
      ""
    )
  }
  return(outputContent)
}

getMeanModelSimulationSetContent <- function(excelFile, workflowTable) {
  simulationSetContent <- NULL
  outputSheets <- NULL

  simulationSetNames <- names(workflowTable[, 3:ncol(workflowTable)])
  simulationSetNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = simulationSetNames)

  for (simulationIndex in seq_along(simulationSetNames)) {
    outputSheet <- getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$sheetOutput)
    outputSheets <- c(outputSheets, outputSheet)
    outputs <- paste0(getOutputNames(excelFile, outputSheet), collapse = ", ")

    observedMetaDataSheet <- getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$sheetDataDictTimeProfile)
    observedMetaDataFile <- getObservedMetaDataFile(excelFile, observedMetaDataSheet)

    simulationSetContent <- c(
      simulationSetContent,
      paste0(
        simulationSetNames[simulationIndex],
        " <- SimulationSet$new(
    simulationSetName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$name), ", 
    simulationFile = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$pkml), ", 
    simulationName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$reportName), ", 
    outputs = c(", outputs, "), 
    observedDataFile = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$dataFileTimeProfile), ", 
    observedMetaDataFile = ", observedMetaDataFile, ", 
    dataReportName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$dataReportName), ")"
      ),
      ""
    )
  }
  simulationSetContent <- c(simulationSetContent, "")
  return(list(
    simulationSetContent = simulationSetContent,
    simulationSetNames = simulationSetNames,
    outputSheets = unique(outputSheets)
  ))
}

getPopulationSimulationSetContent <- function(excelFile, workflowTable) {
  simulationSetContent <- NULL
  outputSheets <- NULL

  simulationSetNames <- names(workflowTable[, 3:ncol(workflowTable)])
  simulationSetNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = simulationSetNames)

  for (simulationIndex in seq_along(simulationSetNames)) {
    outputSheet <- getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$sheetOutput)
    outputSheets <- c(outputSheets, outputSheet)
    outputs <- paste0(getOutputNames(excelFile, outputSheet), collapse = ", ")

    observedMetaDataSheet <- getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$sheetDataDictTimeProfile)
    observedMetaDataFile <- getObservedMetaDataFile(excelFile, observedMetaDataSheet)

    simulationSetContent <- c(
      simulationSetContent,
      paste0(
        simulationSetNames[simulationIndex],
        " <- PopulationSimulationSet$new(
    referencePopulation = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$isReference), ",     
    simulationSetName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$name), ", 
    simulationFile = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$pkml), ", 
    simulationName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$reportName), ", 
    populationFile = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$popcsv), ", 
    populationName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$popReportName), ", 
    outputs = c(", outputs, "), 
    observedDataFile = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$dataFileTimeProfile), ", 
    observedMetaDataFile = ", observedMetaDataFile, ", 
    dataReportName = ", getIdentifierInfo(workflowTable, simulationIndex, WorkflowCodeIdentifiers$dataReportName), ")"
      ),
      ""
    )
  }
  simulationSetContent <- c(simulationSetContent, "")
  return(list(
    simulationSetContent = simulationSetContent,
    simulationSetNames = simulationSetNames,
    outputSheets = unique(outputSheets)
  ))
}

getMeanModelWorkflowContent <- function(simulationSetNames, workflowFolder = "Output") {
  workflowContent <- paste0(
    "simulationSets <- list(",
    paste0(simulationSetNames, collapse = ", "),
    ")"
  )

  workflowContent <- c(
    workflowContent,
    "",
    paste0('workflow <- MeanModelWorkflow$new(simulationSets, workflowFolder = "', workflowFolder, '")')
  )
  return(workflowContent)
}

getPopulationWorkflowContent <- function(workflowTable, simulationSetNames, workflowFolder = "Output") {
  workflowContent <- paste0(
    "simulationSets <- list(",
    paste0(simulationSetNames, collapse = ", "),
    ")"
  )

  workflowContent <- c(
    workflowContent,
    "",
    paste0(
      "workflow <- PopulationWorkflow$new(workflowType = ", getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$WorkflowMode),
      ', simulationSets, workflowFolder = "', workflowFolder, '")'
    )
  )
  return(workflowContent)
}


# Might be extended to output
WorkflowMandatoryVariables <- enum(c(
  "Code Identifier",
  "Description"
))

WorkflowCodeIdentifiers <- enum(c(
  "WorkflowType",
  "WorkflowMode",
  "isReference",
  "name",
  "reportName",
  "popcsv",
  "popReportName",
  "pkml",
  "studyDesign",
  "sheetOutput",
  "dataFileTimeProfile",
  "sheetDataDictTimeProfile",
  "dataSelection",
  "dataReportName"
))

OutputCodeIdentifiers <- enum(c(
  "path",
  "reportName",
  "displayUnit",
  "dataSelection"
))

getIdentifierInfo <- function(workflowTable, simulationIndex, codeId) {
  validateIsOfType(workflowTable, "data.frame")
  validateIsInteger(simulationIndex)
  validateIsIncluded(codeId, c(WorkflowCodeIdentifiers, OutputCodeIdentifiers))

  # Which is necessary because of NAs
  workflowID <- which(workflowTable[, WorkflowMandatoryVariables$`Code Identifier`] == codeId)
  # Shift of columns because Code identifier and Description are part of the table
  workflowInfo <- as.character(workflowTable[workflowID, simulationIndex + 2])

  # readxl::read_excel returns na for missing values which need to be passed on as NULL to simulation sets
  if (is.na(workflowInfo)) {
    return("NULL")
  }
  if (isIncluded(codeId, c(WorkflowCodeIdentifiers$sheetOutput, WorkflowCodeIdentifiers$sheetDataDictTimeProfile))) {
    return(workflowInfo)
  }
  if (isIncluded(codeId, WorkflowCodeIdentifiers$isReference)) {
    # Allows 1/0 and TRUE/FALSE for reference population
    return(as.character(workflowInfo == "1" | workflowInfo == "TRUE"))
  }
  return(paste0("'", workflowInfo, "'"))
}

getOutputNames <- function(excelFile, outputSheet) {
  outputTable <- readxl::read_excel(excelFile, sheet = outputSheet)
  outputNames <- paste(outputSheet, names(outputTable)[3:ncol(outputTable)], sep = "")
  # Remove spaces in the name
  outputNames <- gsub(pattern = "[[:space:]*]", replacement = "", x = outputNames)
  return(outputNames)
}

getObservedMetaDataFile <- function(excelFile, observedMetaDataSheet, format = "csv") {
  if (observedMetaDataSheet == "NULL") {
    return(observedMetaDataSheet)
  }
  observedMetaDataFilename <- paste0(observedMetaDataSheet, ".", format)
  observedMetaDataTable <- readxl::read_excel(excelFile, sheet = observedMetaDataSheet)
  # Save the data dictionary as a csv file, missing values stay missing (na = ""),
  # row numbers are not printed in the file (row.names = FALSE), file uses UTF-8 encoding (fileEncoding = "UTF-8")
  write.csv(observedMetaDataTable, file = observedMetaDataFilename, na = "", row.names = FALSE, fileEncoding = "UTF-8")
  return(paste0('"', observedMetaDataFilename, '"'))
}
