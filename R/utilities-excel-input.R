# ----- Helper enums -----
#' @title StandardExcelSheetNames
#' @description Enum defining the standard names for the Excel sheets of template
#' @import ospsuite.utils
#' @keywords internal
StandardExcelSheetNames <- enum(c(
  "Documentation",
  "Workflow and Tasks",
  "SimulationSets",
  "Data Sources",
  "Outputs",
  "Userdef PK Parameter",
  "PK Parameter",
  "SensitivityParameter",
  "tpDictionary"
))

#' @title WorkflowMandatoryVariables
#' @description Mandatory Columns for standard sheets
#' @import ospsuite.utils
#' @keywords internal
WorkflowMandatoryVariables <- enum(c(
  "Code Identifier",
  "Description"
))

#' @title OptionalSettings
#' @description Workflow optional settings
#' @keywords internal
OptionalSettings <- list(
  "reportFileName" = "workflow$reportFileName <- ",
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

#' @title WorkflowCodeIdentifiers
#' @description Workflow Code Identifiers
#' @import ospsuite.utils
#' @keywords internal
WorkflowCodeIdentifiers <- enum(c(
  "Workflow Mode",
  "Population Workflow type",
  "workflowFolder",
  "reportTitle",
  "reportFileName",
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

#' @title SimulationCodeIdentifiers
#' @description Simulation Sets Code Identifiers
#' @import ospsuite.utils
#' @keywords internal
SimulationCodeIdentifiers <- enum(c(
  "simulationSetName",
  "simulationFile",
  "outputs",
  "dataSource",
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
#' @title DataSourceCodeIdentifiers
#' @description Data Sources Code Identifiers
#' @import ospsuite.utils
#' @keywords internal
DataSourcesCodeIdentifiers <- enum(c(
  "dataFile",
  "DictionaryType",
  "DictionaryLocation",
  "caption"
))

#' @title OutputsCodeIdentifiers
#' @description Outputs Code Identifiers
#' @import ospsuite.utils
#' @keywords internal
OutputsCodeIdentifiers <- enum(c(
  "path",
  "displayName",
  "displayUnit",
  "groupID",
  "color",
  "fill",
  "dataSelection",
  "dataUnit",
  "dataDisplayName",
  "pkParameters"
))

#' @title UserDefPKParametersOptionalSettings
#' @description UserDefPKParametersOptionalSettings
#' @keywords internal
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

#' @title logicalCodeIds
#' @description Some of the identifiers above need to be treated
#' as logical by [getIdentifierInfo()] while usually input as `"Yes"`/`"No"`
#' @keywords internal
logicalCodeIds <- c(
  SimulationCodeIdentifiers$referencePopulation,
  SimulationCodeIdentifiers$plotReferenceObsData,
  WorkflowCodeIdentifiers$createWordReport,
  WorkflowCodeIdentifiers$`simulate: showProgress`,
  WorkflowCodeIdentifiers$`calculateSensitivity: showProgress`
)

#' @title asisCodeIds
#' @description Some of the identifiers above need to be treated
#' as is by [getIdentifierInfo()]
#' @keywords internal
asisCodeIds <- c(
  WorkflowCodeIdentifiers$`Workflow Mode`,
  WorkflowCodeIdentifiers$activitySpecificCode,
  SimulationCodeIdentifiers$StudyDesignType,
  SimulationCodeIdentifiers$StudyDesignLocation,
  SimulationCodeIdentifiers$timeOffset,
  DataSourcesCodeIdentifiers$DictionaryType,
  DataSourcesCodeIdentifiers$DictionaryLocation,
  OutputsCodeIdentifiers$pkParameters,
  setdiff(names(OptionalSettings), logicalCodeIds)
)

# ----- Exported Functions -----

#' @title createWorkflowFromExcelInput
#' @description Creates an R script for running mean model or population workflows from an Excel file
#' Note that the `{readxl}` package is required to use this function
#' @param excelFile name of the Excel file from which the R script is created
#' @param workflowFile name of the R script file created from the Excel file
#' @param removeComments logical to remove comments and information in `workflowFile`
#' @return An R script of name `workflowFile` to be run
#' @export
#' @family workflow helpers
createWorkflowFromExcelInput <- function(excelFile, workflowFile = "workflow.R", removeComments = FALSE) {
  # ----- Checks inputs -----
  if (!requireNamespace("readxl", quietly = TRUE)) {
    logError(c(
      paste0("Package '", highlight("readxl"), "' is required but not installed."),
      paste0("\t> use '", 'install.packages("readxl")', "' to install package")
    ))
    return(invisible())
  }

  logCatch({
    validateFileExists(excelFile)
    validateIsFileExtension(excelFile, c("xls", "xlsx"))
    validateIsFileExtension(workflowFile, "R")
    validateIsLogical(removeComments)

    # ----- Script initialization -----
    scriptContent <- NULL
    # Initialize warnings and errors
    scriptWarnings <- ExcelMessaging$new("warnings")
    scriptErrors <- ExcelMessaging$new("errors")


    inputSections <- readxl::excel_sheets(excelFile)
    # Check for mandatory input sections
    validateIsIncludedAndLog(c(
      StandardExcelSheetNames$`Workflow and Tasks`,
      StandardExcelSheetNames$SimulationSets
    ),
    inputSections,
    groupName = paste0("Sheet names of '", excelFile, "'")
    )

    # ----- Documentation -----
    if (isIncluded(StandardExcelSheetNames$`Documentation`, inputSections)) {
      scriptDocumentation <- getScriptDocumentation(excelFile)
      scriptContent <- c(scriptContent, scriptDocumentation)
    }

    scriptContent <- c(
      scriptContent,
      '# Load package "ospsuite.reportingengine" (require() function install the package if not installed yet)',
      "require(ospsuite.reportingengine)", ""
    )

    # ----- User Defined PK Parameters -----
    userDefPKParametersContent <- NULL
    if (isIncluded(StandardExcelSheetNames$`Userdef PK Parameter`, inputSections)) {
      userDefPKParametersTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Userdef PK Parameter`)
      validateIsIncluded(c("Name", "Standard PK parameter", "Display Unit"), names(userDefPKParametersTable))
      userDefPKParametersInfo <- getUserDefPKParametersContent(userDefPKParametersTable)
      userDefPKParametersContent <- userDefPKParametersInfo$content
      scriptWarnings$messages[["User Defined PK Parameters"]] <- userDefPKParametersInfo$warnings
      scriptErrors$messages[["User DefinedPK Parameters"]] <- userDefPKParametersInfo$errors
    }

    # ----- PK Parameters -----
    pkParametersContent <- NULL
    if (isIncluded(StandardExcelSheetNames$`PK Parameter`, inputSections)) {
      pkParametersTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`PK Parameter`)
      validateIsIncluded(c("Name", "Display name", "Unit"), names(pkParametersTable))
      pkParametersInfo <- getPKParametersContent(pkParametersTable)
      pkParametersContent <- pkParametersInfo$content
      scriptWarnings$messages[["PK Parameters"]] <- pkParametersInfo$warnings
      scriptErrors$messages[["PK Parameters"]] <- pkParametersInfo$errors
    }

    # ----- Workflow  -----
    workflowTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Workflow and Tasks`)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(workflowTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(workflowTable)[2])

    workflowInfo <- getWorkflowContent(workflowTable = workflowTable, excelFile = excelFile)
    workflowContent <- workflowInfo$content
    plotFormatContent <- workflowInfo$plotFormatContent
    activitySpecificContent <- workflowInfo$activitySpecificContent
    scriptWarnings$messages[["Workflow and Tasks"]] <- workflowInfo$warnings
    scriptErrors$messages[["Workflow and Tasks"]] <- workflowInfo$errors

    # ----- Simulation Sets -----
    simulationSetContent <- NULL
    simulationSetTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$SimulationSets)
    validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(simulationSetTable)[1])
    validateIsIncluded(WorkflowMandatoryVariables$Description, names(simulationSetTable)[2])

    simulationSetInfo <- getSimulationSetContent(excelFile, simulationSetTable, workflowMode = workflowInfo$workflowMode)
    simulationSetContent <- simulationSetInfo$content
    simulationSources <- simulationSetInfo$sources
    simulationOutputs <- simulationSetInfo$outputs
    scriptWarnings$messages[["Simulation Sets"]] <- simulationSetInfo$warnings
    scriptErrors$messages[["Simulation Sets"]] <- simulationSetInfo$errors

    # ----- Data Sources -----
    dataSourcesContent <- NULL
    useDataSources <- all(
      !isEmpty(simulationSources),
      isIncluded(StandardExcelSheetNames$`Data Sources`, inputSections)
    )
    if (useDataSources) {
      dataSourcesTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$`Data Sources`)
      validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(dataSourcesTable)[1])
      validateIsIncluded(WorkflowMandatoryVariables$Description, names(dataSourcesTable)[2])
      dataSourcesInfo <- getDataSourcesContent(excelFile, dataSourcesTable, simulationSources)
      dataSourcesContent <- dataSourcesInfo$content
      scriptWarnings$messages[["Data Sources"]] <- dataSourcesInfo$warnings
      scriptErrors$messages[["Data Sources"]] <- dataSourcesInfo$errors
    }

    # ----- Outputs -----
    outputsContent <- NULL
    useOutputs <- all(
      !isEmpty(simulationOutputs),
      isIncluded(StandardExcelSheetNames$Outputs, inputSections)
    )
    if (useOutputs) {
      outputsTable <- readxl::read_excel(excelFile, sheet = StandardExcelSheetNames$Outputs)
      validateIsIncluded(WorkflowMandatoryVariables$`Code Identifier`, names(outputsTable)[1])
      validateIsIncluded(WorkflowMandatoryVariables$Description, names(outputsTable)[2])
      outputsInfo <- getOutputsContent(excelFile, outputsTable, simulationOutputs)
      outputsContent <- outputsInfo$content
      scriptWarnings$messages[["Outputs"]] <- outputsInfo$warnings
      scriptErrors$messages[["Outputs"]] <- outputsInfo$errors
    }

    # ----- Run workflow -----
    runWorkflowContent <- c("", "workflow$runWorkflow()", "")

    # ----- Concatenate Sections -----
    scriptContent <- c(
      scriptContent,
      activitySpecificContent,
      plotFormatContent,
      userDefPKParametersContent,
      pkParametersContent,
      outputsContent,
      dataSourcesContent,
      simulationSetContent,
      workflowContent,
      runWorkflowContent
    )

    # Use styler to beautify and standardize the format of the output file
    # Used only if package is installed
    if (requireNamespace("styler", quietly = TRUE)) {
      scriptContent <- styler::style_text(scriptContent)
    }

    if (removeComments) {
      scriptContent <- removeCommentsFromWorkflowContent(scriptContent)
    }

    # Write the script
    fileObject <- file(workflowFile, encoding = "UTF-8")
    write(scriptContent, file = fileObject, sep = "\n")
    close(fileObject)
  })
  scriptErrors$displayMessage()
  scriptWarnings$displayMessage()
  logInfo(paste0("Script '", workflowFile, "' successfully created"))
  return(invisible(workflowFile))
}

# ----- Internal Functions -----

#' @title getScriptDocumentation
#' @description Get R script from Documentation sheet
#' @param excelFile name of the Excel file from which the R script is created
#' @param colSep Displayed column separator written in script
#' @return Character vector translating the Documentation sheet
#' @keywords internal
getScriptDocumentation <- function(excelFile, colSep = "\t") {
  docContent <- NULL
  suppressMessages({
    docTable <- readxl::read_excel(
      path = excelFile,
      sheet = StandardExcelSheetNames$`Documentation`,
      col_names = FALSE
    )
  })
  if (isEmpty(docTable)) {
    return(docContent)
  }
  for (lineIndex in 1:nrow(docTable)) {
    docContent <- c(
      docContent,
      # Each line is commented
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
  if (!hasOnlyDistinctValues(pkParametersTable$Name)) {
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
  if (!hasOnlyDistinctValues(pkParametersTable$`Display name`)) {
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

#' @title getOutputsContent
#' @description Creates a character vector to be written in a workflow .R script defining `Output` object.
#' @param excelFile name of the Excel file from which the R script is created
#' @param outputsTable Data.frame read from the Excel sheet "Outputs"
#' @param simulationOutputs Names of Output objects used by simulation sets
#' @return Character vector defining the `Output` object
#' @keywords internal
getOutputsContent <- function(excelFile, outputsTable, simulationOutputs) {
  outputsWarnings <- NULL
  outputsErrors <- NULL

  outputsNames <- getHeaderNames(outputsTable)
  outputsContent <- paste0(
    "# Create a Output objects named ", paste(outputsNames, collapse = ", "),
    " defining simulation output paths to be used in the workflow",
    "# The 'pkParameters' input can be a character array using defined PK Parameter names",
    "or an array of 'PkParameterInfo' objects"
  )
  # Check that all DataSource objects defined in simulation sets are included Data Sources table
  if (!isIncluded(simulationOutputs, outputsNames)) {
    undefinedOutputs <- setdiff(simulationOutputs, outputsNames)
    outputsErrors <- c(
      outputsErrors,
      paste0(
        "The following objects were not defined in the '", highlight("Outputs"), "' sheet: '",
        paste(highlight(undefinedOutputs), collapse = "', '"), "'"
      )
    )
  }
  if (!hasOnlyDistinctValues(outputsNames)) {
    outputsErrors <- c(
      outputsErrors,
      messages$errorHasNoUniqueValues(
        highlight(outputsNames),
        dataName = highlight("Output names")
      )
    )
  }
  for (outputsIndex in seq_along(outputsNames)) {
    pkParametersOutputContent <- NULL
    pkParametersSheet <- getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$pkParameters)
    if (!is.na(pkParametersSheet)) {
      validateIsIncludedAndLog(pkParametersSheet, readxl::excel_sheets(excelFile), groupName = paste0("Sheet names of '", excelFile, "'"))
      pkParametersInfo <- getPKParametersInfoContent(excelFile, pkParametersSheet)
      pkParametersContent <- pkParametersInfo$content

      outputsWarnings <- c(outputsWarnings, pkParametersInfo$warnings)
      outputsErrors <- c(outputsErrors, pkParametersInfo$errors)
      # Define PK Parameters before each Output definition
      outputsContent <- c(outputsContent, pkParametersContent)
      # The line break within paste0 is needed to ensure a nice structure in the output script
      pkParametersOutputContent <- paste0(",\npkParameters = pkParameters")
    }

    outputsContent <- c(
      outputsContent,
      paste0(
        outputsNames[outputsIndex], " <- Output$new(",
        "path = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$path), ",\n",
        "displayName = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$displayName), ",\n",
        "displayUnit = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$displayUnit), ",\n",
        "groupID = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$groupID), ",\n",
        "color = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$color), ",\n",
        "fill = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$fill), ",\n",
        "dataSelection = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$dataSelection), ",\n",
        "dataUnit = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$dataUnit), ",\n",
        "dataDisplayName = ", getIdentifierInfo(outputsTable, outputsIndex, OutputsCodeIdentifiers$dataDisplayName),
        pkParametersOutputContent,
        "\n)"
      ),
      ""
    )
  }
  return(list(
    content = outputsContent,
    warnings = outputsWarnings,
    errors = outputsErrors
  ))
}

#' @title getDataSourcesContent
#' @description Creates a character vector to be written in a workflow .R script defining `DataSource` objects.
#' @param excelFile name of the Excel file from which the R script is created
#' @param dataSourcesTable Data.frame read from the Excel sheet "DataSources"
#' @param simulationSources Names of DataSource objects used by simulation sets
#' @return Character vector defining the `DataSource` objects
#' @keywords internal
getDataSourcesContent <- function(excelFile, dataSourcesTable, simulationSources) {
  dataSourcesWarnings <- NULL
  dataSourcesErrors <- NULL

  dataSourcesNames <- getHeaderNames(dataSourcesTable)
  dataSourcesContent <- paste0(
    "# Create a DataSource objects named ", paste(dataSourcesNames, collapse = ", "),
    " defining observed datasets to be used in the workflow"
  )
  # Check that all DataSource objects defined in simulation sets are included Data Sources table
  if (!isIncluded(simulationSources, dataSourcesNames)) {
    undefinedSources <- setdiff(simulationSources, dataSourcesNames)
    dataSourcesErrors <- c(
      dataSourcesErrors,
      paste0(
        "The following objects were not defined in the '", highlight("Data Sources"), "' sheet: '",
        paste(highlight(undefinedSources), collapse = "', '"), "'"
      )
    )
  }
  if (!hasOnlyDistinctValues(dataSourcesNames)) {
    dataSourcesErrors <- c(
      dataSourcesErrors,
      messages$errorHasNoUniqueValues(
        highlight(dataSourcesNames),
        dataName = highlight("Data Source names")
      )
    )
  }
  for (dataSourceIndex in seq_along(dataSourcesNames)) {
    # Function for dictionary
    dictionaryType <- getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$DictionaryType)
    dictionaryLocation <- getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$DictionaryLocation)
    dictionaryLocation <- getFileLocationFromType(
      location = dictionaryLocation,
      type = dictionaryType,
      excelFile = excelFile
    )
    # Throw an error if file already exists and sheet tried to overwrite it
    # The simulation set input will still have the aimed dictionary file path
    if (is.null(dictionaryLocation)) {
      dataSourcesErrors <- c(
        dataSourcesErrors,
        paste0(
          "Dictionary file '",
          highlight(getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$DictionaryLocation)),
          ".csv' already exists and was not overwritten"
        )
      )
      dictionaryLocation <- paste0("'", getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$DictionaryLocation), ".csv'")
    }

    dataSourcesContent <- c(
      dataSourcesContent,
      paste0(
        dataSourcesNames[dataSourceIndex],
        " <- DataSource$new(",
        "dataFile = ", getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$dataFile), ",\n",
        "metaDataFile = ", dictionaryLocation, ", \n",
        "caption = ", getIdentifierInfo(dataSourcesTable, dataSourceIndex, DataSourcesCodeIdentifiers$caption),
        "\n)"
      ),
      ""
    )
  }

  return(list(
    content = dataSourcesContent,
    warnings = dataSourcesWarnings,
    errors = dataSourcesErrors
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
  simulationSetWarnings <- NULL
  simulationSetErrors <- NULL

  simulationSetNames <- getHeaderNames(simulationTable)
  simulationType <- getSimulationSetType(workflowMode)

  outputsNames <- NULL
  dataSourceNames <- NULL
  for (simulationIndex in seq_along(simulationSetNames)) {
    # Get data source and outputs for checking
    outputsId <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$outputs)
    dataSourceId <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$dataSource)
    outputsNames <- c(
      outputsNames,
      outputNamesFromIdentifier(outputsId)
    )
    dataSourceNames <- c(
      dataSourceNames,
      dataSourceNamesFromIdentifier(dataSourceId)
    )
    # MeanModelWorkflow doesn't use population fields, which are set to NULL
    populationFileContent <- NULL
    populationNameContent <- NULL
    referencePopulationContent <- NULL
    plotReferenceObsDataContent <- NULL
    studyDesignFileContent <- NULL
    if (isIncluded(workflowMode, "PopulationWorkflow")) {
      referencePopulation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$referencePopulation)
      plotReferenceObsData <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$plotReferenceObsData)
      populationFile <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$populationFile)
      populationName <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$populationName)

      studyDesignType <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignType)
      studyDesignLocation <- getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$StudyDesignLocation)
      studyDesignLocation <- getFileLocationFromType(
        location = studyDesignLocation,
        type = studyDesignType,
        excelFile = excelFile
      )
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
      referencePopulationContent <- paste0("referencePopulation = ", referencePopulation, ",\n")
      plotReferenceObsDataContent <- paste0("plotReferenceObsData = ", plotReferenceObsData, ",\n")
      populationFileContent <- paste0("populationFile = ", populationFile, ",\n")
      populationNameContent <- paste0("populationName = ", populationName, ",\n")
      studyDesignFileContent <- paste0("studyDesignFile = ", studyDesignLocation, ",\n")
    }

    simulationSetContent <- c(
      simulationSetContent,
      paste0(
        "# Create a SimulationSet object of name '", simulationSetNames[simulationIndex],
        "' defining which simulation, its Output objects, observed dataset ",
        "and potentially its population to use in the workflow"
      ),
      paste0(
        "# Note that workflows of type 'MeanModelWorkflow' require SimulationSet objects, ",
        "while workflows of type 'PopulationWorkflow' require PopulationSimulationSet objects"
      ),
      paste0(
        simulationSetNames[simulationIndex],
        " <- ", simulationType, "$new(\n",
        "simulationSetName = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationSetName), ",\n",
        referencePopulationContent,
        populationFileContent,
        populationNameContent,
        plotReferenceObsDataContent,
        studyDesignFileContent,
        "simulationFile = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationFile), ",\n",
        "outputs = ", outputsId, ",\n",
        "dataSource = ", dataSourceId, ",\n",
        "timeUnit = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$timeUnit), ",\n",
        "timeOffset = ", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$timeOffset), "\n)"
      ),
      ""
    )
    # Check that pkml file has correct extension
    simulationFile <- gsub("'", "", getIdentifierInfo(simulationTable, simulationIndex, SimulationCodeIdentifiers$simulationFile))
    if (!isFileExtension(simulationFile, "pkml")) {
      simulationSetErrors <- c(
        simulationSetErrors,
        paste0(
          "In simulation set '", highlight(simulationSetNames[simulationIndex]), "', ",
          messages$errorExtension(simulationFile, "pkml")
        )
      )
    }
  }
  simulationSetContent <- c(
    simulationSetContent,
    "",
    paste0("simulationSets <- list(", paste0(simulationSetNames, collapse = ", "), ")"),
    ""
  )

  return(list(
    content = simulationSetContent,
    simulationSetNames = simulationSetNames,
    outputs = unique(outputsNames),
    sources = unique(dataSourceNames),
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
    workflowTypeContent <- paste0("workflowType = ", workflowType, ",\n")
  }
  workflowFolder <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$workflowFolder)
  workflowTitle <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$reportTitle)
  createWordReport <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$createWordReport)
  simulationSetDescriptor <- getIdentifierInfo(workflowTable, 1, WorkflowCodeIdentifiers$simulationSetDescriptor)
  workflowContent <- paste0(
    "workflow <- ", workflowMode, "$new(\n",
    workflowTypeContent,
    "simulationSets = simulationSets,\n",
    "workflowFolder = ", workflowFolder, ",\n",
    "reportTitle = ", workflowTitle, ",\n",
    "createWordReport = ", createWordReport, ",\n",
    "simulationSetDescriptor = ", simulationSetDescriptor, "\n)"
  )

  # Activate tasks as defined by user
  isActive <- lapply(AllAvailableTasks, function(x) {
    FALSE
  })
  for (taskName in AllAvailableTasks) {
    activeTaskName <- getIdentifierInfo(workflowTable, 1, taskName)
    if (isEmpty(activeTaskName)) {
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
  optionalSettingContent <- NULL
  for (optionalSettingName in names(OptionalSettings)) {
    settingValue <- getIdentifierInfo(workflowTable, 1, optionalSettingName)
    if (is.na(settingValue) | isIncluded(settingValue, "NULL")) {
      next
    }
    if (isIncluded(
      optionalSettingName,
      OptionalSettings$`calculateSensitivity: variableParameterPaths`
    )) {
      settingValue <- getSensitivityVariableParameterPaths(excelFile, sensitivityParametersSheet = settingValue)
    }
    optionalSettingContent <- c(
      optionalSettingContent,
      paste0(
        OptionalSettings[[optionalSettingName]],
        settingValue
      )
    )
  }
  if (!isEmpty(optionalSettingContent)) {
    workflowContent <- c(
      workflowContent,
      "",
      "# Update workflow or tasks settings",
      optionalSettingContent
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
  validateIsIncluded(codeId, c(
    WorkflowCodeIdentifiers,
    SimulationCodeIdentifiers,
    DataSourcesCodeIdentifiers,
    OutputsCodeIdentifiers
  ))

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

  # For info of type sheet or numeric, return directly sheet name or value
  if (isIncluded(codeId, asisCodeIds)) {
    return(workflowInfo)
  }
  # readxl::read_excel returns na for missing values
  # These usually need to be passed on as NULL inputs
  if (is.na(workflowInfo)) {
    return("NULL")
  }
  # For simulation set outputs, wrap with c()
  if (isIncluded(codeId, SimulationCodeIdentifiers$outputs)) {
    return(paste0("c(", workflowInfo, ")"))
  }
  # For simulation set dataSource use as is
  if (isIncluded(codeId, SimulationCodeIdentifiers$dataSource)) {
    return(workflowInfo)
  }
  # For logical info Yes/No input is enforced by template
  # and translated as TRUE/FALSE
  if (isIncluded(codeId, logicalCodeIds)) {
    # Will return false if input is not included in Yes
    return(as.character(isIncluded(workflowInfo, c("Yes", "YES", "1", "TRUE", "true"))))
  }
  # For any other info, it needs to be returned in between quotes
  # because expected as character value
  return(paste0("'", workflowInfo, "'"))
}

#' @title getHeaderNames
#' @description Get the names of objects in header
#' @param excelTable table read from Excel sheet
#' @return names of objects in header
#' @keywords internal
getHeaderNames <- function(excelTable) {
  if (ncol(excelTable) < 3) {
    return(NULL)
  }
  excelHeaderNames <- names(excelTable)[3:ncol(excelTable)]
  return(gsub(pattern = "[[:space:]*]", replacement = "", x = excelHeaderNames))
}

#' @title outputNamesFromIdentifier
#' @description Get the names of output from simulation set identifier
#' @param outputsId Content translated from cell corresponding to outputs in SimulationSets sheet.
#' The `outputsId` are expected as `"c(outputName1, outputName2, etc.)"`
#' @return names of Output objects in SimulationSets sheet
#' @keywords internal
outputNamesFromIdentifier <- function(outputsId) {
  if (isIncluded(outputsId, "NULL")) {
    return(NULL)
  }
  # Remove the wrapping c()
  outputsId <- substring(outputsId, first = 3, last = nchar(outputsId) - 1)
  outputsNames <- unlist(strsplit(outputsId, split = ","))
  return(trimws(outputsNames))
}

#' @title dataSourceNamesFromIdentifier
#' @description Get the names of dataSource from simulation set identifier
#' @param dataSourceId Content translated from cell corresponding to dataSource in SimulationSets sheet.
#' @return names of Output objects in SimulationSets sheet
#' @keywords internal
dataSourceNamesFromIdentifier <- function(dataSourceId) {
  if (isIncluded(dataSourceId, "NULL")) {
    return(NULL)
  }
  return(trimws(dataSourceId))
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
  if (isEmpty(pkParametersTable)) {
    return(pkParametersContent)
  }
  # Check for duplicate PK parameters as input of Output object
  if (!hasOnlyDistinctValues(pkParametersTable$Name)) {
    pkParametersWarnings <- c(
      pkParametersWarnings,
      messages$errorHasNoUniqueValues(pkParametersTable$Name, dataName = "PK parameters update")
    )
  }
  # Check for duplicate PK parameter display names as input of Output object
  if (!hasOnlyDistinctValues(pkParametersTable$`Display name`)) {
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
  if (isEmpty(userDefPKParametersTable)) {
    return(userDefPKParametersContent)
  }
  # Check for duplicate PK parameters as input of Output object
  if (!hasOnlyDistinctValues(userDefPKParametersTable$Name)) {
    userDefPKParametersErrors <- c(
      userDefPKParametersErrors,
      messages$errorHasNoUniqueValues(
        userDefPKParametersTable$Name,
        dataName = "User Defined PK parameters"
      )
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
      if (isEmpty(userDefinedSetting)) {
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
