#' @title loadQualificationWorkflow
#' @description Load a `ConfigurationPlan` object from json file
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
#' @return A `QualificationWorkflow` object
#' @export
loadQualificationWorkflow <- function(workflowFolder, configurationPlanFile) {
  configurationPlan <- loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = configurationPlanFile
  )

  outputsDataframe <- getOutputsFromConfigurationPlan(configurationPlan = configurationPlan)

  # TODO outputs and pk parameters may need to be defined
  # Either as part of simulationSets or as a settings for the task
  # Currently, no output is saved as simulationResults
  simulationSets <- list()
  for (simulationIndex in 1:nrow(configurationPlan$simulationMappings)) {
    project <- configurationPlan$simulationMappings$project[simulationIndex]
    simulationName <- configurationPlan$simulationMappings$simulation[simulationIndex]
    simulationFile <- configurationPlan$getSimulationPath(project = project, simulation = simulationName)

    cat(paste0(
      "Creating output objects for ", simulationIndex, "/", nrow(configurationPlan$simulationMappings),
      ": Project '", project, "' - Simulation '", simulationName, "'\n"
    ))

    outputsDataframeSubset <- outputsDataframe[outputsDataframe$project == project & outputsDataframe$simulation == simulationName, ]

    outputs <- lapply(unique(outputsDataframeSubset$outputPath), function(outputPath) {
      Output$new(
        path = as.character(outputPath),
        pkParameters = outputsDataframeSubset$pkParameter[outputsDataframeSubset$outputPath == outputPath & !(is.na(outputsDataframeSubset$pkParameter))]
      )
    })

    minimumSimulationEndTime <- NULL
    if (any(!is.na(outputsDataframeSubset$endTime))) {
      minimumSimulationEndTime <- max(outputsDataframeSubset$endTime, na.rm = TRUE)
    }

    # simulationSetName defined as project-simulation uniquely identifies the simulation
    simulationSets[[simulationIndex]] <- SimulationSet$new(
      simulationSetName = paste(project, simulationName, sep = "-"),
      simulationFile = simulationFile,
      outputs = c(outputs),
      minimumSimulationEndTime = minimumSimulationEndTime
    )
  }
  workflow <- QualificationWorkflow$new(
    simulationSets = simulationSets,
    workflowFolder = workflowFolder,
    configurationPlan = configurationPlan
  )
  return(workflow)
}

#' @title loadConfigurationPlan
#' @description Load a `ConfigurationPlan` object from json file
#' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @return A `ConfigurationPlan` object including the content of json file
#' @export
loadConfigurationPlan <- function(configurationPlanFile, workflowFolder) {
  jsonConfigurationPlan <- jsonlite::fromJSON(configurationPlanFile, simplifyDataFrame = FALSE)

  # Check if mandatory variables were input
  # Matlab version had as well ObservedDataSets and Inputs, but they don't need to be mandatory in R
  validateIsIncluded(c("SimulationMappings", "Plots", "Sections"), names(jsonConfigurationPlan))

  # Create `ConfigurationPlan` object
  configurationPlan <- ConfigurationPlan$new()
  # The workflow and reference folders are required to know from where accessing the files
  configurationPlan$workflowFolder <- workflowFolder
  configurationPlan$referenceFolder <- normalizePath(path = dirname(configurationPlanFile), winslash = .Platform$file.sep)

  # Assiociate fields defined in json to ConfigurationPlan object using expression
  jsonFieldNames <- names(jsonConfigurationPlan)
  # jsonFieldNames is almost camel case, only first letter needs to be switched to lower case
  fieldNames <- paste0(tolower(substring(jsonFieldNames, 1, 1)), substring(jsonFieldNames, 2))
  eval(parse(text = paste0("configurationPlan$", fieldNames, "<- jsonConfigurationPlan$", jsonFieldNames)))

  return(configurationPlan)
}

#' @title sectionsAsDataFrame
#' @description Recursively parse Sections field of configuration plan
#' to create a data.frame easier to handle by the wrkflow
#' @param sectionsIn list including Id and Title of section
#' @param sectionsOut data.frame including id, path, title
#' @param parentFolder For subsections only, path of parent section
#' @param sectionLevel Section level defining the level of markdown title
#' @return A data.frame including information about every section and subsection
sectionsAsDataFrame <- function(sectionsIn, sectionsOut = data.frame(), parentFolder = NULL, sectionLevel = 1) {
  # If sections are already as a data.frame format,
  # return them after checking that every field is present
  if (isOfType(sectionsIn, "data.frame")) {
    validateIsIncluded(c("id", "title", "content", "index", "path", "md"), names(sectionsIn))
    return(sectionsIn)
  }
  # Parse every section
  for (section in sectionsIn) {
    # sectionIndex ensures that folder names are in correct order and have unique names
    sectionIndex <- nrow(sectionsOut) + 1
    validateIsIncluded(c("Id", "Title"), names(section))
    # Actual section path will be relative to the workflowFolder
    # and is wrapped in method configurationPlan$getSectionPath(id)
    sectionPath <- paste0(parentFolder,
      sprintf("%0.3d_%s", sectionIndex, removeForbiddenLetters(section$Title)),
      sep = .Platform$file.sep
    )

    sectionMarkdown <- sprintf("%0.3d_%s.md", sectionIndex, removeForbiddenLetters(section$Title))

    # section data.frame with every useful information
    sectionOut <- data.frame(
      id = section$Id,
      title = section$Title,
      content = section$Content %||% NA,
      index = sectionIndex,
      path = sectionPath,
      md = sectionMarkdown,
      level = sectionLevel,
      stringsAsFactors = FALSE
    )
    sectionsOut <- rbind.data.frame(sectionsOut, sectionOut, stringsAsFactors = FALSE)

    # If subsections are included and not empty
    # Update sectionsOut data.frame
    if (!isOfLength(section$Sections, 0)) {
      sectionsOut <- sectionsAsDataFrame(
        sectionsIn = section$Sections,
        sectionsOut = sectionsOut,
        parentFolder = sectionPath,
        sectionLevel = sectionLevel + 1
      )
    }
  }
  return(sectionsOut)
}

#' @title createSectionOutput
#' @description Create the Sections output and their markdown content
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder path where logs are saved
#' @return Names of created appendices
createSectionOutput <- function(configurationPlan, logFolder = getwd()) {
  # Intro
  resetReport(configurationPlan$getIntroMarkdown())
  appendices <- configurationPlan$getIntroMarkdown()
  for (intro in configurationPlan$intro) {
    configurationPlan$copyIntro(intro, logFolder = logFolder)
  }
  # Sections
  for (sectionIndex in configurationPlan$sections$index) {
    sectionId <- configurationPlan$sections$id[sectionIndex]
    # Create section output path
    dir.create(configurationPlan$getSectionPath(sectionId), showWarnings = FALSE, recursive = TRUE)
    # Initialize markdown appendices
    resetReport(configurationPlan$getSectionMarkdown(sectionId), logFolder = logFolder)
    appendices <- c(appendices, configurationPlan$getSectionMarkdown(sectionId))

    # Add info from content or title to section content
    configurationPlan$copySectionContent(sectionId, logFolder = logFolder)
  }
  # Inputs
  for (input in configurationPlan$inputs) {
    configurationPlan$copyInput(input, logFolder = logFolder)
  }
  return(appendices)
}



#' @title getOutputsFromTimeProfileConfiguration
#' @description Get a vector of output paths that are to be used in a time profile plot
#' @param plot is a descriptor of the plot that is read from the `ConfigurationPlan` that is generated by the configuration plan json
#' @return A vector of output paths that are to be used in the time profile plot descriptor `plot`
getOutputsFromTimeProfileConfiguration <- function(plot) {
  validateIsIncluded(values = "Plot", parentValues = names(plot), nullAllowed = TRUE)
  validateIsIncluded(values = "Curves", parentValues = names(plot[["Plot"]]), nullAllowed = FALSE)

  paths <- NULL
  for (curve in plot$Plot$Curves) {
    validateIsString(object = curve$Y)
    if (ospsuite::toPathArray(curve$Y)[2] == "ObservedData") {
      next
    }
    paths <- c(paths, ospsuite::toPathString(tail(ospsuite::toPathArray(curve$Y), -1)))
  }
  return(unique(paths))
}



#' @title getOutputsFromGOFMergedPlotsConfiguration
#' @description Get a vector of output paths that are to be used in a GOF merged plot
#' @param plot is a descriptor of the plot that is read from the `ConfigurationPlan` that is generated by the configuration plan json
#' @return A vector of output paths that are to be used in the GOF merged plot descriptor `plot`
getOutputsFromGOFMergedPlotsConfiguration <- function(plot) {
  validateIsIncluded(values = "Groups", parentValues = names(plot), nullAllowed = TRUE)
  paths <- NULL
  for (group in plot$Groups) {
    validateIsIncluded(values = "OutputMappings", parentValues = names(group), nullAllowed = TRUE)
    for (outputMapping in group$OutputMappings) {
      validateIsIncluded(values = "Output", parentValues = names(outputMapping), nullAllowed = TRUE)
      validateIsString(object = outputMapping$Output)
      paths <- c(paths, outputMapping$Output)
    }
  }
  return(unique(paths))
}


# Vector of allowable plot types in configuration plan.
# TODO deprecate vector using enum ConfigurationPlots
QualificationPlotTypes <- c("GOFMergedPlots", "ComparisonTimeProfilePlots", "DDIRatioPlots", "TimeProfile")




#' @title separateVariableFromUnit
#' @description Split a string containing a varialbe and unit description into a list of two strings that separates the variable and the unit.
#' @param variableUnitString is a string that has the format 'Variable [unit]' or 'Variable'
#' @return A named list, with fields 'name' and 'unit'.  If variableUnitString lacks a '[unit]' substring, then the unit field is returned as an empty string.
separateVariableFromUnit <- function(variableUnitString) {
  splitVariableUnitString <- strsplit(x = sub("[ []", replacement = "", x = variableUnitString), split = "[][]")[[1]]
  return(list(
    name = trimws(splitVariableUnitString[1]),
    unit = ifelse(test = is.na(x = tail(splitVariableUnitString, 1)), yes = "", no = tail(splitVariableUnitString, 1))
  ))
}


#' @title parseObservationsDataFrame
#' @description Function to read the variable names and units in the first two columns of an observations data frame used for qualification. First column stores timepoints.  Second column stores measurements of a quantity corresponding to timepoints in first column.
#' @param observationsDataFrame from CSV
#' @return A named list with `time` and `output` fields.  Each field contains a list that is output by `separateVariableFromUnit` with fields that store the variable name and the unit.
parseObservationsDataFrame <- function(observationsDataFrame) {
  namesObservationsDataFrame <- names(observationsDataFrame)
  validateIsIncluded(length(namesObservationsDataFrame), c(2, 3))
  dataFrameFields <- list(
    time = separateVariableFromUnit(namesObservationsDataFrame[1]),
    output = separateVariableFromUnit(namesObservationsDataFrame[2])
  )
  if (length(namesObservationsDataFrame) == 3) {
    dataFrameFields$error <- separateVariableFromUnit(namesObservationsDataFrame[3])
  }
  return(dataFrameFields)
}



#' @title massMoleConversion
#' @description Function to map `Concentration (mass)` or `Mass` dimensions to `Concentration (molar)` and `Amount` respectively.
#' @param dimension is string that is from among the valid OSP dimensions
#' @return If `dimension` is `Concentration (mass)` or `Mass`, then return `Concentration (molar)` or `Amount` respectively, otherwise return `dimension`.
massMoleConversion <- function(dimension) {
  massMoleConversionList <- list()
  massMoleConversionList[[ospDimensions$Mass]] <- ospsuite::ospDimensions$Amount
  massMoleConversionList[[ospDimensions$`Concentration (mass)`]] <- ospsuite::ospDimensions$`Concentration (molar)`
  return(ifelse(test = dimension %in% c(ospsuite::ospDimensions$Mass, ospsuite::ospDimensions$`Concentration (mass)`),
    yes = massMoleConversionList[[dimension]],
    no = dimension
  ))
}


#' @title getAxesSettings
#' @description Read axes settings for plots.
#' @param axesSettingsFromConfigurationPlot is a field from the `configurationPlan$plots` list
#' @return `axesSettings`, a list of settings for each of the X and Y axis.  Each list contains the unit, dimensions, and scaling type for each axes and option to plot grid lines.
getAxesSettings <- function(axesSettingsFromConfigurationPlot) {
  axesSettings <- lapply(axesSettingsFromConfigurationPlot, function(x) {
    list(
      unit = x$Unit,
      dimension = x$Dimension,
      gridLines = x$GridLines,
      scaling = x$Scaling
    )
  })
  names(axesSettings) <- sapply(axesSettingsFromConfigurationPlot, function(x) x$Type)
  return(axesSettings)
}


#' @title startQualificationRunner
#' @description Starts the qualification runner and creates inputs for the reporting engine
#' @param qualificationRunnerFolder Folder where QualificationRunner.exe is located
#' @param qualificationPlanFile full path of the input qualification plan
#' @param outputFolder Name of output folder created by the qualification runner
#' @param options List or array of options inspired from Matlab version of `startQualificationRunner`
#' \itemize{
#' \item `'-f'` or `'--force'`:    If set to true, the contents of the output folder will be deleted, even if it not empty. Default is false
#' \item `'-n <name>'`:          Name of the report qualification plan to be generated. Default is 'report-configuration-plan.json'
#' \item `'-p <PKSim Folder>'`:  Path of PK-Sim installation folder. If not specified, installation path will be read
#' from the registry (available only in case of full (non-portable) installation). This option is MANDATORY for the portable version of PK-Sim
#' \item `'-l <logFile>'`:       Full path of log file where log output will be written. A log file will not be created if this value is not provided.
#' \item `'-a'` or `'--append'`:   true to append data to the file; false to overwrite the file (default). If the specified file does not exist, this parameter has no effect, and a new file is created.
#' \item `'--logLevel <Level>'`: Log verbosity (Debug, Information, Warning, Error). Default is Information.
#' \item `'--version'`:          Display version information.
#' } 
#' @export
startQualificationRunner <- function(qualificationRunnerFolder, qualificationPlanFile, outputFolder, options = NULL){
  optionalArguments <- paste0(options, collapse = ' ')
  qualificationRunner <- paste0(file.path(qualificationRunnerFolder, 'QualificationRunner.exe'))
  arguments <- paste0(' -i "', qualificationPlanFile,  '" -o "', outputFolder, '" ', optionalArguments)
  shell(paste0(qualificationRunner, arguments))
  return(invisible())
}
