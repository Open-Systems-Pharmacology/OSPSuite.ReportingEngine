#' @title loadQualificationWorkflow
#' @description Load a `ConfigurationPlan` object from json file
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
#' @return A `QualificationWorkflow` object
#' @export
#' @family qualification workflow
loadQualificationWorkflow <- function(workflowFolder, configurationPlanFile) {
  setLogFolder(workflowFolder)
  logCatch({
    validateIsFileExtension(configurationPlanFile, "json")
    validateFileExists(configurationPlanFile)
    configurationPlan <- loadConfigurationPlan(
      workflowFolder = workflowFolder,
      configurationPlanFile = configurationPlanFile
    )
  })

  outputsDataframe <- getOutputsFromConfigurationPlan(configurationPlan = configurationPlan)

  simulationSets <- list()
  # Display a nice progress bar
  t0 <- tic()
  logInfo(paste0("Loading Simulations onto ", highlight("Qualification Workflow")))
  loadingProgress <- txtProgressBar(max = nrow(configurationPlan$simulationMappings), style = 3)

  logCatch({
    for (simulationIndex in seq_len(nrow(configurationPlan$simulationMappings))) {
      project <- configurationPlan$simulationMappings$project[simulationIndex]
      simulationName <- configurationPlan$simulationMappings$simulation[simulationIndex]
      simulationSetName <- paste(project, configurationPlan$simulationMappings$simulationFile[simulationIndex], sep = "-")
      simulationFile <- configurationPlan$getSimulationPath(project = project, simulation = simulationName)
      validateFileExists(path = simulationFile)
      populationFile <- configurationPlan$getPopulationPath(project = project, simulation = simulationName)
      validateFileExists(path = populationFile, nullAllowed = TRUE)

      outputsDataframeSubset <- outputsDataframe[outputsDataframe$project == project & outputsDataframe$simulation == simulationName, ]
      if (nrow(outputsDataframeSubset) == 0) {
        next
      }

      # Display messages about simulations to load
      cat(paste0("\nLoading project '", highlight(project), "', and simulation '", highlight(simulationName), ".'\n"))

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
      if (!is.null(populationFile)) {
        simulationSets <- c(simulationSets, PopulationSimulationSet$new(
          simulationSetName = simulationSetName,
          simulationFile = simulationFile,
          populationFile = populationFile,
          outputs = c(outputs),
          minimumSimulationEndTime = minimumSimulationEndTime
        ))
        # Update progress bar after each simulation is loaded
        setTxtProgressBar(loadingProgress, value = simulationIndex)
        next
      }

      simulationSets <- c(simulationSets, SimulationSet$new(
        simulationSetName = simulationSetName,
        simulationFile = simulationFile,
        outputs = c(outputs),
        minimumSimulationEndTime = minimumSimulationEndTime
      ))
      # Update progress bar after each simulation is loaded
      setTxtProgressBar(loadingProgress, value = simulationIndex)
    }
  })
  close(loadingProgress)
  logInfo(messages$runCompleted(getElapsedTime(t0), "Simulations loading"))

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
#' @import jsonlite
#' @export
#' @family qualification workflow
loadConfigurationPlan <- function(configurationPlanFile, workflowFolder) {
  setLogFolder(workflowFolder)
  logCatch({
    jsonConfigurationPlan <- fromJSON(configurationPlanFile, simplifyDataFrame = FALSE)

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

    # Create unique plot number for each plot named "PlotNumber"
    # within a specific plot type (eg. PKRatio) defined in the configuration plan
    plotFields <- setdiff(names(configurationPlan$plots), c("PlotSettings", "AxesSettings"))
    for (plotField in plotFields) {
      for (plotIndex in seq_along(configurationPlan$plots[[plotField]])) {
        # In json, numbering of fields in {} starts at 0
        # Actual plot index should start at 1
        configurationPlan$plots[[plotField]][[plotIndex]]$PlotNumber <- paste(
          plotIndex - 1, "(json numbering starting at 0); ",
          plotIndex, "(actual plot index)"
        )
      }
    }
  })
  return(configurationPlan)
}

#' @title sectionsAsDataFrame
#' @description Recursively parse Sections field of configuration plan
#' to create a data.frame easier to handle by the workflow
#' @param sectionsIn list including Id and Title of section
#' @param sectionsOut data.frame including id, path, title
#' @param parentFolder For subsections only, path of parent section
#' @param sectionLevel Section level defining the level of markdown title
#' @return A data.frame including information about every section and subsection
#' @importFrom ospsuite.utils %||%
#' @keywords internal
sectionsAsDataFrame <- function(sectionsIn, sectionsOut = data.frame(), parentFolder = "images", sectionLevel = 1) {
  # If sections are already as a data.frame format,
  # return them after checking that every field is present
  if (isOfType(sectionsIn, "data.frame")) {
    validateIsIncluded(c("id", "reference", "title", "content", "index", "path", "md"), names(sectionsIn))
    return(sectionsIn)
  }
  # Parse every section
  for (section in sectionsIn) {
    # sectionIndex ensures that folder names are in correct order and have unique names
    sectionIndex <- nrow(sectionsOut) + 1
    reference <- section$Reference %||% section$Id %||% paste0("undefined-section-", sectionIndex)
    validateIsIncluded("Title", names(section))
    # Actual section path will be relative to the workflowFolder
    # and is wrapped in method configurationPlan$getSectionPath(id)
    sectionPath <- paste(
      parentFolder,
      sprintf("%0.3d_section_%s", sectionIndex, reference),
      sep = .Platform$file.sep
    )

    sectionMarkdown <- sprintf("%0.3d_%s.md", sectionIndex, removeForbiddenLetters(section$Title))

    # section data.frame with every useful information
    sectionOut <- data.frame(
      # Sections with no reference nor id are allowed
      # Such sections won't appear in the table of content
      id = reference,
      title = section$Title,
      # Empty content is allowed and translated by NA
      content = section$Content %||% NA,
      index = sectionIndex,
      path = sectionPath,
      md = sectionMarkdown,
      level = sectionLevel,
      stringsAsFactors = FALSE
    )
    sectionsOut <- rbind.data.frame(sectionsOut, sectionOut, stringsAsFactors = FALSE)
    validateNoDuplicate(values = sectionsOut$id, variableName = "'Id' and 'Reference' of 'Sections'")

    # If subsections are included and not empty
    # Update sectionsOut data.frame
    if (!isEmpty(section$Sections)) {
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
#' @return Names of created appendices
#' @keywords internal
createSectionOutput <- function(configurationPlan) {
  # Introduction
  reportIntro <- NULL
  if (!isEmpty(configurationPlan$intro)) {
    # Centralize content of possibly multiple intro files in one md intro
    reportIntro <- configurationPlan$getIntroMarkdown()
    resetReport(reportIntro)
    for (intro in configurationPlan$intro) {
      configurationPlan$copyIntro(intro)
    }
  }

  # Sections
  appendices <- NULL
  for (sectionIndex in configurationPlan$sections$index) {
    sectionId <- configurationPlan$sections$id[sectionIndex]
    # Create section output path
    dir.create(configurationPlan$getSectionPath(sectionId), showWarnings = FALSE, recursive = TRUE)
    # Initialize markdown appendices
    resetReport(configurationPlan$getSectionMarkdown(sectionId))
    appendices <- c(appendices, configurationPlan$getSectionMarkdown(sectionId))

    # Add info from content or title to section content
    configurationPlan$copySectionContent(sectionId)
  }
  # Inputs
  for (input in configurationPlan$inputs) {
    configurationPlan$copyInput(input)
  }
  # Images and other raw content to copy
  configurationPlan$copyContentFiles()
  return(list(
    intro = reportIntro,
    appendices = appendices
  ))
}



#' @title getOutputsFromTimeProfileConfiguration
#' @description Get a vector of output paths that are to be used in a time profile plot
#' @param plot is a descriptor of the plot that is read from the `ConfigurationPlan` that is generated by the configuration plan json
#' @return A vector of output paths that are to be used in the time profile plot descriptor `plot`
#' @keywords internal
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
#' @keywords internal
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

#' @title extractNameAndUnit
#' @description Returns a named list with two entries (name, unit) corresponding to the name and unit
#' extracted out of the `text` provided as parameter
#' @param text Character from which name and unit are extracted
#' @return A named list, with fields `name` and `unit`.
#' @import ospsuite.utils
#' @keywords internal
#' @examples \dontrun{
#' res <- extractNameAndUnit("Value [mg]")
#' res$name
#' # > "Value"
#' res$unit
#' # > "mg"
#'
#' res <- extractNameAndUnit("Value")
#' res$name
#' # > "Value"
#' res$unit
#' # > ""
#' }
extractNameAndUnit <- function(text) {
  validateIsString(text)
  dimensionTask <- rSharp::callStatic("OSPSuite.R.Api", "GetDimensionTask")
  res <- dimensionTask$call("ExtractNameAndUnit", enc2utf8(text))
  return(list(name = res[1], unit = res[2]))
}

#' @title parseObservationsDataFrame
#' @description Function to read the variable names and units in the first two columns of an observations data frame used for qualification. First column stores timepoints.  Second column stores measurements of a quantity corresponding to timepoints in first column.
#' @param observationsDataFrame from CSV
#' @return A named list with `time` and `output` fields.  Each field contains a list that is output by `extractNameAndUnit` with fields that store the variable name and the unit.
#' @keywords internal
parseObservationsDataFrame <- function(observationsDataFrame) {
  namesObservationsDataFrame <- names(observationsDataFrame)
  validateIsIncluded(length(namesObservationsDataFrame), c(2, 3))
  dataFrameFields <- list(
    time = extractNameAndUnit(namesObservationsDataFrame[1]),
    output = extractNameAndUnit(namesObservationsDataFrame[2])
  )
  if (length(namesObservationsDataFrame) == 3) {
    dataFrameFields$error <- extractNameAndUnit(namesObservationsDataFrame[3])
  }
  return(dataFrameFields)
}



#' @title massMoleConversion
#' @description Function to map `Concentration (mass)` or `Mass` dimensions to `Concentration (molar)` and `Amount` respectively.
#' @param dimension is string that is from among the valid OSP dimensions
#' @return If `dimension` is `Concentration (mass)` or `Mass`, then return `Concentration (molar)` or `Amount` respectively, otherwise return `dimension`.
#' @keywords internal
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
#' @keywords internal
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


#' @title getPlotSettings
#' @description Read plot settings from configuration plan.
#' @param plotSettingsFromConfigurationPlot is a field from the `configurationPlan$plots` list
#' @return `plotSettings`, a list of settings for each of the X and Y axis.  Each list contains the unit, dimensions, and scaling type for each axes and option to plot grid lines.
#' @keywords internal
getPlotSettings <- function(plotSettingsFromConfigurationPlot) {
  plotSettings <- list(
    width = plotSettingsFromConfigurationPlot$ChartWidth,
    height = plotSettingsFromConfigurationPlot$ChartHeight,
    axisFontSize = plotSettingsFromConfigurationPlot$Fonts$AxisSize,
    legendFontSize = plotSettingsFromConfigurationPlot$Fonts$LegendSize,
    originFontSize = plotSettingsFromConfigurationPlot$Fonts$OriginSize,
    font = plotSettingsFromConfigurationPlot$Fonts$FontFamilyName,
    watermarkFontSize = plotSettingsFromConfigurationPlot$Fonts$WatermarkSize
  )
  return(plotSettings)
}



#' @title startQualificationRunner
#' @description Starts the qualification runner and creates inputs for the reporting engine
#' @param qualificationRunnerFolder Folder where QualificationRunner.exe is located
#' @param qualificationPlanFile full path of the input qualification plan
#' @param outputFolder Name of output folder created by the qualification runner
#' @param pkSimPortableFolder Folder where PK-Sim is located.
#' If not specified, installation path will be read from the registry (available only in case of full **non-portable** installation).
#' This option is **MANDATORY** for the portable version of PK-Sim.
#' @param configurationPlanName Name of the configuration plan to be generated. Default is `"report-configuration-plan"`
#' @param overwrite Logical defining if the contents of the output folder will be deleted, even if it not empty. Default is false.
#' @param logFile Full path of log file where log output will be written. A log file will not be created if this value is not provided.
#' @param logLevel Log verbosity (Debug, Information, Warning, Error). Default is Information.
#' @param displayVersion Logical defining if version information is displayed
#' @export
#' @family qualification workflow
startQualificationRunner <- function(qualificationRunnerFolder,
                                     qualificationPlanFile,
                                     outputFolder,
                                     pkSimPortableFolder = NULL,
                                     configurationPlanName = NULL,
                                     overwrite = TRUE,
                                     logFile = NULL,
                                     logLevel = NULL,
                                     displayVersion = FALSE) {
  validateIsFileExtension(qualificationPlanFile, "json")
  validateFileExists(qualificationPlanFile)
  validateIsLogical(overwrite)
  validateIsLogical(displayVersion)

  options <- c(
    ifNotNull(pkSimPortableFolder, paste0('-p "', pkSimPortableFolder, '"')),
    ifNotNull(configurationPlanName, paste0('-n "', configurationPlanName, '"')),
    switch(as.character(overwrite),
      "TRUE" = "-f",
      NULL
    ),
    ifNotNull(logFile, paste0('-l "', logFile, '"')),
    ifNotNull(logLevel, paste0("--logLevel ", logLevel)),
    switch(as.character(displayVersion),
      "TRUE" = "--version",
      NULL
    )
  )
  optionalArguments <- paste0(options, collapse = " ")
  qualificationRunner <- paste0('"', file.path(qualificationRunnerFolder, 'QualificationRunner.exe"'))

  arguments <- paste0(' -i "', qualificationPlanFile, '" -o "', outputFolder, '" ', "--norun ", optionalArguments)
  command <- paste0(qualificationRunner, arguments)
  status <- system(command)
  validateCommandStatus(command, status)
  return(invisible())
}
