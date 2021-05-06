#' @title loadQualificationWorkflow
#' @description Load a `ConfigurationPlan` object from json file
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @param jsonFile name of the JSON snapshot file including the configuration plan
#' @return A `QualificationWorkflow` object
#' @export
loadQualificationWorkflow <- function(workflowFolder, jsonFile) {
  configurationPlan <- loadConfigurationPlan(
    workflowFolder = workflowFolder,
    jsonFile = jsonFile
  )
  # TODO outputs and pk parameters may need to be defined
  # Either as part of simulationSets or as a settings for the task
  # Currently, no output is saved as simulationResults
  simulationSets <- list()
  for (simulationIndex in 1:nrow(configurationPlan$simulationMappings)) {
    project <- configurationPlan$simulationMappings$project[simulationIndex]
    simulationName <- configurationPlan$simulationMappings$simulation[simulationIndex]
    simulationFile <- configurationPlan$getSimulationPath(project = project, simulation = simulationName)
    
    simulation <- loadSimulation(simulationFile) 
    outputs <- lapply(simulation$outputSelections$allOutputs, function(output){Output$new(output$path)})

    # simulationSetName defined as project-simulation uniquely identifies the simulation
    simulationSets[[simulationIndex]] <- SimulationSet$new(
      simulationSetName = paste(project, simulationName, sep = "-"),
      simulationFile = simulationFile,
      outputs = c(outputs)
    )
  }
  workflow <- QualificationWorkflow$new(simulationSets = simulationSets, workflowFolder = workflowFolder)
  workflow$configurationPlan <- configurationPlan
  return(workflow)
}

#' @title loadConfigurationPlan
#' @description Load a `ConfigurationPlan` object from json file
#' @param jsonFile name of the JSON snapshot file including the configuration plan
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @return A `ConfigurationPlan` object including the content of json file
#' @export
loadConfigurationPlan <- function(jsonFile, workflowFolder) {
  jsonConfigurationPlan <- jsonlite::fromJSON(jsonFile, simplifyDataFrame = FALSE)

  # Check if mandatory variables were input
  # Matlab version had as well ObservedDataSets and Inputs, but they don't need to be mandatory in R
  validateIsIncluded(c("SimulationMappings", "Plots", "Sections"), names(jsonConfigurationPlan))

  # Create `ConfigurationPlan` object
  configurationPlan <- ConfigurationPlan$new()
  # The workflow and reference folders are required to know from where accessing the files
  configurationPlan$workflowFolder <- workflowFolder
  configurationPlan$referenceFolder <- normalizePath(path = dirname(jsonFile), winslash = .Platform$file.sep)

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
        sectionLevel = sectionLevel +1
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
createSectionOutput <- function(configurationPlan, logFolder = getwd()){
  # Intro
  resetReport(configurationPlan$getIntroMarkdown())
  appendices <- configurationPlan$getIntroMarkdown()
  for(intro in configurationPlan$intro){
    configurationPlan$copyIntro(intro, logFolder = logFolder)
  }
  # Sections
  for(sectionIndex in configurationPlan$sections$index){
    sectionId <- configurationPlan$sections$id[sectionIndex]
    # Create section output path
    dir.create(configurationPlan$getSectionPath(sectionId), showWarnings = FALSE, recursive = TRUE)
    # Initialize markdown appendices
    resetReport(configurationPlan$getSectionMarkdown(sectionId), logFolder = logFolder)
    appendices <- c(appendices, configurationPlan$getSectionMarkdown(sectionId))
    # Possibility to add title to section content if not defined in sections
    #addTextChunk(configurationPlan$getSectionTitle(sectionId),
    #             fileName = configurationPlan$getSectionMarkdown(sectionId),
    #             logFolder = logFolder)
    
    # Add info from content to section content
    configurationPlan$copySectionContent(sectionId, logFolder = logFolder)
  }
  # Inputs
  for(input in configurationPlan$inputs){
    configurationPlan$copyInput(input, logFolder = logFolder)
  }
  return(appendices)
}