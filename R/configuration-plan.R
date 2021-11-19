#' @title ConfigurationPlan
#' @description R6 class for ConfigurationPlan guiding Qualification Workflow
#' @field plots list defining `PlotConfiguration` settings for specific tasks
#' @field inputs data.frame or list mapping input files to their paths
#' @field intro data.frame or list mapping introduction files to their paths
#' @field markdownIntro name of markdown file that includes intro content
#' @field referenceFolder Reference path for accessing inputs
#' @field workflowFolder path of the output folder created or used by the Workflow.
#' @export
ConfigurationPlan <- R6::R6Class(
  "ConfigurationPlan",
  cloneable = FALSE,

  public = list(
    plots = NULL,
    inputs = NULL,
    intro = NULL,
    markdownIntro = "000_Intro.md",
    referenceFolder = NULL,
    workflowFolder = NULL,

    #' @description Get location of directory corresponding to a specific section Id
    #' @param id section identifier
    #' @return The section path corresponding to the id in the configuration plan field `sections`
    getSectionPath = function(id) {
      validateIsIncluded(id, private$.sections$id, groupName = "'id' variable of sections")
      selectedId <- private$.sections$id %in% id
      return(file.path(self$workflowFolder, private$.sections$path[selectedId]))
    },

    #' @description Get markdown title to a specific section Id
    #' @param id section identifier
    #' @return The title associated with "#" corresponding to the subection level
    getSectionTitle = function(id) {
      validateIsIncluded(id, private$.sections$id, groupName = "'id' variable of sections")
      selectedId <- private$.sections$id %in% id
      sectionTitle <- paste0(rep("#", private$.sections$level[selectedId]), collapse = "")
      sectionTitle <- paste(sectionTitle, private$.sections$title[selectedId], sep = " ")
      return(sectionTitle)
    },

    #' @description Get location of .md file corresponding to a specific section Id
    #' @param id section identifier
    #' @return The markdown file corresponding to the id in the configuration plan field `sections`
    getSectionMarkdown = function(id) {
      validateIsIncluded(id, private$.sections$id, groupName = "'id' variable of sections")
      selectedId <- private$.sections$id %in% id
      return(file.path(self$workflowFolder, private$.sections$md[selectedId]))
    },

    #' @description Get location of .md intro file
    #' @return The markdown file corresponding to introduction
    getIntroMarkdown = function() {
      return(file.path(self$workflowFolder, self$markdownIntro))
    },

    #' @description Copy content to section markdown
    #' @param id section identifier
    #' @param logFolder path where logs are saved
    copySectionContent = function(id, logFolder = getwd()) {
      validateIsIncluded(id, private$.sections$id, groupName = "'id' variable of sections")
      selectedId <- private$.sections$id %in% id
      sectionContent <- private$.sections$content[selectedId]
      sectionTitle <- private$.sections$title[selectedId]
      sectionLevel <- private$.sections$level[selectedId]
      # If available, add title at appropriate level
      if(!is.na(sectionTitle)){
        markdownContent <- paste(
          paste0(rep("#", sectionLevel), collapse = ""),
          sectionTitle,
          sep = " "
        )
        addTextChunk(fileName = self$getSectionMarkdown(id), text = markdownContent, logFolder = logFolder)
      }
      if (is.na(sectionContent)) {
        return(invisible())
      }
      # Get location of content
      markdownLocation <- file.path(self$referenceFolder, sectionContent)
      # In case file does not exist
      if (!file.exists(markdownLocation)) {
        logWorkflow(
          message = paste0("Section content '", sectionContent, "' not found"),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Error, LogTypes$Debug)
        )
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Section Content
      markdownContent <- readLines(markdownLocation, encoding = "UTF-8")
      addTextChunk(fileName = self$getSectionMarkdown(id), text = markdownContent, logFolder = logFolder)
      return(invisible())
    },

    #' @description Copy input to section markdown
    #' @param input list including SectionId and Path
    #' @param logFolder path where logs are saved
    copyInput = function(input, logFolder = getwd()) {
      validateIsIncluded(names(input), c("SectionId", "Path"))
      # Get location of input
      inputLocation <- file.path(self$referenceFolder, input$Path)
      # In case file does not exist
      if (!file.exists(inputLocation)) {
        logWorkflow(
          message = paste0("Input '", input$Path, "' not found"),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Error, LogTypes$Debug)
        )
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Input
      markdownContent <- readLines(inputLocation, encoding = "UTF-8")
      addTextChunk(fileName = self$getSectionMarkdown(input$SectionId), text = markdownContent, logFolder = logFolder)
      return(invisible())
    },

    #' @description Copy intro to intro markdown
    #' @param intro list including Path
    #' @param logFolder path where logs are saved
    copyIntro = function(intro, logFolder = getwd()) {
      validateIsIncluded(names(intro), "Path")
      # Get location of intro
      introLocation <- file.path(self$referenceFolder, intro$Path)
      # In case file does not exist
      if (!file.exists(introLocation)) {
        logWorkflow(
          message = paste0("Input '", intro$Path, "' not found"),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Error, LogTypes$Debug)
        )
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Intro
      markdownContent <- readLines(introLocation, encoding = "UTF-8")
      addTextChunk(fileName = self$getIntroMarkdown(), text = markdownContent, logFolder = logFolder)
      return(invisible())
    },

    #' @description Get location of observed data corresponding to a specific observedDataSet Id
    #' @param id observedDataSet identifier
    #' @return The observed data file path corresponding to the id in the configuration plan field `observedDataSet`
    getObservedDataPath = function(id) {
      validateIsIncluded(id, private$.observedDataSets$id, groupName = "'id' variable of observedDataSets")
      selectedId <- private$.observedDataSets$id %in% id
      # In case of duplicate observed data, use first
      return(utils::head(file.path(self$referenceFolder, private$.observedDataSets$path[selectedId]), 1))
    },

    #' @description Get molecular weight of observed data corresponding to a specific observedDataSet Id
    #' @param id observedDataSet identifier
    #' @return The observed data file path corresponding to the id in the configuration plan field `observedDataSet`
    getMolWeightForObservedData = function(id) {
      validateIsIncluded(id, private$.observedDataSets$id, groupName = "'id' variable of observedDataSets")
      selectedId <- private$.observedDataSets$id %in% id
      # In case of duplicate observed data, use first
      return(utils::head(private$.observedDataSets$molWeight[selectedId], 1))
    },

    #' @description Get location of simulation file corresponding to a specific simulation and project names
    #' @param project name of simulation project
    #' @param simulation name of the simulation
    #' @return The simulation file path corresponding to the project and simulation in the configuration plan field `simulationMappings`
    getSimulationPath = function(project, simulation) {
      # Since data is imported from json, simulationMappings fields have a first upper case letter
      validateIsIncluded(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncluded(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")
      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      simulationPath <- file.path(self$referenceFolder, private$.simulationMappings$path[selectedId], paste0(private$.simulationMappings$simulationFile[selectedId], ".pkml"))
      return(simulationPath)
    },

    #' @description Get location of population file corresponding to a specific simulation and project names
    #' @param project name of simulation project
    #' @param simulation name of the simulation
    #' @return The population file path corresponding to the project and simulation in the configuration plan field `simulationMappings`
    getPopulationPath = function(project, simulation) {
      # Since data is imported from json, simulationMappings fields have a first upper case letter
      validateIsIncluded(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncluded(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")
      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      populationPath <- file.path(self$referenceFolder, private$.simulationMappings$path[selectedId], paste0(private$.simulationMappings$simulationFile[selectedId], "-Population.csv"))
      if(file.exists(populationPath)){
        return(populationPath)
      }
      return()
    },

    #' @description Get location of simulation result file corresponding to a specific simulation and project names
    #' @param project name of simulation project
    #' @param simulation name of the simulation
    #' @return The simulation results file path corresponding to the project and simulation in the configuration plan field `simulationMappings`
    getSimulationResultsPath = function(project, simulation) {
      # Since data is imported from json, simulationMappings fields have a first upper case letter
      validateIsIncluded(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncluded(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")

      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      simulationFile <- private$.simulationMappings$simulationFile[selectedId]
      simulationResultsPath <- file.path(self$workflowFolder, "SimulationResults", paste(project, simulationFile, "SimulationResults.csv", sep = "-"))
      return(simulationResultsPath)
    },

    #' @description Get location of PK Analysis result file corresponding to a specific simulation and project names
    #' @param project name of simulation project
    #' @param simulation name of the simulation
    #' @return The PKAnalysis results file path corresponding to the project and simulation in the configuration plan field `simulationMappings`
    getPKAnalysisResultsPath = function(project, simulation) {
      # Since data is imported from json, simulationMappings fields have a first upper case letter
      validateIsIncluded(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncluded(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")

      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      simulationFile <- private$.simulationMappings$simulationFile[selectedId]
      pkAnalysisResultsPath <- file.path(self$workflowFolder, "PKAnalysisResults", paste(project, simulationFile, "PKAnalysisResults.csv", sep = "-"))
      return(pkAnalysisResultsPath)
    },

    #' @description Update environment theme that will be used as default during workflow
    updateTheme = function() {
      setDefaultPlotFormat(
        format = self$plots$PlotSettings$ChartFormat,
        width = self$plots$PlotSettings$ChartWidth,
        height = self$plots$PlotSettings$ChartHeight,
        units = self$plots$PlotSettings$ChartUnits %||% "px"
      )

      reEnv$theme$fonts$legend$size <- self$plots$PlotSettings$Fonts$LegendSize %||% reEnv$theme$fonts$legend$size
      reEnv$theme$fonts$xAxis$size <- self$plots$PlotSettings$Fonts$AxisSize %||% reEnv$theme$xAxis$legend$size
      reEnv$theme$fonts$yAxis$size <- self$plots$PlotSettings$Fonts$AxisSize %||% reEnv$theme$yAxis$legend$size
      # TODO resizing of the input: normal size always appears bigger in background due to annotation_custom()
      reEnv$theme$fonts$watermark$size <- self$plots$PlotSettings$Fonts$WatermarkSize %||% reEnv$theme$fonts$watermark$size
      return(invisible())
    }
  ),

  active = list(
    #' @field sections data.frame mapping section ids to their paths
    sections = function(value) {
      if (missing(value)) {
        return(private$.sections)
      }
      private$.sections <- sectionsAsDataFrame(value)
      validateHasUniqueValues(private$.sections$id, dataName = "Sections Id")
    },

    #' @field simulationMappings data.frame mapping simulations to their paths
    simulationMappings = function(value) {
      if (missing(value)) {
        return(private$.simulationMappings)
      }
      # Change field names to appropriate camelCase names within the mapping
      private$.simulationMappings <- data.frame(
        project = sapply(value, function(mapping) {
          mapping$Project
        }),
        simulation = sapply(value, function(mapping) {
          mapping$Simulation
        }),
        path = sapply(value, function(mapping) {
          mapping$Path
        }),
        simulationFile = sapply(value, function(mapping) {
          mapping$SimulationFile
        }),
        stringsAsFactors = FALSE
      )
      simulationMappingsId <- paste0(
        "project: '", private$.simulationMappings$project,
        "' - simulation: '", private$.simulationMappings$simulation, "'"
      )
      validateHasUniqueValues(simulationMappingsId, dataName = "SimulationMappings combinations")
    },

    #' @field observedDataSets data.frame mapping observed datasets to their paths
    observedDataSets = function(value) {
      if (missing(value)) {
        return(private$.observedDataSets)
      }
      # ObservedDataSets can be null or empty list, in which case, nothing happens
      if (isOfLength(value, 0)) {
        return(invisible())
      }
      # Change field names to appropriate camelCase names within the mapping
      private$.observedDataSets <- data.frame(
        id = sapply(value, function(mapping) {
          mapping$Id
        }),
        path = sapply(value, function(mapping) {
          mapping$Path
        }),
        molWeight = sapply(value, function(mapping) {
          mapping$MolWeight %||% mapping$MW %||% NA
        }),
        type = sapply(value, function(mapping) {
          mapping$Type %||% NA
        }),
        stringsAsFactors = FALSE
      )
      # Checks if field observedDataSets is appropriate
      dataPaths <- private$.observedDataSets$path
      dataIds <- private$.observedDataSets$id
      unexistingFiles <- !file.exists(file.path(self$referenceFolder, dataPaths))
      if (any(unexistingFiles)) {
        unexistingFiles <- paste0(dataPaths[unexistingFiles], collapse = "', '")
        warning(paste0("Observed datasets '", unexistingFiles, "' not found"))
      }
      if (!hasUniqueValues(dataIds)) {
        # Check if the id reference same paths
        duplicatedIds <- unique(dataIds[duplicated(dataIds)])
        for (observedDataSetsId in duplicatedIds) {
          selectedRows <- dataIds %in% observedDataSetsId
          selectedPaths <- dataPaths[selectedRows]
          if (isOfLength(unique(selectedPaths), 1)) {
            warning(messages$errorHasNoUniqueValues(dataIds[selectedRows], dataName = "ObservedDataSets Id"))
            next
          }
          stop(paste0("Inconsistent ObservedDataSets Paths '", paste0(selectedPaths, collapse = "', '"), "' for non unique Id '", observedDataSetsId, "'"))
        }
      }
    }
  ),

  private = list(
    .sections = NULL,
    .simulationMappings = NULL,
    .observedDataSets = NULL
  )
)
