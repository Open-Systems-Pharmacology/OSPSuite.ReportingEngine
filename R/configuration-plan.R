#' @title ConfigurationPlan
#' @description R6 class for ConfigurationPlan guiding Qualification Workflow
#' @field plots list defining `PlotConfiguration` settings for specific tasks
#' @field inputs data.frame or list mapping input files to their paths
#' @field intro data.frame or list mapping introduction files to their paths
#' @field markdownIntro name of markdown file that includes intro content
#' @field referenceFolder Reference path for accessing inputs
#' @field workflowFolder path of the output folder created or used by the Workflow.
#' @export
#' @import ospsuite.utils
#' @family qualification workflow
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
      selectedId <- private$.selectSectionsRow(id)
      return(file.path(self$workflowFolder, private$.sections$path[selectedId]))
    },

    #' @description Get markdown title to a specific section Id
    #' @param id section identifier
    #' @return The title associated with "#" corresponding to the subection level
    getSectionTitle = function(id) {
      selectedId <- private$.selectSectionsRow(id)
      sectionTitle <- paste0(rep("#", private$.sections$level[selectedId]), collapse = "")
      sectionTitle <- paste(sectionTitle, private$.sections$title[selectedId], sep = " ")
      return(sectionTitle)
    },

    #' @description Get location of .md file corresponding to a specific section Id
    #' @param id section identifier
    #' @return The markdown file corresponding to the id in the configuration plan field `sections`
    getSectionMarkdown = function(id) {
      selectedId <- private$.selectSectionsRow(id)
      return(file.path(self$workflowFolder, private$.sections$md[selectedId]))
    },

    #' @description Get section level
    #' @param id section identifier
    #' @return Section level
    getSectionLevel = function(id) {
      selectedId <- private$.selectSectionsRow(id)
      return(private$.sections$level[selectedId])
    },

    #' @description Get location of .md intro file
    #' @return The markdown file corresponding to introduction
    getIntroMarkdown = function() {
      return(file.path(self$workflowFolder, self$markdownIntro))
    },

    #' @description Copy content to section markdown
    #' @param id section identifier
    copySectionContent = function(id) {
      selectedId <- private$.selectSectionsRow(id)

      # Add reference as anchor tag before title and content
      sectionReference <- private$.sections$id[selectedId]
      addTextChunk(fileName = self$getSectionMarkdown(id), text = anchor(sectionReference))

      # Add title at appropriate level
      addTextChunk(fileName = self$getSectionMarkdown(id), text = self$getSectionTitle(id))

      # Add section content
      sectionContent <- private$.sections$content[selectedId]
      if (is.na(sectionContent)) {
        return(invisible())
      }
      # Get location of content
      markdownLocation <- file.path(self$referenceFolder, sectionContent)
      # In case file does not exist
      if (!file.exists(markdownLocation)) {
        logError(paste0("Section content '", .highlight(sectionContent), "' not found"))
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Section Content
      markdownContent <- readLines(markdownLocation, encoding = "UTF-8", warn = FALSE)
      addTextChunk(fileName = self$getSectionMarkdown(id), text = markdownContent)
      return(invisible())
    },

    #' @description Copy input to section markdown
    #' @param input list including SectionId and Path
    copyInput = function(input) {
      validateIsIncluded("Path", names(input))
      # Get location of input
      inputLocation <- file.path(self$referenceFolder, input$Path)
      # In case file does not exist
      if (!file.exists(inputLocation)) {
        logError(paste0("Input '", .highlight(input$Path), "' not found"))
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Input
      markdownContent <- readLines(inputLocation, encoding = "UTF-8", warn = FALSE)
      addTextChunk(fileName = self$getSectionMarkdown(input$SectionReference %||% input$SectionId), text = markdownContent)
      return(invisible())
    },

    #' @description Copy intro to intro markdown
    #' @param intro list including Path
    copyIntro = function(intro) {
      validateIsIncluded(names(intro), "Path")
      # Get location of intro
      introLocation <- file.path(self$referenceFolder, intro$Path)
      # In case file does not exist
      if (!file.exists(introLocation)) {
        logError(paste0("Input '", .highlight(intro$Path), "' not found"))
        return(invisible())
      }
      # UTF-8 encoding is assumed for md files in Intro
      markdownContent <- readLines(introLocation, encoding = "UTF-8", warn = FALSE)
      addTextChunk(fileName = self$getIntroMarkdown(), text = markdownContent)
      return(invisible())
    },

    #' @description If available, copy all files and folders of `Content` directory into `<workflowFolder>`
    copyContentFiles = function() {
      srcContentFolder <- file.path(self$referenceFolder, "Content")
      # If no Content available, does not need to copy anything
      if (!dir.exists(srcContentFolder)) {
        return(invisible())
      }
      # Get list of folders within content as well as all the subfolders using recursive option
      contentSubFolders <- list.dirs(path = srcContentFolder, full.names = FALSE, recursive = TRUE)
      # Get list of files within content as well as all files within subfolders using recursive option
      contentFiles <- list.files(path = srcContentFolder, full.names = FALSE, recursive = TRUE)

      # Create all necessary subfolders within workflow folder
      # Note that recursive is not needed here since list of folders used it
      for (contentSubFolder in contentSubFolders) {
        dir.create(file.path(self$workflowFolder, contentSubFolder), showWarnings = FALSE)
      }
      # Copy all available files of Content within workflow folder
      # Note that recursive is not needed here since list of files used it
      for (contentFile in contentFiles) {
        file.copy(
          from = file.path(srcContentFolder, contentFile),
          to = file.path(self$workflowFolder, contentFile)
        )
      }
      return(invisible())
    },

    #' @description Get location of observed data corresponding to a specific observedDataSet Id
    #' @param id observedDataSet identifier
    #' @return The observed data file path corresponding to the id in the configuration plan field `observedDataSet`
    getObservedDataPath = function(id) {
      validateIsIncludedAndLog(id, private$.observedDataSets$id, groupName = "'id' variable of observedDataSets")
      selectedId <- private$.observedDataSets$id %in% id
      # In case of duplicate observed data, use first
      return(utils::head(file.path(self$referenceFolder, private$.observedDataSets$path[selectedId]), 1))
    },

    #' @description Get molecular weight of observed data corresponding to a specific observedDataSet Id
    #' @param id observedDataSet identifier
    #' @return The observed data file path corresponding to the id in the configuration plan field `observedDataSet`
    getMolWeightForObservedData = function(id) {
      validateIsIncludedAndLog(id, private$.observedDataSets$id, groupName = "'id' variable of observedDataSets")
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
      validateIsIncludedAndLog(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncludedAndLog(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")
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
      validateIsIncludedAndLog(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncludedAndLog(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")
      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      populationPath <- file.path(self$referenceFolder, private$.simulationMappings$path[selectedId], paste0(private$.simulationMappings$simulationFile[selectedId], "-Population.csv"))
      if (file.exists(populationPath)) {
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
      validateIsIncludedAndLog(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncludedAndLog(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")

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
      validateIsIncludedAndLog(project, private$.simulationMappings$project, groupName = "'project' variable of simulationMappings")
      validateIsIncludedAndLog(simulation, private$.simulationMappings$simulation, groupName = "'simulation' variable of simulationMappings")

      selectedId <- (private$.simulationMappings$project %in% project) & (private$.simulationMappings$simulation %in% simulation)
      simulationFile <- private$.simulationMappings$simulationFile[selectedId]
      pkAnalysisResultsPath <- file.path(self$workflowFolder, "PKAnalysisResults", paste(project, simulationFile, "PKAnalysisResults.csv", sep = "-"))
      return(pkAnalysisResultsPath)
    },

    #' @description Update environment theme that will be used as default during workflow
    updateTheme = function() {
      plotFormat <- self$plots$PlotSettings$ChartFormat
      plotWidth <- self$plots$PlotSettings$ChartWidth
      plotHeight <- self$plots$PlotSettings$ChartHeight
      plotSizeUnits <- self$plots$PlotSettings$ChartUnits %||% "px"
      if (is.null(c(plotWidth, plotHeight))) {
        plotSizeUnits <- NULL
      }
      setDefaultPlotFormat(
        format = plotFormat,
        width = plotWidth,
        height = plotHeight,
        units = plotSizeUnits
      )

      reEnv$theme$fonts$legend$size <- self$plots$PlotSettings$Fonts$LegendSize %||% reEnv$theme$fonts$legend$size
      reEnv$theme$fonts$xAxis$size <- self$plots$PlotSettings$Fonts$AxisSize %||% reEnv$theme$fonts$xAxis$size
      reEnv$theme$fonts$yAxis$size <- self$plots$PlotSettings$Fonts$AxisSize %||% reEnv$theme$fonts$yAxis$size
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
      validatehasOnlyDistinctValues(private$.sections$id, dataName = "Sections Id")
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
      validatehasOnlyDistinctValues(simulationMappingsId, dataName = "SimulationMappings combinations")
    },

    #' @field observedDataSets data.frame mapping observed datasets to their paths
    observedDataSets = function(value) {
      if (missing(value)) {
        return(private$.observedDataSets)
      }
      # ObservedDataSets can be null or empty list, in which case, nothing happens
      if (isEmpty(value)) {
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
        logError(paste0("Observed datasets '", .highlight(unexistingFiles), "' not found"))
      }
      if (!hasOnlyDistinctValues(dataIds)) {
        # Check if the id reference same paths
        duplicatedIds <- unique(dataIds[duplicated(dataIds)])
        for (observedDataSetsId in duplicatedIds) {
          selectedRows <- dataIds %in% observedDataSetsId
          selectedPaths <- unique(dataPaths[selectedRows])
          tryCatch(
            {
              validateIsOfLength(selectedPaths, 1)
            },
            error = function(e) {
              .logErrorThenStop(paste0(
                "Inconsistent ObservedDataSets Paths '",
                paste0(selectedPaths, collapse = "', '"),
                "' for non unique Id '", observedDataSetsId, "'"
              ))
            }
          )
        }
      }
    }
  ),
  private = list(
    .sections = NULL,
    .simulationMappings = NULL,
    .observedDataSets = NULL,
    .selectSectionsRow = function(id) {
      validateIsIncludedAndLog(id, private$.sections$id, groupName = "'id' variable of sections")
      return(which(private$.sections$id %in% id))
    }
  )
)
