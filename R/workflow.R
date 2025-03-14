#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field simulationStructures `SimulationStructure` R6 class object managing the structure of the workflow output
#' @field workflowFolder path of the folder create by the Workflow
#' @field taskNames Enum of task names
#' @field reportFileName name of the Rmd report file
#' @field reportTitle report title page
#' @field createWordReport logical of option for creating Markdown-Report only but not a Word-Report.
#' @field wordConversionTemplate optional docx template for rendering a tuned Word-Report document
#' @field userDefinedTasks List of user-defined tasks (to update with loadUserDefinedTask)
#' @field numberSections logical defining if the report sections should be numbered
#' @import tlf
#' @importFrom ospsuite.utils %||%
#' @keywords internal
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    simulationStructures = NULL,
    workflowFolder = NULL,
    taskNames = NULL,
    reportFileName = NULL,
    reportTitle = NULL,
    createWordReport = NULL,
    wordConversionTemplate = NULL,
    userDefinedTasks = NULL,
    numberSections = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the output folder created or used by the Workflow.
    #' @param createWordReport logical of option for creating Markdown-Report only but not a Word-Report.
    #' @param wordConversionTemplate optional docx template for rendering a tuned Word-Report document
    #' @param watermark displayed watermark in figures background
    #' @param simulationSetDescriptor character Descriptor of simulation sets indicated in reports
    #' @param numberSections logical defining if the report sections should be numbered
    #' @param reportTitle report title internally added as a cover page
    #' If `reportTitle` is an existing file, it will be merged to the report as cover page.
    #' @param theme A `Theme` object from `{tlf}` package
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                          workflowFolder,
                          createWordReport = TRUE,
                          wordConversionTemplate = NULL,
                          watermark = NULL,
                          simulationSetDescriptor = NULL,
                          numberSections = TRUE,
                          reportTitle = NULL,
                          theme = NULL) {
      #----- Check and initialize workflow folder  -----
      validateIsString(workflowFolder)
      self$workflowFolder <- workflowFolder
      resetLogs(workflowFolder)
      checkExisitingPath(self$workflowFolder)
      dir.create(self$workflowFolder, showWarnings = FALSE, recursive = TRUE)

      private$.reportingEngineInfo <- ReportingEngineInfo$new()
      logInfo(private$.reportingEngineInfo$print())

      #----- Check and initialize workflow fields  -----
      logCatch({
        # Checks of input arguments
        if (!isOfType(simulationSets, "list")) {
          simulationSets <- list(simulationSets)
        }
        sapply(c(simulationSets), function(simulationSet) {
          validateIsOfType(object = simulationSet, type = "SimulationSet")
        })
        allSimulationSetNames <- sapply(simulationSets, function(set) {
          set$simulationSetName
        })
        validateNoDuplicate(
          values = allSimulationSetNames,
          variableName = "simulation set names",
          na.rm = FALSE
        )

        validateIsLogical(createWordReport)
        validateIsLogical(numberSections)
        validateIsString(watermark, nullAllowed = TRUE)
        validateIsString(simulationSetDescriptor, nullAllowed = TRUE)
        validateIsString(wordConversionTemplate, nullAllowed = TRUE)
        validateIsString(reportTitle, nullAllowed = TRUE)

        # Define workflow fields
        # Empty list on which users can load tasks
        self$userDefinedTasks <- list()

        self$simulationStructures <- list()
        simulationSets <- c(simulationSets)
        for (simulationSetIndex in seq_along(simulationSets)) {
          self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
            simulationSet = simulationSets[[simulationSetIndex]],
            workflowFolder = self$workflowFolder
          )
        }

        self$createWordReport <- createWordReport
        self$wordConversionTemplate <- wordConversionTemplate
        self$numberSections <- numberSections
        self$reportTitle <- reportTitle
        self$setSimulationDescriptor(simulationSetDescriptor %||% reEnv$defaultSimulationSetDescriptor)

        private$.reportFolder <- workflowFolder
        self$reportFileName <- paste0(defaultFileNames$reportName(), ".md")
        self$taskNames <- enum(self$getAllTasks())

        # Load default workflow theme, and sync the watermark
        setDefaultTheme(theme)
        self$setWatermark(watermark)
        setDefaultPlotFormat()
      })
      return(invisible())
    },

    #' @description
    #' Get a vector with all the names of the tasks within the `Workflow`
    #' @return Vector of `Task` names
    getAllTasks = function() {
      # get isTaskVector as a named vector
      isTaskVector <- unlist(eapply(self, function(x) {
        isOfType(x, "Task")
      }))

      taskNames <- setdiff(names(isTaskVector[as.logical(isTaskVector)]), "userDefinedTasks")

      return(taskNames)
    },

    #' @description
    #' Get a vector with all the names of the plot tasks within the `Workflow`
    #' @return Vector of `PlotTask` names
    getAllPlotTasks = function() {
      # get isTaskVector as a named vector
      isPlotTaskVector <- unlist(eapply(self, function(x) {
        isOfType(x, "PlotTask")
      }))

      taskNames <- setdiff(names(isPlotTaskVector[as.logical(isPlotTaskVector)]), "userDefinedTasks")

      return(taskNames)
    },

    #' @description
    #' Get a vector with all the names of active tasks within the `Workflow`
    #' @return Vector of active `Task` names
    getActiveTasks = function() {
      return(private$.getTasksWithStatus(status = TRUE))
    },

    #' @description
    #' Get a vector with all the names of inactive tasks within the `Workflow`
    #' @return Vector of inactive `Task` names
    getInactiveTasks = function() {
      return(private$.getTasksWithStatus(status = FALSE))
    },

    #' @description
    #' Activates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the workflow tasks to activate.
    #' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    activateTasks = function(tasks = self$getAllTasks()) {
      activateWorkflowTasks(self, tasks = tasks)
    },

    #' @description
    #' Inactivates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the workflow tasks to inactivate.
    #' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    inactivateTasks = function(tasks = self$getAllTasks()) {
      inactivateWorkflowTasks(self, tasks = tasks)
    },

    #' @description
    #' Print reporting engine information obtained from initializing a `Workflow`
    printReportingEngineInfo = function() {
      private$.reportingEngineInfo$print()
    },

    #' @description
    #' Get the current watermark to be reported on figures background
    getWatermark = function() {
      private$.watermark
    },

    #' @description
    #' Set the watermark to be reported on figure background.
    #' The default value `NULL` leads to check if the computer has a validated environment.
    #' If the environment is validated, no watermark is reported on the background.
    #' If the environment is NOT validated, `workflowWatermarkMessage` is reported on the background.
    #' Any user-defined text will overwrite this default feature and be reported on the figure background.
    #' @param watermark text to be reported on figures background.
    #' Default value is `NULL`, which leads to check if the computer has a validated environment.
    #' If the environment is validated, no watermark is reported on the background.
    #' If the environment is NOT validated, `workflowWatermarkMessage` is reported on the background.
    #' Any user-defined text will overwrite this default feature and be reported on the figure background.
    setWatermark = function(watermark) {
      validateIsString(watermark, nullAllowed = TRUE)
      # Define default feature based on validated system
      private$.watermark <- ""
      if (!private$.reportingEngineInfo$isValidated()) {
        private$.watermark <- reEnv$workflowWatermarkMessage
      }
      # Non-NULL user-defined watermark overwrite default feature
      private$.watermark <- watermark %||% private$.watermark
      tlf::setDefaultWatermark(private$.watermark)
      return(invisible())
    },

    #' @description Set mapping between parameters and their display paths in workflow
    #' to replace standard display of parameter paths.
    #' @param parameterDisplayPaths data.frame mapping Parameters with their display paths
    #' Variables of the data.frame should include `parameter` and `displayPath`.
    setParameterDisplayPaths = function(parameterDisplayPaths) {
      setLogFolder(self$workflowFolder)
      logCatch({
        validateIsOfType(parameterDisplayPaths, "data.frame", nullAllowed = TRUE)
        if (!isEmpty(parameterDisplayPaths)) {
          validateIsIncluded(c("parameter", "displayPath"), names(parameterDisplayPaths))
        }
        # In case the same parameter is defined more than once, throw an error
        validateNoDuplicate(values = parameterDisplayPaths$parameter, variableName = "variable 'parameter'")
        # parameterDisplayPaths are centralized in the central private field .parameterDisplayPaths
        # However, they need to be send to the task using the field simulationStructures commmon among all tasks
        private$.parameterDisplayPaths <- parameterDisplayPaths
        for (structureSet in self$simulationStructures) {
          structureSet$parameterDisplayPaths <- parameterDisplayPaths
        }
      })
      return(invisible())
    },

    #' @description Get mapping between parameters and their display paths in workflow
    #' to replace standard display of parameter paths.
    #' @return A data.frame with `parameter` and `displayPath` variables.
    getParameterDisplayPaths = function() {
      return(private$.parameterDisplayPaths)
    },

    #' @description Set descriptor of simulation sets indicated in reports
    #' @param text character describing what simulation sets refer to
    setSimulationDescriptor = function(text) {
      validateIsString(text, nullAllowed = TRUE)

      # simulationSetDescriptor needs to be send to the tasks using the field simulationStructures commmon among all tasks
      private$.simulationSetDescriptor <- text %||% ""
      for (structureSet in self$simulationStructures) {
        structureSet$simulationSetDescriptor <- text %||% ""
      }
    },

    #' @description Get descriptor of simulation sets indicated in reports
    #' @return character Descriptor of simulation sets
    getSimulationDescriptor = function() {
      return(private$.simulationSetDescriptor)
    },

    #' @description
    #' Print workflow list of tasks
    #' @return Task list information
    print = function() {
      tasksInfo <- list()
      for (task in self$getAllTasks()) {
        tasksInfo[[paste0("Task: '", task, "'")]] <- self[[task]]$print()
      }

      invisible(self)
      return(tasksInfo)
    }
  ),
  active = list(
    #' @field reportFolder Directory in which workflow report is saved
    reportFolder = function(value) {
      if (missing(value)) {
        return(private$.reportFolder)
      }
      validateIsCharacter(value)
      dir.create(value, showWarnings = FALSE, recursive = TRUE)
      private$.reportFolder <- value
      return(invisible())
    },

    #' @field reportFilePath Path of workflow report
    reportFilePath = function(value) {
      if (missing(value)) {
        return(file.path(self$reportFolder, self$reportFileName))
      }
      validateIsFileExtension(value, "md")
      self$reportFolder <- dirname(value)
      self$reportFileName <- basename(value)
      return(invisible())
    }
  ),
  private = list(
    .reportingEngineInfo = NULL,
    .watermark = NULL,
    .parameterDisplayPaths = NULL,
    .simulationSetDescriptor = NULL,
    .reportFolder = NULL,
    .getTasksWithStatus = function(status) {
      taskNames <- self$getAllTasks()

      tasksWithStatus <- NULL
      for (taskName in taskNames) {
        if (self[[taskName]]$active == status) {
          tasksWithStatus <- c(tasksWithStatus, taskName)
        }
      }
      return(tasksWithStatus)
    }
  )
)
