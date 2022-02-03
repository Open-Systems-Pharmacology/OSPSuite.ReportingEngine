#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field simulationStructures `SimulationStructure` R6 class object managing the structure of the workflow output
#' @field workflowFolder path of the folder create by the Workflow
#' @field taskNames Enum of task names
#' @field reportFileName name of the Rmd report file
#' @field createWordReport logical of option for creating Markdwon-Report only but not a Word-Report.
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
    createWordReport = NULL,
    userDefinedTasks = NULL,
    numberSections = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the output folder created or used by the Workflow.
    #' @param createWordReport logical of option for creating Markdown-Report only but not a Word-Report.
    #' @param watermark displayed watermark in figures background
    #' @param simulationSetDescriptor character Descriptor of simulation sets indicated in reports
    #' @param numberSections logical defining if the report sections should be numbered
    #' @param theme A `Theme` object from `{tlf}` package
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                          workflowFolder,
                          createWordReport = TRUE,
                          watermark = NULL,
                          simulationSetDescriptor = NULL,
                          numberSections = TRUE,
                          theme = NULL) {
      private$.reportingEngineInfo <- ReportingEngineInfo$new()
      # Empty list on which users can load tasks
      self$userDefinedTasks <- list()

      validateIsString(workflowFolder)
      validateIsString(watermark, nullAllowed = TRUE)
      validateIsString(simulationSetDescriptor, nullAllowed = TRUE)
      sapply(c(simulationSets), function(simulationSet) {
        validateIsOfType(object = simulationSet, type = "SimulationSet")
      })
      validateIsLogical(createWordReport)
      validateIsLogical(numberSections)

      self$createWordReport <- createWordReport
      self$numberSections <- numberSections
      if (!isOfType(simulationSets, "list")) {
        simulationSets <- list(simulationSets)
      }

      allSimulationSetNames <- sapply(simulationSets, function(set) {
        set$simulationSetName
      })
      validateNoDuplicatedEntries(allSimulationSetNames)

      self$workflowFolder <- workflowFolder
      workflowFolderCheck <- file.exists(self$workflowFolder)

      if (workflowFolderCheck) {
        logWorkflow(
          message = workflowFolderCheck,
          pathFolder = self$workflowFolder,
          logTypes = c(LogTypes$Debug)
        )
      }
      dir.create(self$workflowFolder, showWarnings = FALSE, recursive = TRUE)

      logWorkflow(
        message = private$.reportingEngineInfo$print(),
        pathFolder = self$workflowFolder
      )

      self$reportFileName <- file.path(self$workflowFolder, paste0(defaultFileNames$reportName(), ".md"))
      self$taskNames <- enum(self$getAllTasks())

      self$simulationStructures <- list()
      simulationSets <- c(simulationSets)
      for (simulationSetIndex in seq_along(simulationSets)) {
        self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
          simulationSet = simulationSets[[simulationSetIndex]],
          workflowFolder = self$workflowFolder
        )
      }
      self$setSimulationDescriptor(simulationSetDescriptor %||% reEnv$defaultSimulationSetDescriptor)

      # Load default workflow theme, and sync the watermark
      setDefaultTheme(theme)
      self$setWatermark(watermark)
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
    #' @param tasks names of the worklfow tasks to activate.
    #' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    activateTasks = function(tasks = self$getAllTasks()) {
      activateWorkflowTasks(self, tasks = tasks)
    },

    #' @description
    #' Inactivates a series of `Tasks` from current `Workflow`
    #' @param tasks names of the worklfow tasks to inactivate.
    #' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
    #' @return Vector of inactive `Task` names
    inactivateTasks = function(tasks = self$getAllTasks()) {
      inactivateWorkflowTasks(self, tasks = tasks)
    },

    #' @description
    #' Print reporting engine information obtained from initiliazing a `Workflow`
    printReportingEngineInfo = function() {
      private$.reportingEngineInfo$print()
    },

    #' @description
    #' Get the current watermark to be reprted on figures background
    getWatermark = function() {
      private$.watermark
    },

    #' @description
    #' Set the watermark to be reported on figure background.
    #' The default value `NULL` leads to check if the computer has a validated environment.
    #' If the environment is validated, no watermark is reported on the background.
    #' If the environment is NOT validated, \code{workflowWatermarkMessage} is reported on the background.
    #' Any user-defined text will overwrite this default feature and be reported on the figure background.
    #' @param watermark text to be reported on figures background.
    #' Default value is `NULL`, which leads to check if the computer has a validated environment.
    #' If the environment is validated, no watermark is reported on the background.
    #' If the environment is NOT validated, \code{workflowWatermarkMessage} is reported on the background.
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
      setWatermarkConfiguration(private$.watermark)
    },

    #' @description Set mapping between parameters and their display paths in workflow
    #' to replace standard display of parameter paths.
    #' @param parameterDisplayPaths data.frame mapping Parameters with their display paths
    #' Variables of the data.frame should include `parameter` and `displayPath`.
    setParameterDisplayPaths = function(parameterDisplayPaths) {
      validateIsOfType(parameterDisplayPaths, "data.frame", nullAllowed = TRUE)
      if (!FisOfLength(parameterDisplayPaths, 0)) {
        validateIsIncluded(c("parameter", "displayPath"), names(parameterDisplayPaths))
      }
      # In case the same parameter is defined more than once, throw a warning
      if (!hasUniqueValues(parameterDisplayPaths$parameter)) {
        logWorkflow(
          message = messages$errorHasNoUniqueValues(parameterDisplayPaths$parameter, dataName = "parameter variable"),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Error
        )
      }

      # parameterDisplayPaths are centralized in the central private field .parameterDisplayPaths
      # However, they need to be send to the task using the field simulationStructures commmon among all tasks
      private$.parameterDisplayPaths <- parameterDisplayPaths
      for (structureSet in self$simulationStructures) {
        structureSet$parameterDisplayPaths <- parameterDisplayPaths
      }
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

  private = list(
    .reportingEngineInfo = NULL,
    .watermark = NULL,
    .parameterDisplayPaths = NULL,
    .simulationSetDescriptor = NULL,

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
