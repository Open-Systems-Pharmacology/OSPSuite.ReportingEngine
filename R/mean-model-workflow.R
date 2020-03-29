#' @title MeanModelWorkflow
#' @description  R6 class for Reporting Engine Mean Model Workflow
#' @field simulate R6 class `Task` for simulation
#' @field meanModelPKParameters R6 class `Task` for PK parameters calculation
#' @field meanModelSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotGoF R6 class `PlotTask` for goodness of fit plots
#' @field plotMassBalance R6 class `PlotTask` for mass balance plot
#' @field plotAbsorption R6 class `PlotTask` for absorption plot
#' @field plotPKParameters R6 class `PlotTask` for PK parameters plot
#' @field plotSensitivity R6 class `PlotTask` for sensitivity plot
#' @field renderReport R6 class `Task` for saving report in a specific format
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
MeanModelWorkflow <- R6::R6Class(
  "MeanModelWorkflow",
  inherit = Workflow,

  public = list(
    simulate = NULL, # TO DO: rename with simpler task name simulate
    meanModelPKParameters = NULL, # TO DO: rename with simpler task name calculatePKParameters
    meanModelSensitivityAnalysis = NULL, # TO DO: rename with simpler task name calculateSensitivity
    plotGoF = NULL,
    plotMassBalance = NULL,
    plotAbsorption = NULL,
    plotPKParameters = NULL,
    plotSensitivity = NULL,
    renderReport = NULL,

    #' @description
    #' Create a new `MeanModelWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @return A new `MeanModelWorkflow` object
    initialize = function(...) {
      super$initialize(...)

      # TO DO: include task parameters from initialization ?
      self$simulateSettings()
      self$calculatePKParametersSettings()
      self$plotGoFSettings()
      self$plotMassBalanceSettings()
      self$plotAbsorptionSettings()
      self$plotPKParametersSettings()
      self$meanModelSensitivityAnalysisSettings()
      self$plotSensitivitySettings()
      self$renderReportSettings()
    },

    #' @description
    #' Define simulate `SimulationTask` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `TRUE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `SimulationTask` object
    simulateSettings = function(taskFunction = simulateModel,
                                outputFolder = defaultTaskOutputFolders$simulate,
                                active = TRUE,
                                message = defaultWorkflowMessages$simulate,
                                settings = NULL) {
      self$simulate <- SimulationTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        settings = settings,
        message = message
      )
    },

    #' @description
    #' Define calculate PK Parameters `CalculatePKParametersTask` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `SimulationTask` object
    calculatePKParametersSettings = function(taskFunction = NULL,
                                             outputFolder = defaultTaskOutputFolders$calculatePKParameters,
                                             active = FALSE,
                                             message = defaultWorkflowMessages$calculatePKParameters,
                                             settings = NULL) {
      self$meanModelPKParameters <- CalculatePKParametersTask$new(
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        settings = settings,
        message = message
      )
    },

    #' @description
    #' Define mean Model Sensitivity Analysis `SensitivityAnalysisTask` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `SimulationTask` object
    meanModelSensitivityAnalysisSettings = function(taskFunction = NULL,
                                                    outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
                                                    active = FALSE,
                                                    message = defaultWorkflowMessages$sensitivityAnalysis,
                                                    settings = NULL) {
      self$meanModelSensitivityAnalysis <- SensitivityAnalysisTask$new(
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        settings = settings,
        message = message)
    },

    #' @description
    #' Define Goodness of fit `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotGoFSettings = function(reportTitle = defaultWorkflowTitles$plotGoF,
                               taskFunction = plotMeanGoodnessOfFit,
                               outputFolder = defaultTaskOutputFolders$plotGoF,
                               active = FALSE,
                               message = defaultWorkflowMessages$plotGoF,
                               settings = NULL) {
      self$plotGoF <- PlotTask$new(
        reportTitle = reportTitle,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define PK parameters `PlotPKParametersTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotPKParametersTask` object for PK parameters tables
    plotPKParametersSettings = function(reportTitle = defaultWorkflowTitles$plotPKParameters,
                                        taskFunction = plotMeanPKParameters,
                                        outputFolder = defaultTaskOutputFolders$plotPKParameters,
                                        active = FALSE,
                                        message = defaultWorkflowMessages$plotPKParameters,
                                        settings = NULL) {
      self$plotPKParameters <- PlotTask$new(
        reportTitle = reportTitle,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define Mass Balance `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotMassBalanceSettings = function(reportTitle = defaultWorkflowTitles$plotMassBalance,
                                       taskFunction = plotMeanMassBalance,
                                       outputFolder = defaultTaskOutputFolders$plotMassBalance,
                                       active = FALSE,
                                       message = defaultWorkflowMessages$plotMassBalance,
                                       settings = NULL) {
      self$plotMassBalance <- PlotTask$new(
        reportTitle = reportTitle,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define Absorption `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotAbsorptionSettings = function(reportTitle = defaultWorkflowTitles$plotAbsorption,
                                      taskFunction = plotMeanAbsorption,
                                      outputFolder = defaultTaskOutputFolders$plotAbsorption,
                                      active = FALSE,
                                      message = defaultWorkflowMessages$plotAbsorption,
                                      settings = NULL) {
      self$plotAbsorption <- PlotTask$new(
        reportTitle = reportTitle,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define sensitivity analysis `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotSensitivitySettings = function(reportTitle = defaultWorkflowTitles$plotSensitivity,
                                       taskFunction = NULL,
                                       outputFolder = defaultTaskOutputFolders$plotSensitivity,
                                       active = FALSE,
                                       message = defaultWorkflowMessages$plotSensitivity,
                                       settings = NULL) {
      self$plotSensitivity <- PlotTask$new(
        reportTitle = reportTitle,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define render report `ReportTask` settings
    #' @param active logical indicating if `Task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    renderReportSettings = function(active = FALSE,
                                    message = defaultWorkflowMessages$renderReport,
                                    settings = NULL) {
      self$renderReport <- ReportTask$new(
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Run mean model workflow tasks for all simulation sets if tasks are activated
    #' The order of tasks is as follows:
    #' # 1) Run simulations
    #' # 2) Perform PK and sensitivity analyses
    #' # 3) Perform plot tasks
    #' ## 3.a) time profiles and residual plots
    #' ## 3.b) absorption plots
    #' ## 3.c) mass balance plots
    #' ## 3.d) PK and sensitivity analyses tables and plots
    #' # 4) Render report
    #' @return All results and plots as a structured output in the workflow folder
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of mean model workflow",
        pathFolder = self$workflowFolder
      )

      initializeRmdFile(self$reportFileName,
        title = "Mean Model Workflow Report"
      )

      if (self$simulate$active) {
        self$simulate$runTask(self$simulationStructures)
      }

      if (self$meanModelPKParameters$active) {
        logWorkflow(
          message = paste0("Starting ", self$meanModelPKParameters$message),
          pathFolder = self$workflowFolder
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Calculate PK parameters: ", paste0(set$simulationSet$pkParameters, collapse = ", "))
            ),
            pathFolder = self$workflowFolder
          )
          if (self$meanModelPKParameters$validateInput()) {
            if (!is.null(file.path(self$meanModelPKParameters$workflowFolder, self$meanModelPKParameters$outputFolder))) {
              dir.create(file.path(self$meanModelPKParameters$workflowFolder, self$meanModelPKParameters$outputFolder))
            }
            # Create the Output for PK parameters
            pkAnalyses <- calculatePKParameters(set)
            exportPKAnalysesToCSV(
              pkAnalyses = pkAnalyses,
              filePath = set$pkAnalysisResultsFileNames
            )
          }
        }
      }

      if (self$meanModelSensitivityAnalysis$active) {
        logWorkflow(
          message = paste0("Starting ", self$meanModelSensitivityAnalysis$message),
          pathFolder = self$workflowFolder
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Calculate sensitivity for the PK parameters: ", paste0(set$simulationSet$pkParameters, collapse = ", "))
            ),
            pathFolder = self$workflowFolder
          )
          if (self$meanModelSensitivityAnalysis$validateInput()) {

            if (!is.null(file.path(self$meanModelSensitivityAnalysis$workflowFolder, self$meanModelSensitivityAnalysis$outputFolder))) {
              dir.create(file.path(self$meanModelSensitivityAnalysis$workflowFolder, self$meanModelSensitivityAnalysis$outputFolder))
            }

            set$sensitivityAnalysisResultsFileNames <- runSensitivity(
              simFilePath = set$simulationSet$simulationFile,
              resultsFileFolder = file.path(self$meanModelSensitivityAnalysis$workflowFolder, self$meanModelSensitivityAnalysis$outputFolder),
              resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
              variableParameterPaths = self$meanModelSensitivityAnalysis$settings$variableParameterPaths,
              variationRange = self$meanModelSensitivityAnalysis$settings$variationRange,
              numberOfCores = self$meanModelSensitivityAnalysis$settings$numberOfCores
            )
          }
        }
      }

      for (plotTask in self$getAllPlotTasks()) {
        if (self[[plotTask]]$active) {
          self[[plotTask]]$runTask(
            self$simulationStructures,
            self$reportFileName
          )
        }
      }
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
  )
)
