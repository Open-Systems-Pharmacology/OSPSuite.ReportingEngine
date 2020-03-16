#' @title MeanModelWorkflow
#' @description  R6 class for Reporting Engine Mean Model Workflow
#' @field meanModelSimulation R6 class `Task` for simulation
#' @field meanModelPKParameters R6 class `Task` for PK parameters calculation
#' @field meanModelSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotGoF R6 class `PlotTask` for goodness of fit plots
#' @field plotMassBalance R6 class `PlotTask` for mass balance plot
#' @field plotAbsorption R6 class `PlotTask` for absorption plot
#' @field plotPKParameters R6 class `PlotTask` for PK parameters plot
#' @field plotSensitivity R6 class `PlotTask` for sensitivity plot
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
MeanModelWorkflow <- R6::R6Class(
  "MeanModelWorkflow",
  inherit = Workflow,

  public = list(
    meanModelSimulation = NULL, # TO DO: rename with simpler task name simulate
    meanModelPKParameters = NULL, # TO DO: rename with simpler task name calculatePKParameters
    meanModelSensitivityAnalysis = NULL, # TO DO: rename with simpler task name calculateSensitivity
    plotGoF = NULL,
    plotMassBalance = NULL,
    plotAbsorption = NULL,
    plotPKParameters = NULL,
    plotSensitivity = NULL,

    #' @description
    #' Create a new `MeanModelWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @return A new `MeanModelWorkflow` object
    initialize = function(...) {
      super$initialize(...)

      # TO DO: include task parameters from initialization ?
      self$meanModelSimulationSettings()
      self$calculatePKParametersSettings()
      self$plotGoFSettings()
      self$plotMassBalanceSettings()
      self$plotAbsorptionSettings()
      self$plotPKParametersSettings()
      self$meanModelSensitivityAnalysisSettings()
      self$plotSensitivitySettings()
    },

    #' @description
    #' Define simulate `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @param inputFolderName folder of input
    #' @param simulationFileName name of simulation file
    #' @param resultsFolderName folder where output is saved
    #' @param resultsFileName file where output is saved
    #' @return A new `Task` object
    meanModelSimulationSettings = function(input = NULL,
                                               output = NULL,
                                               settings = NULL,
                                               active = TRUE,
                                               message = NULL) {
      self$meanModelSimulation <- SimulationTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Simulate mean model"
      )
    },

    #' @description
    #' Define calculate PK Parameters `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @param simulationFilePath TO DO
    #' @param simulationResultFilePaths TO DO
    #' @param pkParametersToEvaluate TO DO
    #' @param userDefinedPKFunctions TO DO
    #' @param pkParameterResultsFilePath TO DO
    #' @return A new `Task` object
    calculatePKParametersSettings = function(input = NULL,
                                                 output = NULL,
                                                 settings = NULL,
                                                 active = TRUE,
                                                 message = NULL) {
      self$meanModelPKParameters <- CalculatePKParametersTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Calculate mean model PK parameters"
      )
    },

    #' @description
    #' Define mean Model Sensitivity Analysis `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @param inputFolderName TO DO
    #' @param simulationFileName TO DO
    #' @param resultsFolderName TO DO
    #' @param resultsFileName TO DO
    #' @param variationRange TO DO
    #' @param numberOfCores TO DO
    #' @return A new `Task` object
    meanModelSensitivityAnalysisSettings = function(input = NULL,
                                                        output = NULL,
                                                        settings = NULL,
                                                        active = FALSE,
                                                        message = NULL,
                                                        variationRange = NULL,
                                                        numberOfCores = NULL) {
      self$meanModelSensitivityAnalysis <- SensitivityAnalysisTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for mean model",
        variationRange = variationRange,
        numberOfCores = numberOfCores
      )
    },

    #' @description
    #' Define Goodness of fit `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @return A `PlotTask` object for goodness of fit plots
    plotGoFSettings = function(reportTitle = NULL,
                                   taskFunction = plotMeanGoodnessOfFit,
                                   input = NULL,
                                   output = file.path(self$resultsFolder, "TimeProfile"),
                                   active = FALSE,
                                   message = NULL) {
      self$plotGoF <- PlotTask$new(
        reportTitle = reportTitle %||% defaultWorkflowTitles$plotGoF,
        getTaskResults = taskFunction,
        workflowFolder = self$workflowFolder,
        input = input,
        output = output,
        active = active,
        message = message %||% defaultWorkflowMessages$plotGoF
      )
    },

    #' @description
    #' Define PK parameters `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @return A `PlotPKParametersTask` object for PK parameters tables
    plotPKParametersSettings = function(reportTitle = NULL,
                                            taskFunction = plotMeanPKParameters,
                                            input = NULL,
                                            output = file.path(self$resultsFolder, "PKParameters"),
                                            active = FALSE,
                                            message = NULL) {
      self$plotPKParameters <- PlotPKParametersTask$new(
        reportTitle = reportTitle %||% defaultWorkflowTitles$plotPKParameters,
        getTaskResults = taskFunction,
        workflowFolder = self$workflowFolder,
        input = input,
        output = output,
        active = active,
        message = message %||% defaultWorkflowMessages$plotPKParameters
      )
    },

    #' @description
    #' Define Mass Balance `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @return A `PlotTask` object for mass balance plots
    plotMassBalanceSettings = function(reportTitle = NULL,
                                           taskFunction = plotMeanMassBalance,
                                           input = NULL,
                                           output = file.path(self$resultsFolder, "MassBalance"),
                                           active = FALSE,
                                           message = NULL) {
      self$plotMassBalance <- PlotTask$new(
        reportTitle = reportTitle %||% defaultWorkflowTitles$plotMassBalance,
        getTaskResults = taskFunction,
        workflowFolder = self$workflowFolder,
        input = input,
        output = output,
        active = active,
        message = message %||% defaultWorkflowMessages$plotMassBalance
      )
    },

    #' @description
    #' Define Absorption `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @return A `PlotTask` object for absorption plots
    plotAbsorptionSettings = function(reportTitle = NULL,
                                          taskFunction = plotMeanAbsorption,
                                          input = NULL,
                                          output = file.path(self$resultsFolder, "Absorption"),
                                          active = FALSE,
                                          message = NULL) {
      self$plotAbsorption <- PlotTask$new(
        reportTitle = reportTitle %||% defaultWorkflowTitles$plotAbsorption,
        getTaskResults = taskFunction,
        workflowFolder = self$workflowFolder,
        input = input,
        output = output,
        active = active,
        message = message %||% defaultWorkflowMessages$plotAbsorption
      )
    },

    #' @description
    #' Define sensitivity analysis `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @return A `PlotTask` object for sensitivity analysis plots
    plotSensitivitySettings = function(reportTitle = NULL,
                                           taskFunction = NULL,
                                           input = NULL,
                                           output = NULL,
                                           active = FALSE,
                                           message = NULL) {
      self$plotSensitivity <- PlotTask$new(
        reportTitle = reportTitle %||% defaultWorkflowTitles$plotSensitivity,
        getTaskResults = taskFunction,
        workflowFolder = self$workflowFolder,
        input = input,
        output = output,
        active = active,
        message = message %||% defaultWorkflowMessages$plotSensitivity
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
    #' @return All results and plots as a structured output in the workflow folder
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of mean model workflow",
        pathFolder = self$workflowFolder
      )

      initializeRmdFile(self$reportFileName,
        title = "Mean Model Workflow Report"
      )

      if (self$meanModelSimulation$active) {
        logWorkflow(
          message = paste0("Starting ", self$meanModelSimulation$message),
          pathFolder = self$workflowFolder
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = paste0("Run simulation: ", set$simulationSet$simulationName),
            pathFolder = self$workflowFolder
          )
          if (self$meanModelSimulation$validateInput()) {
            dir.create(set$simulationResultsFolder)
            # Create the Output of Simulation
            set$simulationResultFileNames <- simulateModel(
              simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
              resultsFilePath = file.path(set$simulationResultsFolder, defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName))
            )
          }
        }
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
            dir.create(set$pkAnalysisResultsFolder)
            # Create the Output for PK parameters
            set$pkAnalysisResultsFileNames <- calculatePKParameters(
              simulationFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
              simulationResultFilePaths = set$simulationResultFileNames,
              pkParameterResultsFilePath = file.path(set$pkAnalysisResultsFolder, defaultFileNames$pkAnalysisResultsFile(set$simulationSet$simulationSetName))
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
            dir.create(set$sensitivityAnalysisResultsFolder)
            # Create the Output of Simulation
            set$sensitivityAnalysisResultsFileNames <- runSensitivity(
              simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
              variableParameterPaths = self$meanModelSensitivityAnalysis$variableParameterPaths,
              variationRange = self$meanModelSensitivityAnalysis$variationRange,
              resultsFileFolder = set$sensitivityAnalysisResultsFolder,
              resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
              numberOfCores = self$meanModelSensitivityAnalysis$numberOfCores
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

      # Get output in final format for publication
      renderRmdFile(self$reportFileName)
    },

    #' @description
    #' Print workflow list of tasks
    #' TO DO: add simulationSets to print() method
    #' @return Task list information
    print = function() {
      taskOrder <- list(
        "Task 1" = self$meanModelSimulation$print(),
        "Task 2" = self$meanModelPKParameters$print(),
        "Task 3" = self$meanModelSensitivityAnalysis$print(),
        "Task 4" = self$plotGoF$print(),
        "Task 5" = self$plotMassBalance$print(),
        "Task 6" = self$plotAbsorption$print(),
        "Task 7" = self$plotPKParameters$print(),
        "Task 8" = self$plotSensitivity$print()
      )
      invisible(self)

      return(taskOrder)
    }
  )
)
