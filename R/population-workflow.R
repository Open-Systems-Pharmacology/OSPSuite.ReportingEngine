#' @title PopulationWorkflow
#' @description R6 class for Reporting Engine Population Workflow
#' @field simulatePopulation R6 class `Task` for population simulation
#' @field populationPKParameters R6 class `Task` for PK parameters calculation
#' @field populationSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotDemography R6 class `Task` for demography plots
#' @field plotGoF R6 class `Task` for goodness of fit plots
#' @field plotPKParameters R6 class `Task` for PK parameters plot
#' @field plotSensitivity R6 class `Task` for sensitivity plot
#' @field resetReport R6 class `Task` for saving report in a specific format
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
PopulationWorkflow <- R6::R6Class(
  "PopulationWorkflow",
  inherit = Workflow,

  public = list(
    simulatePopulation = NULL, # TO DO: rename with simpler task name simulate
    populationPKParameters = NULL, # TO DO: rename with simpler task name calculatePKParameters
    populationSensitivityAnalysis = NULL,
    plotDemography = NULL,
    plotGoF = NULL,
    plotPKParameters = NULL,
    plotSensitivity = NULL,
    resetReport = NULL,

    #' @description
    #' Create a new `PopulationWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @return A new `PopulationWorkflow` object
    initialize = function(...) {
      super$initialize(...)

      # TO DO: include task parameters from initialization ?
      self$resetReportSettings()
      self$simulatePopulationSettings()
      self$populationPKParameterSettings()
      self$populationSensitivityAnalysisSettings()
      self$plotDemographySettings()
      self$plotGoFSettings()
      self$plotPKParametersSettings()
      self$plotSensitivitySettings()
    },

    #' @description
    #' Define simulate `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `TRUE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `Task` object
    simulatePopulationSettings = function(taskFunction = simulateModelForPopulation,
                                          outputFolder = defaultTaskOutputFolders$simulate,
                                          settings = NULL,
                                          active = TRUE,
                                          message = defaultWorkflowMessages$simulate) {
      self$simulatePopulation <- SimulationTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        settings = settings,
        message = message
      )
    },

    #' @description
    #' Define calculate PK parameters `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `Task` object
    populationPKParameterSettings = function(taskFunction = calculatePKParameters,
                                             outputFolder = defaultTaskOutputFolders$calculatePKParameters,
                                             settings = NULL,
                                             active = TRUE,
                                             message = defaultWorkflowMessages$calculatePKParameters) {
      self$populationPKParameters <- CalculatePKParametersTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Calculate PK parameters for population"
      )
    },

    #' @description
    #' Define population Sensitivity Analysis `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `SensitivityAnalysisTask` object
    populationSensitivityAnalysisSettings = function(taskFunction = runPopulationSensitivityAnalysis,
                                                     outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
                                                     settings = NULL,
                                                     active = TRUE,
                                                     message = NULL) {
      self$populationSensitivityAnalysis <- PopulationSensitivityAnalysisTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for population"
      )
    },

    # TO DO: Define the tasks settings for plots
    #' @description
    #' Define plot demography `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotDemographySettings = function(reportTitle = defaultWorkflowTitles$plotDemography,
                                      taskFunction = NULL,
                                      outputFolder = defaultTaskOutputFolders$plotDemography,
                                      active = FALSE,
                                      message = defaultWorkflowMessages$plotDemography,
                                      settings = NULL) {
      self$plotDemography <- PlotTask$new(
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
                               taskFunction = NULL,
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
    #' Define reset report `Task` settings
    #' @param active logical indicating if `Task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    resetReportSettings = function(active = FALSE,
                                   message = defaultWorkflowMessages$resetReport,
                                   settings = NULL) {
      self$resetReport <- Task$new(
        active = active,
        workflowFolder = self$workflowFolder,
        message = message,
        settings = settings
      )
    },


    #' @description
    #' Loop through all simulation sets and run active population model workflow tasks for each.
    #' # POPULATION WORKFLOW
    #' # CORE STAGE 0:  DEMOGRAPHY SUMMARY
    #' # 0a - Demography Plots/Tables
    #' # CORE STAGE 1:  SIMULATION
    #' # 1a - Time profile Plot
    #' # CORE STAGE 2:  CALCULATE PK PARAMETER
    #' # 2a - PK parameter Plot
    #' # CORE STAGE 3:  CALCULATE SENSITIVITY
    #' # 3a - Plots and Tables based on sensitivity results
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of population workflow",
        pathFolder = self$workflowFolder
      )

      if (self$resetReport$active) {
        resetReport(self$reportFileName,
          logFolder = self$workflowFolder
        )
      }


      if (self$simulatePopulation$active) {
        self$simulatePopulation$runTask(self$simulationStructures)
      }

      if (self$populationPKParameters$active) {
        self$populationPKParameters$runTask(self$simulationStructures)
      }


      if (self$populationSensitivityAnalysis$active) {
        self$populationSensitivityAnalysis$runTask(self$simulationStructures)
      }

      # if (self$populationSensitivityAnalysis$active) {
      # if (self$populationSensitivityAnalysis$validateInput()) {
      # if (!is.null(file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder))) {
      #   dir.create(file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder))
      # }

      # for (set in self$simulationStructures) {
      #
      #   logWorkflow(
      #     message = paste0("Starting population sensitivity analysis: ",set$simulationSet$simulationSetName),
      #     pathFolder = self$workflowFolder
      #   )
      #
      #
      #
      #   set$sensitivityAnalysisResultsFileNames <- runPopulationSensitivityAnalysis(
      #     simFilePath = set$simulationSet$simulationFile,
      #     popDataFilePath = set$simulationSet$populationFile,
      #     pkParameterResultsFilePath = set$pkAnalysisResultsFileNames,
      #     resultsFileFolder = file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder),
      #     resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
      #     popSAResultsIndexFile = paste(set$simulationSet$simulationSetName,"sensitivityAnalysesResultsIndexFile",sep = "-"),
      #     variableParameterPaths = self$populationSensitivityAnalysis$settings$variableParameterPaths,
      #     pkParameterSelection = self$populationSensitivityAnalysis$settings$pkParameterSelection,
      #     variationRange = self$populationSensitivityAnalysis$settings$variationRange,
      #     quantileVec = self$populationSensitivityAnalysis$settings$quantileVec,
      #     numberOfCores = self$populationSensitivityAnalysis$settings$numberOfCores,
      #     logFolder = self$workflowFolder
      #   )
      #
      #   # set$sensitivityAnalysisResultsFileNames <- runPopulationSensitivityAnalysis(
      #   #   simFilePath = set$simulationSet$simulationFile,
      #   #   popDataFilePath = set$simulationSet$populationFile,
      #   #   pkParameterResultsFilePath = set$pkAnalysisResultsFileNames,
      #   #   resultsFileFolder = file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder),
      #   #   resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
      #   #   popSAResultsIndexFile = paste(set$simulationSet$simulationSetName,"sensitivityAnalysesResultsIndexFile",sep = "-"),
      #   #   variableParameterPaths = self$populationSensitivityAnalysis$settings$variableParameterPaths,
      #   #   pkParameterSelection = self$populationSensitivityAnalysis$settings$pkParameterSelection,
      #   #   variationRange = self$populationSensitivityAnalysis$settings$variationRange,
      #   #   quantileVec = self$populationSensitivityAnalysis$settings$quantileVec,
      #   #   numberOfCores = self$populationSensitivityAnalysis$settings$numberOfCores,
      #   #   logFolder = self$workflowFolder
      #   # )
      #
      #   # logWorkflow(
      #   #   message = "Population sensitivity analysis completed.",
      #   #   pathFolder = self$workflowFolder
      #   # )
      # }
      # }
      # }
    },


    #' @description
    #' Print workflow list of tasks
    #' TO DO: add simulationSets to print() method
    #' @return Task list information
    print = function() {
      taskOrder <- list(
        "Task 1" = self$simulatePopulation$print(),
        "Task 2" = self$pkParametersCalculation$print(),
        "Task 3" = self$sensitivityAnalysis$print(),
        "Task 4" = self$plotDemography$print(),
        "Task 5" = self$plotGoF$print(),
        "Task 6" = self$plotPKParameters$print(),
        "Task 7" = self$plotSensitivity$print()
      )
      invisible(self)

      return(taskOrder)
    }
  )
)
