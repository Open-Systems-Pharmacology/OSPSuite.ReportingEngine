#' @title PopulationWorkflow
#' @description R6 class for Reporting Engine Population Workflow
#' @field simulatePopulation R6 class `Task` for population simulation
#' @field populationPKParameters R6 class `Task` for PK parameters calculation
#' @field populationSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotDemography R6 class `Task` for demography plots
#' @field plotGoF R6 class `Task` for goodness of fit plots
#' @field plotPKParameters R6 class `Task` for PK parameters plot
#' @field plotSensitivity R6 class `Task` for sensitivity plot
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

    #' @description
    #' Create a new `PopulationWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @return A new `PopulationWorkflow` object
    initialize = function(...) {
      super$initialize(...)

      # TO DO: include task parameters from initialization ?
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
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @param numberOfCores number of cores for parallelization
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
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    populationPKParameterSettings = function(taskFunction = NULL,
                                             outputFolder = defaultTaskOutputFolders$calculatePKParameters,
                                             settings = NULL,
                                             active = TRUE,
                                             message = NULL) {
      self$populationPKParameters <- CalculatePKParametersTask$new(
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Calculate PK parameters for population"
      )
    },

    #' @description
    #' Define population Sensitivity Analysis `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @param variationRange variation range for sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    #' @param quantileVec vector of quantiles to be calculated
    #' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    #' @param pkParameterSelection list of selected PK parameters for sensitivity analysis
    #' @return A new `SensitivityAnalysisTask` object
    populationSensitivityAnalysisSettings = function(taskFunction = NULL,
                                                     outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
                                                     settings = NULL,
                                                     active = TRUE,
                                                     message = NULL,
                                                     variationRange = NULL,
                                                     numberOfCores = NULL,
                                                     quantileVec = NULL,
                                                     variableParameterPaths = NULL,
                                                     pkParameterSelection = NULL) {
      self$populationSensitivityAnalysis <- SensitivityAnalysisTask$new(
        output = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for population",
        variationRange = variationRange,
        numberOfCores = numberOfCores,
        quantileVec = quantileVec,
        variableParameterPaths = variableParameterPaths,
        pkParameterSelection = pkParameterSelection
      )
    },

    # TO DO: Define the tasks settings for plots
    #' @description
    #' Define plot demography `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotDemographySettings = function(taskFunction,
                                      input = NULL,
                                      output = NULL,
                                      active = FALSE,
                                      message = NULL) {
      self$plotDemography <- Task$new(
        output = output,
        active = active,
        message = "Plot Demography task not available at the moment"
      )
    },

    #' @description
    #' Define plot goodness of fit `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotGoFSettings = function(taskFunction,
                               output = NULL,
                               active = FALSE,
                               message = NULL) {
      self$plotGoF <- Task$new(
        output = output,
        active = active,
        message = "Plot Goodness of Fit task not available at the moment"
      )
    },

    #' @description
    #' Define plot PK parameters `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotPKParametersSettings = function(taskFunction,
                                        output = NULL,
                                        active = FALSE,
                                        message = NULL) {
      self$plotPKParameters <- Task$new(
        output = output,
        active = active,
        message = "Plot PK parameters task not available at the moment"
      )
    },

    #' @description
    #' Define plot sensisitivity `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotSensitivitySettings = function(taskFunction,
                                       output = NULL,
                                       active = FALSE,
                                       message = NULL) {
      self$plotSensitivity <- Task$new(
        output = output,
        active = active,
        message = "Plot Sensitivity task not available at the moment"
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

      initializeRmdFile(self$reportFileName,
        title = "Population Workflow Report"
      )

      if (self$simulatePopulation$active) {
        self$simulatePopulation$runTask(self$simulationStructures)
      }




      if (self$populationPKParameters$active) {
        if (self$populationPKParameters$validateInput()) {
          if (!is.null(file.path(self$populationPKParameters$workflowFolder, self$populationPKParameters$outputFolder))) {
            dir.create(file.path(self$populationPKParameters$workflowFolder, self$populationPKParameters$outputFolder))
          }

          for (set in self$simulationStructures) {
            logWorkflow(
              message = "Starting PK parameter calculation",
              pathFolder = self$workflowFolder
            )
            pkAnalyses <- calculatePKParameters(set)
            exportPKAnalysesToCSV(
              pkAnalyses = pkAnalyses,
              filePath = set$pkAnalysisResultsFileNames
            )

            logWorkflow(
              message = "PK parameter calculation completed.",
              pathFolder = self$workflowFolder
            )
          }
        }
      }


      if (self$populationSensitivityAnalysis$active) {
        if (self$populationSensitivityAnalysis$validateInput()) {
          if (!is.null(file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder))) {
            dir.create(file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder))
          }

          logWorkflow(
            message = "Starting population sensitivity analysis",
            pathFolder = self$workflowFolder
          )

          set$sensitivityAnalysisResultsFileNames <- runPopulationSensitivityAnalysis(
            simFilePath = set$simulationSet$simulationFile,
            popDataFilePath = set$simulationSet$populationFile,
            pkParameterResultsFilePath = set$pkAnalysisResultsFileNames,
            variableParameterPaths = self$populationSensitivityAnalysis$variableParameterPaths,
            resultsFileFolder = file.path(self$populationSensitivityAnalysis$workflowFolder, self$populationSensitivityAnalysis$outputFolder),
            resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
            pkParameterSelection = self$populationSensitivityAnalysis$pkParameterSelection,
            variationRange = self$populationSensitivityAnalysis$variationRange,
            quantileVec = self$populationSensitivityAnalysis$quantileVec,
            numberOfCores = self$populationSensitivityAnalysis$numberOfCores,
            logFolder = self$workflowFolder
          )

          logWorkflow(
            message = "Population sensitivity analysis completed.",
            pathFolder = self$workflowFolder
          )
        }
      }
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
