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
    simulatePopulationSettings = function(taskFunction = simulatePopulationModel,
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
    populationPKParameterSettings = function(taskFunction,
                                             output = NULL,
                                             settings = NULL,
                                             active = TRUE,
                                             message = NULL) {
      self$populationPKParameters <- CalculatePKParametersTask$new(
        input = input,
        output = output,
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
    populationSensitivityAnalysisSettings = function(output = NULL,
                                                     settings = NULL,
                                                     active = TRUE,
                                                     message = NULL,
                                                     variationRange = NULL,
                                                     numberOfCores = NULL,
                                                     quantileVec = NULL,
                                                     variableParameterPaths = NULL,
                                                     pkParameterSelection = NULL) {
      self$populationSensitivityAnalysis <- SensitivityAnalysisTask$new(
        input = input,
        output = output,
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
        input = input,
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
        input = input,
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
        input = input,
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
        input = input,
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


      for (set in self$simulationStructures) {
        # print(set)
        self$runSingleSetWorkflow(set)
      }
    },

    #' @description
    #' Run population workflow tasks for a single simulation set
    #' @param set R6 class `SimulationStructure` object
    #' @return All results and plots as a structured output in a folder specific to simulation set
    runSingleSetWorkflow = function(set) {
      logInfo(message = "Start of population workflow")

      if (self$simulatePopulation$active) {
        if (self$simulatePopulation$validateInput()) {
          # if (self$simulatePopulation$numberOfCores == 1) {
          #   logInfo(message = "Starting population simulation")
          #   createFolder(set$simulationResultsFolder)
          #   resultsFilePath <- file.path(set$simulationResultsFolder, defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName))
          #   simulateModel(
          #     simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
          #     popDataFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$populationName, ".csv")),
          #     resultsFilePath = resultsFilePath
          #   )
          #   set$simulationResultFileNames <- resultsFilePath
          #   logInfo(message = "Population simulation completed.")
          # }
          # else if (self$simulatePopulation$numberOfCores > 1) {
          #   logInfo(message = "Starting parallel population simulation")
          #   createFolder(set$simulationResultsFolder)
          #   set$simulationResultFileNames <- runParallelsimulatePopulation(
          #     numberOfCores = self$simulatePopulation$numberOfCores,
          #     inputFolderName = set$inputFilesFolder,
          #     simulationFileName = set$simulationSet$simulationName,
          #     populationFileName = set$simulationSet$populationName,
          #     resultsFolderName = set$simulationResultsFolder,
          #     resultsFileName = trimFileName(defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName), extension = "csv")
          #   )
          #   logInfo(message = "Parallel population simulation completed.")
          # }
        }
      }


      if (self$populationPKParameters$active) {
        if (self$populationPKParameters$validateInput()) {
          logInfo("Starting PK parameter calculation")
          createFolder(set$pkAnalysisResultsFolder)
          set$pkAnalysisResultsFileNames <- calculatePKParameters(
            simulationFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
            simulationResultFilePaths = set$simulationResultFileNames,
            pkParameterResultsFilePath = file.path(set$pkAnalysisResultsFolder, defaultFileNames$pkAnalysisResultsFile(set$simulationSet$simulationSetName))
          )
          logInfo("PK parameter calculation completed.")
        }
      }


      if (self$populationSensitivityAnalysis$active) {
        if (self$populationSensitivityAnalysis$validateInput()) {
          logInfo("Starting population sensitivity analysis")
          createFolder(set$sensitivityAnalysisResultsFolder)
          set$sensitivityAnalysisResultsFileNames <- runPopulationSensitivityAnalysis(
            simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
            variableParameterPaths = self$populationSensitivityAnalysis$variableParameterPaths,
            popDataFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$populationName, ".csv")),
            pkParameterResultsFilePath = file.path(set$pkAnalysisResultsFolder, defaultFileNames$pkAnalysisResultsFile(set$simulationSet$simulationSetName)),
            pkParameterSelection = self$populationSensitivityAnalysis$pkParameterSelection,
            resultsFileFolder = set$sensitivityAnalysisResultsFolder,
            resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
            variationRange = self$populationSensitivityAnalysis$variationRange,
            quantileVec = self$populationSensitivityAnalysis$quantileVec,
            numberOfCores = self$populationSensitivityAnalysis$numberOfCores
          )
          logInfo("Population sensitivity analysis completed.")
        }
      }

      # TO DO: plug plot tasks to actual results
      if (self$plotDemography$active) {
        logInfo(message = self$plotDemography$message)
      }
      if (self$plotGoF$active) {
        logInfo(message = self$plotGoF$message)
      }
      if (self$plotPKParameters$active) {
        logInfo(message = self$plotPKParameters$message)
      }
      if (self$plotSensitivity$active) {
        logInfo(message = self$plotSensitivity$message)
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
