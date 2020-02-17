#' @title MeanModelWorkflow
#' @description  R6 class for Reporting Engine Mean Model Workflow
#' @field meanModelSimulation R6 class `Task` for simulation
#' @field meanModelPKParameters R6 class `Task` for PK parameters calculation
#' @field meanModelSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotGoF R6 class `Task` for goodness of fit plots
#' @field plotMassBalance R6 class `Task` for mass balance plot
#' @field plotAbsorption R6 class `Task` for absorption plot
#' @field plotPKParameters R6 class `Task` for PK parameters plot
#' @field plotSensitivity R6 class `Task` for sensitivity plot
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
      #    inputFolderName = getwd(),
      #     simulationFileName = self$simulations,
      #     resultsFolderName = self$resultsFolder,
      #     resultsFileName = "meanModelSimulation"
      self$meanModelSimulation <- SimulationTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Simulate mean model" # ,
        # inputFolderName = inputFolderName,
        # simulationFileName = simulationFileName,
        # resultsFolderName = resultsFolderName,
        # resultsFileName = resultsFileName
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
      # ,
      # simulationFilePath = file.path(self$inputFolder, paste0(self$simulation, ".pkml")),
      # simulationResultFilePaths = self$meanModelSimulation$generatedResultFileNames,
      # pkParametersToEvaluate = NULL,
      # userDefinedPKFunctions = NULL,
      # pkParameterResultsFilePath = file.path(self$pkParametersFolder, "meanModelPKParameters.csv")) {
      self$meanModelPKParameters <- CalculatePKParametersTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Calculate mean model PK parameters" # ,
        # simulationFilePath = simulationFilePath,
        # simulationResultFilePaths = simulationResultFilePaths,
        # pkParametersToEvaluate = pkParametersToEvaluate,
        # userDefinedPKFunctions = userDefinedPKFunctions,
        # pkParameterResultsFilePath = pkParameterResultsFilePath
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
    #' @param numberOfCores TO DO
    #' @return A new `Task` object
    meanModelSensitivityAnalysisSettings = function(input = NULL,
                                                    output = NULL,
                                                    settings = NULL,
                                                    active = TRUE,
                                                    message = NULL,
                                                    # inputFolderName = self$inputFolder,
                                                    # simulationFileName = self$simulation,
                                                    # resultsFolderName = self$sensitivityFolder,
                                                    # resultsFileName = "meanModelSensitivityAnalysis",
                                                    numberOfCores = 1) {
      self$meanModelSensitivityAnalysis <- SensitivityAnalysisTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for mean model",
        # inputFolderName = inputFolderName,
        # simulationFileName = simulationFileName,
        # resultsFolderName = resultsFolderName,
        # resultsFileName = resultsFileName,
        numberOfCores = numberOfCores
      )
    },

    # TO DO: Define the tasks settings for plots
    #' @description
    #' Define Goodness of fit `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotGoFSettings = function(input = NULL,
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
    plotPKParametersSettings = function(input = NULL,
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
    #' Define plot mass balance `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotMassBalanceSettings = function(input = NULL,
                                       output = NULL,
                                       active = FALSE,
                                       message = NULL) {
      self$plotMassBalance <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot Mass Balance task not available at the moment"
      )
    },

    #' @description
    #' Define plot absorption `task` settings
    #' @param input file or folder of input
    #' @param output file or folder of output
    #' @param settings specific settings for task
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `TRUE`
    #' @param message title of the `task`.
    #' Default value indicates `task` name.
    #' @return A new `Task` object
    plotAbsorptionSettings = function(input = NULL,
                                      output = NULL,
                                      active = FALSE,
                                      message = NULL) {
      self$plotAbsorption <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot Absorption task not available at the moment"
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
    plotSensitivitySettings = function(input = NULL,
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
    #' Loop through all simulation sets and run active mean model workflow tasks for each
    runWorkflow = function() {
      logInfo(message = "Start of mean model workflow")

      for (set in self$simulationStructures) {
        # print(set)
        self$runSingleSetWorkflow(set)
      }
    },

    #' @description
    #' Run population workflow tasks for a single simulation set
    #' @return All results and plots as a structured output in a folder specific to simulation set.
    runSingleSetWorkflow = function(set) {

      # MEAN MODEL WORKFLOW
      # CORE STAGE 1:  SIMULATION
      # 1a - Mass Balance Plot
      # 1b - Time profile Plot
      # 1c - Absorption Plot

      # CORE STAGE 2:  CALCULATE PK PARAMETER
      # 2a - PK parameter Plot

      # CORE STAGE 3:  CALCULATE SENSITIVITY
      # 3a - Plots and Tables based on sensitivity results

      if (self$meanModelSimulation$active) {
        # TO DO: include simulationSets
        # for (simulationSet in self$simulationSets){}
        if (self$meanModelSimulation$validateInput()) {
          logInfo(message = "Starting mean model simulation")
          createFolder( set$simulationResultsFolder )
          set$simulationResultFileNames <- simulateModel(
            simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
            resultsFilePath = file.path(set$simulationResultsFolder, defaultFileNames$simulationResultsFile(set$simulationSet$simulationSetName))
          )
        }
      }

      if (self$meanModelPKParameters$active) {
        if (self$meanModelPKParameters$validateInput()) {
          logInfo(message = "Starting mean model PK parameters calculation")
          createFolder(set$pkAnalysisResultsFolder)
          set$pkAnalysisResultsFileNames <- calculatePKParameters(
            simulationFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
            simulationResultFilePaths = set$simulationResultFileNames,
            pkParameterResultsFilePath = file.path(set$pkAnalysisResultsFolder, defaultFileNames$pkAnalysisResultsFile(set$simulationSet$simulationSetName))
          )
        }
      }

      if (self$meanModelSensitivityAnalysis$active) {
        if (self$meanModelSensitivityAnalysis$validateInput()) {
          logInfo(message = "Starting mean model sensitivity analysis")
          createFolder(set$sensitivityAnalysisResultsFolder)
          set$sensitivityAnalysisResultsFileNames <- analyzeSensitivity(
            simFilePath = file.path(set$inputFilesFolder, paste0(set$simulationSet$simulationName, ".pkml")),
            resultsFileFolder = set$sensitivityAnalysisResultsFolder,
            resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
            numberOfCores = self$meanModelSensitivityAnalysis$numberOfCores
          )
        }
      }

      # TO DO: plug plot tasks to actual results
      if (self$plotGoF$active) {
        logInfo(message = self$plotGoF$message)
      }
      if (self$plotMassBalance$active) {
        logInfo(message = self$plotMassBalance$message)
      }
      if (self$plotAbsorption$active) {
        logInfo(message = self$plotAbsorption$message)
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
