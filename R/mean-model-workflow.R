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
                                                        variationRange = 0.1,
                                                        numberOfCores = 1) {
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
                                   output = file.path(self$resultsFolder, "TimeProfile"),
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
    #' Run population workflow tasks for a single simulation set
    #' @return All results and plots as a structured output in a folder specific to simulation set.
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of mean model workflow",
        pathFolder = self$workflowFolder
      )

      initializeRmdFile(self$reportFileName,
        title = "Mean Model Workflow Report"
      )

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
              variationRange = self$meanModelSensitivityAnalysis$variationRange,
              resultsFileFolder = set$sensitivityAnalysisResultsFolder,
              resultsFileName = trimFileName(defaultFileNames$sensitivityAnalysisResultsFile(set$simulationSet$simulationSetName), extension = "csv"),
              numberOfCores = self$meanModelSensitivityAnalysis$numberOfCores
            )
          }
        }
      }

      if (self$plotGoF$active) {
        logWorkflow(
          message = paste0("Starting ", self$plotGoF$message),
          pathFolder = self$workflowFolder
        )

        addRmdTextChunk(
          self$reportFileName,
          "# Time profiles and residuals"
        )

        dir.create(self$plotGoF$output[[1]])
        residualsAcrossAllSimulations <- NULL
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Plot time profile")
            ),
            pathFolder = self$workflowFolder
          )
          if (self$plotGoF$validateInput()) {

            # TO DO: Add obs vs pred, residuals vs time, residual vs sim to plot
            gofResults <- plotMeanGoodnessOfFit(set,
              plotConfigurations = self$plotGoF$settings$plotConfigurations
            )

            residualsAcrossAllSimulations <- rbind.data.frame(
              residualsAcrossAllSimulations,
              gofResults$residuals$data
            )
            addRmdTextChunk(
              self$reportFileName,
              paste0("Simulation: ", set$simulationSet$simulationSetName)
            )

            gofPlotTypes <- names(gofResults$plots)

            for (gofPlotType in gofPlotTypes) {
              gofFileName <- file.path(self$plotGoF$output[[1]], paste0(set$simulationSet$simulationSetName, gofPlotType, ".png"))

              # TO DO: plug ggsave option to GoF settintgs/plotConfigurations
              # Depending on the report options, it is possible to configure subplots here
              ggplot2::ggsave(
                filename = gofFileName,
                plot = gofResults$plots[[gofPlotType]],
                width = 16, height = 9, units = "cm"
              )

              addRmdFigureChunk(
                fileName = self$reportFileName,
                figureFile = gofFileName,
                figureCaption = gofPlotType
              )
            }
          }
        }
        # TO DO:
        # In my understanding of the github instructions,
        # there should be merging of all the residual here
        # leading to a histogram and qq plot of all the the residuals
        if (!is.null(residualsAcrossAllSimulations)) {
          addRmdTextChunk(
            self$reportFileName,
            "## Residuals across all simulations"
          )

          residualHistogramPlot <- tlf::plotHistogram(
            data = residualsAcrossAllSimulations,
            dataMapping = tlf::HistogramDataMapping$new(x = "residuals"),
            plotConfiguration = self$plotGoF$settings$plotConfigurations[["histogram"]]
          )
          ggplot2::ggsave(
            filename = file.path(self$plotGoF$output[[1]], paste0("residuals.png")),
            plot = residualHistogramPlot,
            width = 16, height = 9, units = "cm"
          )

          addRmdFigureChunk(
            fileName = self$reportFileName,
            figureFile = file.path(self$plotGoF$output[[1]], paste0("residuals.png")),
            figureCaption = "Histogram of residuals"
          )
        }
      }

      # TO DO: plug plot tasks to actual results
      if (self$plotMassBalance$active) {
        logWorkflow(
          message = paste0("Starting ", self$plotMassBalance$message),
          pathFolder = self$workflowFolder
        )
        addRmdTextChunk(
          self$reportFileName,
          "# Mass balance"
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Plot Mass Balance")
            ),
            pathFolder = self$workflowFolder
          )
          if (self$plotMassBalance$validateInput()) {

            # massbalancePlot <- ggplot2::ggplot() + ggplot2::ggtitle("TO DO: Mass Balance")

            # ggplot2::ggsave(filename = file.path(self$figuresFolder, paste0(set$simulationSet$simulationName, "-mass-balance.png")))
          }
        }
      }
      if (self$plotAbsorption$active) {
        logWorkflow(
          message = paste0("Starting ", self$plotAbsorption$message),
          pathFolder = self$workflowFolder
        )
        addRmdTextChunk(
          self$reportFileName,
          "# Absoprtion"
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Plot Absorption")
            ),
            pathFolder = self$workflowFolder
          )
          if (self$plotAbsorption$validateInput()) {

            # absorptionPlot <- ggplot2::ggplot() + ggplot2::ggtitle("TO DO: Absorption plot")

            # ggplot2::ggsave(filename = file.path(self$figuresFolder, paste0(set$simulationSet$simulationName, "-absorption.png")))
          }
        }
      }

      if (self$plotPKParameters$active) {
        logWorkflow(
          message = paste0("Starting ", self$plotPKParameters$message),
          pathFolder = self$workflowFolder
        )
        addRmdTextChunk(
          self$reportFileName,
          "# PK parameters"
        )
        for (set in self$simulationStructures) {
          logWorkflow(
            message = c(
              paste0("Simulation: ", set$simulationSet$simulationName),
              paste0("Plot PK parameters")
            ),
            pathFolder = self$workflowFolder
          )
          if (self$plotPKParameters$validateInput()) {

            # pkParametersPlot <- ggplot2::ggplot() + ggplot2::ggtitle("TO DO: PK parameter plot")

            # ggplot2::ggsave(filename = file.path(self$figuresFolder, paste0(set$simulationSet$simulationName, "-pkParameters.png")))
          }
        }
      }

      if (self$plotSensitivity$active) {
        logWorkflow(
          message = paste0("Starting ", self$plotSensitivity$message),
          pathFolder = self$workflowFolder
        )
        addRmdTextChunk(
          self$reportFileName,
          "# Sensititvity Analysis"
        )
        addRmdTextChunk(
          self$reportFileName,
          "TO DO: Create Sensitivity Plots for report"
        )
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
