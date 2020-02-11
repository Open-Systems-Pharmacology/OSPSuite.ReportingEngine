#' @title PopulationWorkflow
#' @description R6 class for Reporting Engine Population Workflow
#' @field populationSimulation R6 class `Task` for population simulation
#' @field populationPKParameters R6 class `Task` for PK parameters calculation
#' @field calculateSensitivity R6 class `Task` for sensitivity analysis
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
    populationSimulation = NULL, # TO DO: rename with simpler task name simulate
    populationPKParameters = NULL, # TO DO: rename with simpler task name calculatePKParameters
    calculateSensitivity = NULL,
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
      self$populationSimulationSettings()
      self$populationPKParameterSettings()
      self$plotGoFSettings()
      self$plotPKParametersSettings()
      self$calculateSensitivitySettings()
      self$plotSensitivitySettings()
    },


    populationSimulationSettings = function(input = NULL,
                                                output = NULL,
                                                settings = NULL,
                                                active = TRUE,
                                                message = NULL,
                                                inputFolderName = self$inputFolder,
                                                simulationFileName = self$simulation,
                                                populationFileName = self$population,
                                                resultsFolderName = self$simulationFolder,
                                                resultsFileName = "populationSimulation",
                                                numberOfCores = 1) {
      self$populationSimulation <- SimulationTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Simulate population",
        inputFolderName = inputFolderName,
        simulationFileName = simulationFileName,
        populationFileName = populationFileName,
        resultsFolderName = resultsFolderName,
        resultsFileName = resultsFileName,
        numberOfCores = numberOfCores
      )
    },

    populationPKParameterSettings = function(input = NULL,
                                                 output = NULL,
                                                 settings = NULL,
                                                 active = TRUE,
                                                 message = NULL,
                                                 simulationFilePath = file.path(self$inputFolder, paste0(self$simulation, ".pkml")),
                                                 simulationResultFilePaths = self$populationSimulation$generatedResultFileNames,
                                                 pkParametersToEvaluate = NULL,
                                                 userDefinedPKFunctions = NULL,
                                                 pkParameterResultsFilePath = file.path(self$pkParametersFolder, "populationPKParameters.csv")) {
      self$populationPKParameters <- CalculatePKParametersTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Calculate PK parameters for population",
        simulationFilePath = simulationFilePath,
        simulationResultFilePaths = simulationResultFilePaths,
        pkParametersToEvaluate = pkParametersToEvaluate,
        userDefinedPKFunctions = userDefinedPKFunctions,
        pkParameterResultsFilePath = pkParameterResultsFilePath
      )
    },

    # TO DO: Define the tasks settings for plots
    plotDemographySettings = function(input = NULL,
                                          output = NULL,
                                          active = TRUE,
                                          message = NULL) {
      self$plotDemography <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot Demography task not available at the moment"
      )
    },

    plotGoFSettings = function(input = NULL,
                                   output = NULL,
                                   active = TRUE,
                                   message = NULL) {
      self$plotGoF <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot Goodness of Fit task not available at the moment"
      )
    },

    plotPKParametersSettings = function(input = NULL,
                                            output = NULL,
                                            active = TRUE,
                                            message = NULL) {
      self$plotPKParameters <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot PK parameters task not available at the moment"
      )
    },

    plotSensitivitySettings = function(input = NULL,
                                           output = NULL,
                                           active = TRUE,
                                           message = NULL) {
      self$plotPKParameters <- Task$new(
        input = input,
        output = output,
        active = active,
        message = "Plot Sensitivity task not available at the moment"
      )
    },

    # TO DO: include this chunk into previous tasks
    # setDemographyPlotSettings = function(input = NULL,
    #                                          output = NULL,
    #                                          active = TRUE,
    #                                          message = NULL) {
    #   self$demographyPlot <- Task$new(
    #     input = input %||% self$populationSimulation$input,
    #     output = output %||% list(
    #       "demographyResults" = file.path(self$simulationFolder, "demography.RData"),
    #       "demographyPlot" = file.path(self$pkParametersFolder, "demographyPlot"),
    #       "demographyTable" = file.path(self$pkParametersFolder, "demographyTable.md")
    #     ),
    #     active = active,
    #     message = message %||% "Plot demography"
    #   )
    # },


    # setGofPlotSettings = function(input = NULL,
    #                                   output = NULL,
    #                                   active = TRUE,
    #                                   message = NULL) {
    #   self$gofPlot <- Task$new(
    #     input = input %||% list(
    #       "population" = file.path(self$inputFolder, paste0(self$population, ".csv")),
    #       "populationSimulation" = self$populationSimulation$output$populationSimulation
    #     ),
    #     output = output %||% list(
    #       "gofResults" = file.path(self$simulationFolder, "gofResults.RData"),
    #       "gofPlot" = file.path(self$pkParametersFolder, "gofPlot")
    #     ),
    #     active = active,
    #     message = message %||% "Plot goodness of fit diagnostics"
    #   )
    # },

    runWorkflow = function() {
      logInfo(message = "Start of population workflow")
      # POPULATION WORKFLOW

      # CORE STAGE 0:  DEMOGRAPHY SUMMARY
      # 0a - Demography Plots/Tables

      # CORE STAGE 1:  SIMULATION
      # 1a - Time profile Plot

      # CORE STAGE 2:  CALCULATE PK PARAMETER
      # 2a - PK parameter Plot

      # CORE STAGE 3:  CALCULATE SENSITIVITY
      # 3a - Plots and Tables based on sensitivity results

      if (self$populationSimulation$active) {
        if (self$populationSimulation$validateInput()) {
          if (self$populationSimulation$numberOfCores == 1) {
            print("Starting population simulation")
            resultsFilePath <- file.path(self$populationSimulation$resultsFolderName, paste0(self$populationSimulation$resultsFileName, ".csv"))
            simulateModel(
              simFilePath = file.path(self$populationSimulation$inputFolderName, paste0(self$populationSimulation$simulationFileName, ".pkml")),
              popDataFilePath = file.path(self$populationSimulation$inputFolderName, paste0(self$populationSimulation$populationFileName, ".csv")),
              resultsFilePath = resultsFilePath
            )
            self$populationSimulation$generatedResultFileNames <- resultsFilePath
          }
          else if (self$populationSimulation$numberOfCores > 1) {
            print("Starting parallel population simulation")
            self$populationSimulation$generatedResultFileNames <- runParallelPopulationSimulation(
              numberOfCores = self$populationSimulation$numberOfCores,
              inputFolderName = self$populationSimulation$inputFolderName,
              simulationFileName = self$populationSimulation$simulationFileName,
              populationFileName = self$populationSimulation$populationFileName,
              resultsFolderName = self$populationSimulation$resultsFolderName,
              resultsFileName = self$populationSimulation$resultsFileName
            )
          }
        }
      }


      if (self$populationPKParameters$active) {
        if (self$populationPKParameters$validateInput()) {
          print("Starting PK parameter calculation")
          self$populationPKParameters$generatedResultFileNames <- calculatePKParameters(
            simulationFilePath = self$populationPKParameters$simulationFilePath,
            simulationResultFilePaths = self$populationSimulation$generatedResultFileNames,
            pkParameterResultsFilePath = self$populationPKParameters$pkParameterResultsFilePath
          )
        }
      }

      # TO DO: Abdullah, I don't know if this chunk have to be included somewhere so I left it as is
      # self$numberOfCores,
      # self$populationSimulation$input$population,
      # self$inputFolder,
      # self$population,
      # simFileName,
      # popFileName,
      # wdir,
      # inputFolder,
      # pkParametersFolder,
      # resultsFileName,

      #
      #             library("Rmpi")
      #             mpi.spawn.Rslaves(nslaves = self$numberOfCores)
      #
      #             # Check that the correct number of slaves has been spawned.
      #             if (!(mpi.comm.size() - 1 == self$numberOfCores)) { #-1 since mpi.comm.size() counts master
      #               mpi.close.Rslaves()
      #               stop(paste0(self$numberOfCores, " cores were not successfully spawned."))
      #             }
      #             mpi.bcast.cmd(library("ospsuite"))
      #             mpi.bcast.cmd(library("ospsuite.reportingengine"))
      #             tempPopDataFiles <- ospsuite::splitPopulationFile(
      #               csvPopulationFile = self$populationSimulation$input$population,
      #               numberOfCores = self$numberOfCores,
      #               pkParametersFolder = paste0(inputFolder, "/"),
      #               outputFileName = popFileName
      #             )
      #             mpi.bcast.Robj2slave(obj = simFileName)
      #             mpi.bcast.Robj2slave(obj = popFileName)
      #             mpi.bcast.Robj2slave(obj = tempPopDataFiles)
      #             mpi.bcast.Robj2slave(obj = wdir)
      #             mpi.bcast.Robj2slave(obj = inputFolder)
      #             mpi.bcast.Robj2slave(obj = pkParametersFolder)
      #
      #             mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
      #               simFileName = paste0(simFileName, ".pkml"),
      #               simFileFolder = paste0(wdir, "/", inputFolder, "/"),
      #               popDataFileName = paste0(popFileName, "_", mpi.comm.rank(), ".csv"),
      #               popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
      #               resultFileName = paste0(resultsFileName, "_", mpi.comm.rank(), ".csv"),
      #               resultFileFolder = paste0(wdir, "/", pkParametersFolder, "/")
      #             ))
      #             mpi.close.Rslaves() # Move to end of workflow

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


    # TO DO: include these chunk into the previous task
    # if (self$pkParametersCalculation$active) {
    #   # if (self$pkParametersCalculation$validateInput()){
    #   # calculatePKParameters()
    #   # }
    # }
    # if (self$sensitivityAnalysis$active) {
    #   if (self$sensitivityAnalysis$validateInput()) {
    #     simulation <- loadSimulation(self$demographyPlot$input$simulation)
    #
    #     pkSensitivities <- analyzeSensitivity(simulation = simulation)
    #     save(pkSensitivities, file = self$sensitivityAnalysis$output$sensitivityAnalysis)
    #   }
    # }
    # if (self$demographyPlot$active) {
    #   if (self$demographyPlot$validateInput()) {
    #     population <- loadPopulation(self$demographyPlot$input$population)
    #     simulation <- loadSimulation(self$demographyPlot$input$simulation)
    #
    #     # The last properties of plotDemograpy will be set within task settings
    #     demographyPlot <- plotDemography(
    #       simulation = simulation,
    #       population = population,
    #       parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height),
    #       plotConfiguration = NULL
    #     )
    #
    #     save(demographyPlot, file = self$demographyPlot$output$demographyResults)
    #     dir.create(self$demographyPlot$output$demographyPlot)
    #     for (plotName in names(demographyPlot)) {
    #       ggplot2::ggsave(
    #         filename = file.path(self$demographyPlot$output$demographyPlot, paste0(removeForbiddenLetters(plotName), ".png")),
    #         plot = demographyPlot[[plotName]]
    #       )
    #     }
    #   }
    # }
    # if (self$gofPlot$active) {
    #   if (self$gofPlot$validateInput()) {
    #     load(file = self$gofPlot$input$populationSimulation)
    #     population <- loadPopulation(self$gofPlot$input$population)
    #     observedData <- self$gofPlot$input$observedData
    #
    #     gofPlot <- plotGoodnessOfFit(
    #       populationSimulation = populationSimulation,
    #       population = population,
    #       observedData = observedData,
    #       quantity = NULL,
    #       plotConfiguration = NULL
    #     )
    #
    #     save(gofPlot, file = self$gofPlot$output$gofResults)
    #     dir.create(self$gofPlot$output$gofPlot)
    #     for (plotName in names(gofPlot)) {
    #       ggplot2::ggsave(
    #         filename = file.path(self$gofPlot$output$gofPlot, paste0(removeForbiddenLetters(plotName), ".png")),
    #         plot = gofPlot[[plotName]]
    #       )
    #     }
    #   }
    # }
    # if (self$pkParametersPlot$active) {
    #   self$pkParametersPlot$output <- plotPKParameters()
    # }
    # if (self$sensitivityPlot$active) {
    #   if (self$sensitivityPlot$validateInput()) {
    #     load(file = self$sensitivityPlot$input$sensitivityAnalysis)
    #
    #     sensitivityPlot <- plotSensitivity(sensitivityAnalysis)
    #     save(sensitivityPlot, file = file.path(self$sensitivityPlot$output$sensitivityPlot))
    #   }
    # }

    #' @description
    #' Print workflow list of tasks
    #' TO DO: add simulationSets to print() method
    #' @return Task list information
    print = function() {
      taskOrder <- list(
        "Task 1" = self$populationSimulation$print(),
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
