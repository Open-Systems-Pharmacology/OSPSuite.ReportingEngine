#' @title MeanModelWorkflow
#' @docType class
#' @description  Mean Model Workflow Task for Reporting Engine
#' @field meanModelSimulation Population simulation task
#' @field pkParametersCalculation PK parameters calculation task
#' @field sensitivityAnalysis Sensitivity analysis task
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize reporting engine mean model workflow}
#' \item{setMeanModelSimulationSettings()}{Define mean model simulation task settings}
#' \item{runWorkflow()}{Run the active tasks of mean model worklfow}
#' }
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
MeanModelWorkflow <- R6::R6Class(
  "MeanModelWorkflow",
  inherit = Workflow,

  public = list(
    meanModelSimulation = NULL,
    meanModelSensitivityAnalysis = NULL,
    sensitivityPlot = NULL,

    initialize = function(...) {
      super$initialize(...)


      # if (!is.null(numberOfCores)) {
      #   validateIsInteger(numberOfCores)
      #   self$numberOfCores <- numberOfCores
      # }



      self$setMeanModelSimulationSettings()
      #      self$setPKParametersCalculationSettings()
      #self$setSensitivityAnalysisSettings()

      # self$setDemographyPlotSettings()
      # self$setGofPlotSettings()
      # self$setpkParametersPlotSettings()
      # self$setSensitivityPlotSettings()
    },

    setMeanModelSimulationSettings = function(input = NULL,
                                              output = NULL,
                                              settings = NULL,
                                              active = TRUE,
                                              message = NULL,
                                              simulationFilePath = file.path(self$inputFolder,paste0(self$simulation, ".pkml")),
                                              populationFilePath = NULL,
                                              resultFileName = "meanModelSimulation") {
      self$meanModelSimulation <- SimulationTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Simulate mean model",
        simulationFilePath = simulationFilePath,
        populationFilePath = populationFilePath,
        resultFileName = resultFileName
      )
    },



    setSensitivityAnalysisSettings = function(input = NULL,
                                              output = NULL,
                                              settings = NULL,
                                              active = TRUE,
                                              message = NULL,
                                              simulationFilePath = file.path(self$inputFolder,paste0(self$simulation, ".pkml")),
                                              populationFilePath = NULL,
                                              resultFileName = "meanModelSimulation") {
      self$meanModelSensitivityAnalysis <- SensitivityAnalysisTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for mean model",
        simulationFilePath = simulationFilePath,
        populationFilePath = populationFilePath,
        resultFileName = resultFileName
      )
    },


    # setSensitivityAnalysisSettings = function(input = NULL,
    #                                           output = NULL,
    #                                           settings = NULL,
    #                                           active = TRUE,
    #                                           message = NULL,
    #                                           resultFileName = "simulationResults") {
    #   self$meanModelSensitivityAnalysis <- Task$new(
    #     input = input %||% list(),
    #     output = output %||% list("meanModelSensitivityAnalysisResultsFileName" = "meanModelSensitivityAnalysis"),
    #     settings = settings,
    #     active = active,
    #     message = message %||% "Perform sensitivity analysis on mean model"
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
    #       "gofPlot" = file.path(self$outputFolder, "gofPlot")
    #     ),
    #     active = active,
    #     message = message %||% "Plot goodness of fit diagnostics"
    #   )
    # },

    runWorkflow = function() {


      #MEAN MODEL WORKFLOW
      #CORE STAGE 1:  SIMULATION
      # 1a - Mass Balance Plot
      # 1b - Time profile Plot
      # 1c - Absorption Plot

      #CORE STAGE 2:  CALCULATE PK PARAMETER
      # 2a - PK parameter Plot

      #CORE STAGE 3:  CALCULATE SENSITIVITY
      # 3a - Plots and Tables based on sensitivity results




      print("Start of mean model workflow: ")
      print(self$reportingEngineInfo)



      if (self$meanModelSimulation$active) {
        if (self$meanModelSimulation$validateInput()) {
          print("Starting mean model simulation")
          print(self$meanModelSimulation$simulationFilePath)
          print( file.path(self$outputFolder,paste0(self$meanModelSimulation$resultFileName,".csv"))   )
          simulateModel(simFilePath     = self$meanModelSimulation$simulationFilePath,
                        resultsFilePath = file.path(self$outputFolder,paste0(self$meanModelSimulation$resultFileName,".csv")))
        }
      }

      #
      #       if (self$meanModelSensitivityAnalysis$active) {
      #        # if (self$meanModelSensitivityAnalysis$validateInput()) {
      #       #    print("Starting mean model sensitivity analysis")
      #           #print(file.path(self$inputFolder,paste0(self$simulation, ".pkml")))
      #           #print("DDDD")
      #           #print(file.path(self$outputFolder))
      #           #print(self$meanModelSimulation$output$meanModelSensitivityAnalysisResultsFileName)
      #           # analyzeSensitivity(
      #           #   simFilePath = file.path(self$inputFolder,paste0(self$simulation, ".pkml")),
      #           #   resultsFileFolder = file.path(self$outputFolder),
      #           #   resultsFileName =  self$meanModelSimulation$output$meanModelSensitivityAnalysisResultsFileName
      #           # )
      #        # }
      #       }


    },






    # self$numberOfCores,
    # self$populationSimulation$input$population,
    # self$inputFolder,
    # self$population,
    # simFileName,
    # popFileName,
    # wdir,
    # inputFolder,
    # outputFolder,
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
    #               outputFolder = paste0(inputFolder, "/"),
    #               outputFileName = popFileName
    #             )
    #             mpi.bcast.Robj2slave(obj = simFileName)
    #             mpi.bcast.Robj2slave(obj = popFileName)
    #             mpi.bcast.Robj2slave(obj = tempPopDataFiles)
    #             mpi.bcast.Robj2slave(obj = wdir)
    #             mpi.bcast.Robj2slave(obj = inputFolder)
    #             mpi.bcast.Robj2slave(obj = outputFolder)
    #
    #             mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
    #               simFileName = paste0(simFileName, ".pkml"),
    #               simFileFolder = paste0(wdir, "/", inputFolder, "/"),
    #               popDataFileName = paste0(popFileName, "_", mpi.comm.rank(), ".csv"),
    #               popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
    #               resultFileName = paste0(resultsFileName, "_", mpi.comm.rank(), ".csv"),
    #               resultFileFolder = paste0(wdir, "/", outputFolder, "/")
    #             ))
    #             mpi.close.Rslaves() # Move to end of workflow


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


    print = function() {
      taskOrder <- list(
        "Task 1" = self$populationSimulation$print(),
        "Task 2" = self$pkParametersCalculation$print(),
        "Task 3" = self$sensitivityAnalysis$print(),
        "Task 4" = self$demographyPlot$print(),
        "Task 5" = self$gofPlot$print(),
        "Task 6" = self$pkParametersPlot$print(),
        "Task 7" = self$sensitivityPlot$print()
      )
      invisible(self)

      return(taskOrder)
    }
  )
)
