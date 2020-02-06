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
    meanModelPKParameters = NULL,
    meanModelSensitivityAnalysis = NULL,

    initialize = function(...) {
      super$initialize(...)


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
                                              inputFolderName = self$inputFolder,
                                              simulationFileName = self$simulation,
                                              populationFileName = NULL,
                                              resultsFolderName = self$simulationFolder,
                                              resultsFileName = "meanModelSimulation") {
      self$meanModelSimulation <- SimulationTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Simulate mean model",
        inputFolderName = inputFolderName,
        simulationFileName = simulationFileName,
        populationFileName = populationFileName,
        resultsFolderName = resultsFolderName,
        resultsFileName = resultsFileName
      )
    },

    setMeanModelPKParameterSettings = function(input = NULL,
                                              output = NULL,
                                              settings = NULL,
                                              active = TRUE,
                                              message = NULL,
                                              simulationFilePath = file.path(self$inputFolder,paste0(self$simulation,".pkml")),
                                              simulationResultFilePaths = self$meanModelSimulation$generatedResultFileNames,
                                              pkParametersToEvaluate = NULL,
                                              userDefinedPKFunctions = NULL,
                                              pkParameterResultsFilePath = file.path(self$pkParametersFolder,"meanModelPKParameters.csv")){
      self$meanModelPKParameters <- CalculatePKParametersTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Calculate mean model PK parameters",
        simulationFilePath = simulationFilePath,
        simulationResultFilePaths = simulationResultFilePaths,
        pkParametersToEvaluate = pkParametersToEvaluate,
        userDefinedPKFunctions = userDefinedPKFunctions,
        pkParameterResultsFilePath = pkParameterResultsFilePath)
    },

    setMeanModelSensitivityAnalysisSettings = function(input = NULL,
                                                       output = NULL,
                                                       settings = NULL,
                                                       active = TRUE,
                                                       message = NULL,
                                                       inputFolderName = self$inputFolder,
                                                       simulationFileName = self$simulation,
                                                       populationFileName = NULL,
                                                       resultsFolderName = self$sensitivityFolder,
                                                       resultsFileName = "meanModelSensitivityAnalysis",
                                                       numberOfCores = 1) {
      self$meanModelSensitivityAnalysis <- SensitivityAnalysisTask$new(
        input = input,
        output = output,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for mean model",
        inputFolderName = inputFolderName,
        simulationFileName = simulationFileName,
        populationFileName = populationFileName,
        resultsFolderName = resultsFolderName,
        resultsFileName = resultsFileName,
        numberOfCores = numberOfCores
      )
    },


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


      # MEAN MODEL WORKFLOW
      # CORE STAGE 1:  SIMULATION
      # 1a - Mass Balance Plot
      # 1b - Time profile Plot
      # 1c - Absorption Plot

      # CORE STAGE 2:  CALCULATE PK PARAMETER
      # 2a - PK parameter Plot

      # CORE STAGE 3:  CALCULATE SENSITIVITY
      # 3a - Plots and Tables based on sensitivity results




      print("Start of mean model workflow: ")
      print(self$reportingEngineInfo)



      if (self$meanModelSimulation$active) {
        if (self$meanModelSimulation$validateInput()) {
          print("Starting mean model simulation")
            self$meanModelSimulation$generatedResultFileNames <-simulateModel(
            simFilePath = file.path(self$meanModelSimulation$inputFolderName, paste0(self$meanModelSimulation$simulationFileName, ".pkml")),
            resultsFilePath = file.path(self$meanModelSimulation$resultsFolderName, paste0(self$meanModelSimulation$resultsFileName, ".csv")))
        }
      }


      if (self$meanModelPKParameters$active) {
        if (self$meanModelPKParameters$validateInput()) {
          print("Starting mean model PK parameters calculation")
          self$meanModelPKParameters$generatedResultFileNames <- calculatePKParameters(
            simulationFilePath = self$meanModelPKParameters$simulationFilePath,
            simulationResultFilePaths = self$meanModelSimulation$generatedResultFileNames,
            pkParameterResultsFilePath = self$meanModelPKParameters$pkParameterResultsFilePath)
        }
      }


      if (self$meanModelSensitivityAnalysis$active) {
        if (self$meanModelSensitivityAnalysis$validateInput()) {
          print("Starting mean model sensitivity analysis")
          self$meanModelSensitivityAnalysis$generatedResultFileNames <- analyzeSensitivity(
            simFilePath = file.path(self$meanModelSensitivityAnalysis$inputFolderName, paste0(self$meanModelSensitivityAnalysis$simulationFileName, ".pkml")),
            resultsFileFolder = file.path(self$meanModelSensitivityAnalysis$resultsFolderName),
            resultsFileName = self$meanModelSensitivityAnalysis$resultsFileName,
            numberOfCores = self$meanModelSensitivityAnalysis$numberOfCores
          )
        }
      }
    },



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
