#' @title PopulationWorkflow
#' @docType class
#' @description  Population Workflow Task for Reporting Engine
#' @field populationSimulation Population simulation task
#' @field pkParametersCalculation PK parameters calculation task
#' @field sensitivityAnalysis Sensitivity analysis task
#' @field demographyPlot Plot demography task
#' @field timeProfilePlot Plot time profile task
#' @field pkParametersPlot Plot PK parameters task
#' @field sensitivityPlot Plot sensitiviy task
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize reporting engine population workflow}
#' \item{setPopulationSimulationSettings()}{Define population simulation task settings}
#' \item{setPlotDemographySettings()}{Define demography plot settings}
#' \item{setPlotTimeProfileSettings()}{Define time profile plot settings}

#' \item{runWorkflow()}{Run the active tasks of population worklfow}
#' }
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
PopulationWorkflow <- R6::R6Class(
  "PopulationWorkflow",
  inherit = Workflow,

  public = list(
    populationSimulation = NULL,
    populationSensitivityAnalysis = NULL,

    initialize = function(...) {
      super$initialize(...)

      # if (!is.null(numberOfCores)) {
      #   validateIsInteger(numberOfCores)
      #   self$numberOfCores <- numberOfCores
      # }

      # self$setPopulationSimulationSettings()
      # self$setPKParametersCalculationSettings()
      # self$setSensitivityAnalysisSettings()

      # self$setDemographyPlotSettings()
      # self$setGofPlotSettings()
      # self$setpkParametersPlotSettings()
      # self$setSensitivityPlotSettings()
    },


    setPopulationSimulationSettings = function(input = NULL,
                                               output = NULL,
                                               settings = NULL,
                                               active = TRUE,
                                               message = NULL,
                                               inputFolderName = self$inputFolder,
                                               simulationFileName = self$simulation,
                                               populationFileName = self$population,
                                               resultsFolderName = self$simulationFolder,
                                               resultsFileName = "populationSimulation",
                                               numberOfCores = 1,
                                               calculatePKParameters = TRUE,
                                               PKParametersFolderName = file.path(self$outputFolder),
                                               PKParametersFileName = "populationPKParameters") {
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
        numberOfCores = numberOfCores,
        calculatePKParameters = calculatePKParameters,
        PKParametersFolderName = PKParametersFolderName,
        PKParametersFileName = PKParametersFileName
      )
    },

    # setPopulationSimulationSettings = function(input = NULL,
    #                                            output = NULL,
    #                                            active = TRUE,
    #                                            message = NULL) {
    #   self$populationSimulation <- Task$new(
    #     input = input %||% list(
    #       "population" = file.path(self$inputFolder, paste0(self$population, ".csv")),
    #       "simulation" = file.path(self$inputFolder, paste0(self$simulation, ".pkml"))
    #     ),
    #     #output = output %||% list("populationSimulation" = file.path(self$simulationFolder, "populationSimulation.RData")),
    #     active = active,
    #     message = message %||% "Simulate population"
    #   )
    # },







    # setDemographyPlotSettings = function(input = NULL,
    #                                          output = NULL,
    #                                          active = TRUE,
    #                                          message = NULL) {
    #   self$demographyPlot <- Task$new(
    #     input = input %||% self$populationSimulation$input,
    #     output = output %||% list(
    #       "demographyResults" = file.path(self$simulationFolder, "demography.RData"),
    #       "demographyPlot" = file.path(self$outputFolder, "demographyPlot"),
    #       "demographyTable" = file.path(self$outputFolder, "demographyTable.md")
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
    #       "gofPlot" = file.path(self$outputFolder, "gofPlot")
    #     ),
    #     active = active,
    #     message = message %||% "Plot goodness of fit diagnostics"
    #   )
    # },

    runWorkflow = function() {


      # POPULATION WORKFLOW

      # CORE STAGE 0:  DEMOGRAPHY SUMMARY
      # 0a - Demography Plots/Tables

      # CORE STAGE 1:  SIMULATION
      # 1a - Time profile Plot

      # CORE STAGE 2:  CALCULATE PK PARAMETER
      # 2a - PK parameter Plot

      # CORE STAGE 3:  CALCULATE SENSITIVITY
      # 3a - Plots and Tables based on sensitivity results



      # #Example:
      # library(ospsuite)
      # library(ospsuite.reportingengine)
      # simfile <- "./data/simpleMobiEventSim.pkml"
      # popfile <- "./data/popData.csv"
      # pwf <- PopulationWorkflow$new(simulationFile = simfile,
      #                               populationFile = popfile,
      #                               numberOfCores = 3)
      # res<-pwf$runWorkflow()


      print("Start of population workflow: ")
      print(self$reportingEngineInfo)


      # wdir <- self$workflowFolder
      # resultsFileName <- "populationSimulationResults"

      if (self$populationSimulation$active) {
        if (self$populationSimulation$validateInput()) {
          if (self$populationSimulation$numberOfCores == 1) {
            print("Starting population simulation")
            resultsFilePath <- file.path(self$populationSimulation$resultsFolderName, paste0(self$populationSimulation$resultsFileName, ".csv"))
            simulateModel(
              simFilePath = file.path(self$populationSimulation$inputFolderName, paste0(self$populationSimulation$simulationFileName, ".pkml")),
              popDataFilePath = file.path(self$populationSimulation$inputFolderName, paste0(self$populationSimulation$populationFileName, ".csv")),
              resultsFilePath = resultsFilePath,
              calculatePKParameters = self$populationSimulation$calculatePKParameters,
              PKParametersFilePath = file.path(self$populationSimulation$PKParametersFolderName, paste0(self$populationSimulation$PKParametersFileName, ".csv"))
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
              resultsFileName = self$populationSimulation$resultsFileName,
              calculatePKParameters = self$populationSimulation$calculatePKParameters,
              PKParametersFolderName = self$populationSimulation$PKParametersFolderName,
              PKParametersFileName = self$populationSimulation$PKParametersFileName
            )
          }
        }
      }
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
