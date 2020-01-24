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
    pkParametersCalculation = NULL,
    sensitivityAnalysis = NULL,
    demographyPlot = NULL,
    gofPlot = NULL,
    pkParametersPlot = NULL,
    sensitivityPlot = NULL,
    numberOfCores = 1,

    initialize = function(numberOfCores = NULL, ...) {
      super$initialize(...)


      if (!is.null(numberOfCores)) {
        validateIsInteger(numberOfCores)
        self$numberOfCores <- numberOfCores
      }



      self$setPopulationSimulationSettings()
      self$setPKParametersCalculationSettings()
      self$setSensitivityAnalysisSettings()

      self$setDemographyPlotSettings()
      self$setGofPlotSettings()
      self$setpkParametersPlotSettings()
      self$setSensitivityPlotSettings()
    },

    setPopulationSimulationSettings = function(input = NULL,
                                                   output = NULL,
                                                   active = TRUE,
                                                   message = NULL) {
      self$populationSimulation <- Task$new(
        input = input %||% list(
          "population" = file.path(self$inputFolder, paste0(self$population, ".csv")),
          "simulation" = file.path(self$inputFolder, paste0(self$simulation, ".pkml"))
        ),
        output = output %||% list("populationSimulation" = file.path(self$simulationFolder, "populationSimulation.RData")),
        active = active,
        message = message %||% "Simulate population"
      )
    },







    setDemographyPlotSettings = function(input = NULL,
                                             output = NULL,
                                             active = TRUE,
                                             message = NULL) {
      self$demographyPlot <- Task$new(
        input = input %||% self$populationSimulation$input,
        output = output %||% list(
          "demographyResults" = file.path(self$simulationFolder, "demography.RData"),
          "demographyPlot" = file.path(self$outputFolder, "demographyPlot"),
          "demographyTable" = file.path(self$outputFolder, "demographyTable.md")
        ),
        active = active,
        message = message %||% "Plot demography"
      )
    },


    setGofPlotSettings = function(input = NULL,
                                      output = NULL,
                                      active = TRUE,
                                      message = NULL) {
      self$gofPlot <- Task$new(
        input = input %||% list(
          "population" = file.path(self$inputFolder, paste0(self$population, ".csv")),
          "populationSimulation" = self$populationSimulation$output$populationSimulation
        ),
        output = output %||% list(
          "gofResults" = file.path(self$simulationFolder, "gofResults.RData"),
          "gofPlot" = file.path(self$outputFolder, "gofPlot")
        ),
        active = active,
        message = message %||% "Plot goodness of fit diagnostics"
      )
    },

    runWorkflow = function() {

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


      wdir <- getwd()
      inputFolder <- self$inputFolder
      outputFolder <- self$outputFolder
      simFileName <- self$simulation
      popFileName <- self$population
      resultsFileName <- "populationSimulationResults"

      if (self$populationSimulation$active) {
        if (self$populationSimulation$validateInput()) {
          if (self$numberOfCores == 1) {
            print("Starting population simulation")

            simulatePopulation(
              simFileName = paste0(simFileName, ".pkml"),
              simFileFolder = paste0(wdir, "/", inputFolder, "/"),
              popDataFileName = paste0(popFileName, ".csv"),
              popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
              resultFileName = paste0(resultsFileName, ".csv"),
              resultFileFolder = paste0(wdir, "/", outputFolder, "/")
            )
          }
          else if (self$numberOfCores > 1) {
            print("Starting parallel population simulation")
            library("Rmpi")
            mpi.spawn.Rslaves(nslaves = self$numberOfCores)

            # Check that the correct number of slaves has been spawned.
            if (!(mpi.comm.size() - 1 == self$numberOfCores)) { #-1 since mpi.comm.size() counts master
              mpi.close.Rslaves()
              stop(paste0(self$numberOfCores, " cores were not successfully spawned."))
            }
            mpi.bcast.cmd(library("ospsuite"))
            mpi.bcast.cmd(library("ospsuite.reportingengine"))
            tempPopDataFiles <- ospsuite::splitPopulationFile(
              csvPopulationFile = self$populationSimulation$input$population,
              numberOfCores = self$numberOfCores,
              outputFolder = paste0(self$inputFolder, "/"),
              outputFileName = self$population
            )
            mpi.bcast.Robj2slave(obj = simFileName)
            mpi.bcast.Robj2slave(obj = popFileName)
            mpi.bcast.Robj2slave(obj = tempPopDataFiles)
            mpi.bcast.Robj2slave(obj = wdir)
            mpi.bcast.Robj2slave(obj = inputFolder)
            mpi.bcast.Robj2slave(obj = outputFolder)

            mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
              simFileName = paste0(simFileName, ".pkml"),
              simFileFolder = paste0(wdir, "/", inputFolder, "/"),
              popDataFileName = paste0(popFileName, "_", mpi.comm.rank(), ".csv"),
              popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
              resultFileName = paste0(resultsFileName, "_", mpi.comm.rank(), ".csv"),
              resultFileFolder = paste0(wdir, "/", outputFolder, "/")
            ))
            mpi.close.Rslaves() # Move to end of workflow
          }
        }
      }


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
    },

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
