#' @title Workflow
#' @docType class
#' @description  Workflow Task for Reporting Engine
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field settings setting object
#' @field models simulation class object
#' @field population population class object
#' @field observedData List of observed data (use Nonmem format ?)
#' @field outputFolder path where output is saved
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    reportingEngineInfo = ReportingEngineInfo$new(),
    settings = NULL,
    simulation = NULL,
    population = NULL,
    observedData = NULL,
    workflowFolder = NULL,
    inputFolder = NULL,
    simulationFolder = NULL,
    sensitvityFolder = NULL,
    outputFolder = NULL,

    initialize = function(simulationFile,
                              populationFile,
                              observedDataFile = NULL,
                              workflowFolder = paste0("Workflow", Sys.Date()),
                              inputFolder = "Inputs",
                              simulationFolder = "Simulations",
                              sensitvityFolder = "Sensitivities",
                              outputFolder = "Outputs",
                              settings = NULL) {


      # Check of Workflow inputs
      validateIsOfType(simulationFile, "character")
      validateIsOfType(populationFile, "character")
      validateIsSameLength(simulationFile, populationFile)

      validateIsFileExtension(simulationFile, "pkml")
      validateIsFileExtension(populationFile, "csv")
      validateIsFileExtension(observedDataFile, "csv", nullAllowed = TRUE)

      # Create folder and branches where reporting engine structure will be saved
      if (!is.null(workflowFolder)) {
        dir.create(workflowFolder)
      }
      # Null workflow folder is assumed to be current folder
      self$workflowFolder <- workflowFolder %||% getwd()
      self$inputFolder <- file.path(workflowFolder, inputFolder)
      self$simulationFolder <- file.path(workflowFolder, simulationFolder)
      self$sensitvityFolder <- file.path(workflowFolder, sensitvityFolder)
      self$outputFolder <- file.path(workflowFolder, outputFolder)

      dir.create(self$inputFolder)
      dir.create(self$simulationFolder)
      dir.create(self$sensitvityFolder)
      dir.create(self$outputFolder)

      # Workflow inputs:
      simulationName <- trimFileName(simulationFile, extension = "pkml")
      populationName <- trimFileName(populationFile, extension = "csv")

      file.copy(
        simulationFile,
        file.path(self$inputFolder, paste0(simulationName, ".pkml"))
      )
      self$simulation <- simulationName

      file.copy(
        populationFile,
        file.path(self$inputFolder, paste0(populationName, ".csv"))
      )
      self$population <- populationName

      if (!is.null(observedDataFile)) {
        observedDataName <- trimFileName(observedDataFile, extension = "csv")
        file.copy(
          observedDataFile,
          file.path(self$inputFolder, paste0(observedDataName, ".csv"))
        )
        self$observedData <- observedDataName
      }

      # In case settings need to be defined later on
      self$settings <- settings
    }
  )
)

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
#' \item{setPKParametersCalculationSettings()}{Define PK parameters calculation task settings}
#' \item{setSensitivityAnalysisSettings()}{Define sensitivity analysis task settings}
#' \item{setPlotDemographySettings()}{Define demography plots settings}
#' \item{setPlotTimeProfileSettings()}{Define goodness of fit plots settings}
#' \item{setPlotPKParametersSettings()}{Define PK parameters plot settings}
#' \item{setPlotSensitivitySettings()}{Define sensitivity plot settings}
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

    initialize = function(...) {
      super$initialize(...)

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
    setPKParametersCalculationSettings = function(message = NULL) {
      self$pkParametersCalculation <- Task$new(message = message %||% "Calculate PK parameters")
    },
    setSensitivityAnalysisSettings = function(message = NULL) {
      self$sensitivityAnalysis <- Task$new(message = message %||% "Analyze sensitivity")
    },
    setDemographyPlotSettings = function(input = NULL,
                                             output = NULL,
                                             active = TRUE,
                                             message = NULL) {
      self$demographyPlot <- Task$new(
        input = input %||% list(
          "population" = file.path(self$inputFolder, paste0(self$population, ".csv")),
          "simulation" = file.path(self$inputFolder, paste0(self$simulation, ".pkml"))
        ),
        output = output %||% list(
          "demographyResults" = file.path(self$simulationFolder, "demography.RData"),
          "demographyPlot" = file.path(self$outputFolder, "demographyPlot.png"),
          "demographyTable" = file.path(self$outputFolder, "demographyTable.md")
        ),
        active = active,
        message = message %||% "Plot demography"
      )
    },
    setGofPlotSettings = function(message = NULL) {
      self$gofPlot <- Task$new(message = message %||% "Plot goodness of fit diagnostics")
    },
    setpkParametersPlotSettings = function(message = NULL) {
      self$pkParametersPlot <- Task$new(message = message %||% "Plot PK parameters")
    },
    setSensitivityPlotSettings = function(message = NULL) {
      self$sensitivityPlot <- Task$new(message = message %||% "Plot sensitivity analysis")
    },

    runWorkflow = function() {
      print("Start of population workflow: ")
      print(self$reportingEngineInfo)

      if (self$populationSimulation$active) {
        if (self$populationSimulation$validateInput()) {
          # These lines can be encompassed within a unit of work funciton
          population <- loadPopulation(self$populationSimulation$input$population)
          simulation <- loadSimulation(self$populationSimulation$input$simulation)

          populationSimulation <- ospsuite::runSimulation(simulation, population)
          save(populationSimulation, file = self$populationSimulation$output$populationSimulation)
        }
      }
      if (self$pkParametersCalculation$active) {
        # if (self$pkParametersCalculation$validateInput()){
        # calculatePKParameters()
        # }
      }
      if (self$sensitivityAnalysis$active) {
        # if (self$sensitivityAnalysis$validateInput()){
        # runSensitivityAnalysis()
        # }
      }
      if (self$demographyPlot$active) {
        if (self$demographyPlot$validateInput()) {
          population <- loadPopulation(self$demographyPlot$input$population)
          simulation <- loadSimulation(self$demographyPlot$input$simulation)

          # The last properties of plotDemograpy will be set within task settings
          demographyPlot <- plotDemography(
            simulation = simulation,
            population = population,
            parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height),
            plotConfiguration = NULL
          )
          save(demographyPlot, file = self$demographyPlot$output$demographyResults)
        }
      }
      if (self$gofPlot$active) {
        self$gofPlot$output <- plotGoodnessOfFit(self$populationSimulation$output,
          self$population,
          self$observedData,
          quantity = NULL,
          plotConfiguration = NULL
        )
        gofPlot <- self$gofPlot$output
        save(gofPlot, file = file.path(self$outputFolder, "gofPlot.RData"))
      }
      if (self$pkParametersPlot$active) {
        self$pkParametersPlot$output <- plotPKParameters()
      }
      if (self$sensitivityPlot$active) {
        self$sensitivityPlot$output <- plotSensitivity()
      }
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

      print(taskOrder)
      return(taskOrder)
    }
  )
)
