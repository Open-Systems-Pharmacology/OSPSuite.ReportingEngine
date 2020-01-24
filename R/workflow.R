#' @title Workflow
#' @docType class
#' @description  Workflow Task for Reporting Engine
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field settings setting object
#' @field models simulation class object
#' @field population population class object
#' @field observedData List of observed data (use Nonmem format ?)
#' @field outputFolder path where output is saved
#' #' @section Methods:
#' #' \describe{
#' \item{setPKParametersCalculationSettings()}{Define PK parameters calculation task settings}
#' \item{setSensitivityAnalysisSettings()}{Define sensitivity analysis task settings}
#' \item{setpkParametersPlotSettings()}{Define PK parameters plot settings}
#' \item{setSensitivityPlotSettings()}{Define sensitivity plot settings}
#' }
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
    sensitivityFolder = NULL,
    outputFolder = NULL,

    initialize = function(simulationFile,
                              populationFile,
                              observedDataFile = NULL,
                              workflowFolder = paste0("Workflow", "_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")),
                              inputFolder = "Inputs",
                              simulationFolder = "Simulations",
                              sensitivityFolder = "Sensitivities",
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
      self$sensitivityFolder <- file.path(workflowFolder, sensitivityFolder)
      self$outputFolder <- file.path(workflowFolder, outputFolder)

      dir.create(self$inputFolder)
      dir.create(self$simulationFolder)
      dir.create(self$sensitivityFolder)
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
    },

    setPKParametersCalculationSettings = function(message = NULL) {
      self$pkParametersCalculation <- Task$new(message = message %||% "Calculate PK parameters")
    },

    setSensitivityAnalysisSettings = function(input = NULL,
                                                  output = NULL,
                                                  active = TRUE,
                                                  message = NULL) {
      self$sensitivityAnalysis <- Task$new(
        input = input %||% list(
          "simulation" = file.path(self$inputFolder, paste0(self$simulation, ".pkml"))
        ),
        output = output %||% list("sensitivityAnalysis" = file.path(self$sensitivityFolder, "sensitivityAnalysis.RData")),
        active = active,
        message = message %||% "Analyze sensitivity"
      )
    },

    setpkParametersPlotSettings = function(message = NULL) {
      self$pkParametersPlot <- Task$new(message = message %||% "Plot PK parameters")
    },

    setSensitivityPlotSettings = function(input = NULL,
                                              output = NULL,
                                              active = TRUE,
                                              message = NULL) {
      self$sensitivityPlot <- Task$new(
        input = input %||% list(
          "sensitivityAnalysis" = self$sensitivityAnalysis$output$sensitivityAnalysis
        ),
        output = output %||% list("sensitivityPlot" = file.path(self$outputFolder, "sensitivityPlot.png")),
        active = active,
        message = message %||% "Plot sensitivity analysis"
      )
    }
  )
)
