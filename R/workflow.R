#' @title Workflow
#' @docType class
#' @description  Workflow Task for Reporting Engine
#' @field settings setting object
#' @field models simulation class object
#' @field population population class object
#' @field observedData List of observed data (use Nonmem format ?)
#' @field outputFolder path where output is saved
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    settings = NULL,
    models = NULL,
    population = NULL,
    observedData = NULL,
    outputFolder = NULL,
    initialize = function(outputFolder = NULL,
                          simulationPath = NULL,
                          populationPath = NULL,
                          observationPath = NULL,
                          settings = NULL){

      if(!is.null(outputFolder)){
        dir.create(outputFolder)
        }
      # Get current folder if output folder left empty
      self$outputFolder <- outputFolder %||% getwd()

      self$settings <- settings
      self$models <- ifnotnull(simulationPath, loadSimulation(simulationPath))
      self$population <- ifnotnull(populationPath, loadPopulation(populationPath))
      self$observedData <- ifnotnull(observationPath, read.table(observationPath))
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
    sensitivityPlot =NULL,

    initialize = function(...){
      super$initialize(...)

      self$setPopulationSimulationSettings()
      self$setPKParametersCalculationSettings()
      self$setSensitivityAnalysisSettings()

      self$setDemographyPlotSettings()
      self$setGofPlotSettings()
      self$setpkParametersPlotSettings()
      self$setSensitivityPlotSettings()
    },

    setPopulationSimulationSettings = function(populationsList = NULL,
                                               active = TRUE,
                                               message = NULL){
      self$populationSimulation <- Task$new(input = populationsList %||% self$population,
                                            active = active,
                                            message = message %||% "Simulate population")
    },
    setPKParametersCalculationSettings = function(message = NULL){
      self$pkParametersCalculation <- Task$new(message = message %||% "Calculate PK parameters")
    },
    setSensitivityAnalysisSettings = function(message = NULL){
      self$sensitivityAnalysis <- Task$new(message = message %||% "Analyze sensitivity")
    },
    setDemographyPlotSettings = function(message = NULL){
      self$demographyPlot <- Task$new(message = message %||% "Plot demography")
    },
    setGofPlotSettings = function(message = NULL){
      self$gofPlot <- Task$new(message = message %||% "Plot goodness of fit diagnostics")
    },
    setpkParametersPlotSettings = function(message = NULL){
      self$pkParametersPlot <- Task$new(message = message %||% "Plot PK parameters")
    },
    setSensitivityPlotSettings = function(message = NULL){
      self$sensitivityPlot <- Task$new(message = message %||% "Plot sensitivity analysis")
    },

    runWorkflow = function(){
      if(self$populationSimulation$active){
        self$populationSimulation$output <- runSimulation(self$models, self$population)
      }
      if(self$pkParametersCalculation$active){
        self$pkParametersCalculation$output <- calculatePKParameters()
      }
      if(self$sensitivityAnalysis$active){
        self$sensitivityAnalysis$output <- runSensitivityAnalysis()
      }
      if(self$demographyPlot$active){
        # The last properties of plotDemograpy will be set within task settings
        self$demographyPlot$output <- plotDemography(simulation = self$models,
                                                     population = self$population,
                                                     parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height),
                                                     plotConfiguration = NULL)
        demographyPlot <- self$demographyPlot$output
        save(demographyPlot, file = file.path(self$outputFolder, 'demographyPlot.RData'))
      }
      if(self$gofPlot$active){
        self$gofPlot$output <- plotGoodnessOfFit(self$populationSimulation$output,
                                                 self$population,
                                                 self$observedData,
                                                 quantity = NULL,
                                                 plotConfiguration = NULL)
        gofPlot <- self$gofPlot$output
        save(gofPlot, file = file.path(self$outputFolder, 'gofPlot.RData'))
      }
      if(self$pkParametersPlot$active){
        self$pkParametersPlot$output <- plotPKParameters()
      }
      if(self$sensitivityPlot$active){
        self$sensitivityPlot$output <- plotSensitivity()
      }
    },

    print = function(){
      tasks <- c(self$populationSimulation,
                 self$pkParametersCalculation,
                 self$sensitivityAnalysis,
                 self$demographyPlot,
                 self$gofPlot,
                 self$pkParametersPlot,
                 self$sensitivityPlot)

      message <- data.frame("task"=NULL,
                            "active"=NULL,
                            "available ouptut" = NULL)
      for (task in tasks){
        message <- rbind.data.frame(message,
                                    data.frame("task"=task$message,
                                               "active"=task$active,
                                               "available ouptut" = !is.null(task$output)))
      }
      print(message)
      return(message)
    }
  )
)
