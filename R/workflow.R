#' @title Workflow
#' @docType class
#' @description  Workflow Task for Reporting Engine
#' @field settings setting object
#' @field models simulation class object
#' @field population population class object
#' @field observedDatasets Nonmem format datasets
#' @field outputFolder path where output is saved
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    settings = NULL,
    models = NULL,
    population = NULL,
    observedDatasets = NULL,
    outputFolder = NULL,
    initialize = function(outputFolder = NULL,
                          simulationPath = NULL,
                          populationPath = NULL,
                          settings = NULL){
      
      if(!is.null(outputFolder)){
        dir.create(outputFolder)
        }
      # Get current folder if output folder left empty
      self$outputFolder <- outputFolder %||% getwd()
      
      self$settings <- settings
      self$models <- ifnotnull(simulationPath, loadSimulation(simulationPath))
      self$population <- ifnotnull(populationPath, loadPopulation(populationPath))
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
#' \item{runPopulationSimulation()}{Simulate population(s)}
#' \item{runPKParametersCalculation(Calculate population PK parameters)}{}
#' \item{runSensitivityAnalysis(Calculate population sensitivity)}{}
#' \item{plotDemography()}{Plot demography}
#' \item{plotTimeProfile()}{Plot time profile, residuals and their distribution}
#' \item{plotPKParameters()}{Plot PK parameters}
#' \item{plotSensitivity()}{Plot sensitivity analysis results}
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
    timeProfilePlot = NULL,
    pkParametersPlot = NULL,
    sensitivityPlot =NULL,
   
    initialize = function(...){
      super$initialize(...)
      
      self$setPopulationSimulationSettings()
      self$setPKParametersCalculationSettings()
      self$setSensitivityAnalysisSettings()
      
      self$setDemographyPlotSettings()
      self$setTimeProfilePlotSettings()
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
    setTimeProfilePlotSettings = function(message = NULL){
      self$timeProfilePlot <- Task$new(message = message %||% "Plot time profile")
    },
    setpkParametersPlotSettings = function(message = NULL){
      self$pkParametersPlot <- Task$new(message = message %||% "Plot PK parameters")
    },
    setSensitivityPlotSettings = function(message = NULL){
      self$sensitivityPlot <- Task$new(message = message %||% "Plot sensitivity analysis")
    },
    
    runPopulationSimulation = function(){
      populationResults <- runSimulation(self$models, self$population)
      self$populationSimulation$output <- populationResults
      save(populationResults, file = file.path(self$outputFolder, 'populationSimulation.Rdata'))
      return(populationResults)
    },
    runPKParametersCalculation = function(){},
    runSensitivityAnalysis = function(){},
    
    plotDemography = function(parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height)){
      demographyParameters <- ospsuite::getAllParametersMatching(parameterNames, self$models)
      demographyValues <- as.data.frame(lapply(demographyParameters, function(p) toDisplayUnit(p, self$population$getValues(p))))
      names(demographyValues) <- parameterNames
      
      demographyPlot <- list()
      for (parameterName in parameterNames){
        mapping <- tlf::HistogramDataMapping$new(x = parameterName)
        demographyPlot[[parameterName]] <- plotHistogram(data = demographyValues,
                                                         dataMapping = mapping,
                                                         bins = 5)
      }
      # TO DO: add plot configuration to generate standadized plot and save format
      save(demographyPlot, file = file.path(self$outputFolder, 'demographyPlot.Rdata'))
      self$demographyPlot$output <- demographyPlot
      return(demographyPlot)
    },
    plotTimeProfile = function(quantity = NULL){
      if(is.null(self$populationSimulation$output)){
        warning("No simulated population, time profile task can't be performed")
        return()
      }
      resultsPaths <- self$populationSimulation$output$allQuantityPaths
      path <- resultsPaths[[1]]
      
      timeProfileResults <- getOutputValuesTLF(self$populationSimulation$output, 
                                               path, 
                                               population = self$population)
      # For this test the default mapping will be last quantity of data
      quantity <- quantity %||% utils::tail(names(timeProfileResults$data), 1)
      
      mapping <- TimeProfileDataMapping$new(x = "Time",
                                            y = quantity)
      timeProfilePlot <- plotTimeProfile(data = timeProfileResults$data,
                                         metaData = timeProfileResults$metaData,
                                         dataMapping = mapping)
      save(timeProfilePlot, file = file.path(self$outputFolder, 'timeProfilePlot.Rdata'))
      self$timeProfilePlot$output <- timeProfilePlot
      return(timeProfilePlot)
    },
    plotPKParameters = function(){},
    plotSensitivity = function(){},
    
    runWorkflow = function(){
      if(self$populationSimulation$active){
        self$runPopulationSimulation()
      }
      if(self$pkParametersCalculation$active){
        self$runPKParametersCalculation()
      }
      if(self$sensitivityAnalysis$active){
        self$runSensitivityAnalysis()
      }
      if(self$demographyPlot$active){
        self$plotDemography()
      }
      if(self$timeProfilePlot$active){
        self$plotTimeProfile()
      }
      if(self$pkParametersPlot$active){
        self$plotPKParameters()
      }
      if(self$sensitivityPlot$active){
        self$plotSensitivity()
      }
    }
  )
)