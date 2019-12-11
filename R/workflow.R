#' @title Workflow
#' @docType class
#' @description  Workflow Task for Reporting Engine
#' @field settings class
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    settings = NULL,
    initialize = function(settings = NULL){
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
#' \item{runPopulationSimulation()}{Simulate population(s)}
#' \item{runPKParametersCalculation(Calculate population PK parameters)}{}
#' \item{runSensitivityAnalysis(Calculate population sensitivity)}{}
#' \item{plotDemography()}{}
#' \item{plotSensitivity()}{}
#' \item{runWorkflow()}{}
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
   
    initialize = function(){
      super$initialize()
      
      self$populationSimulation <- Task$new()
      self$pkParametersCalculation <- Task$new()
      self$sensitivityAnalysis <- Task$new()
      
      self$demographyPlot <- Task$new()
      self$timeProfilePlot <- Task$new()
      self$pkParametersPlot <- Task$new()
      self$sensitivityPlot <- Task$new()
    },
    
    runPopulationSimulation = function(){
      
    },
    runPKParametersCalculation = function(){},
    runSensitivityAnalysis = function(){},
    
    plotDemography = function(simulation,
                              parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height)){
      demographyParameters <- ospsuite::getAllParametersMatching(parameterNames, simulation)
      demographyValues <- as.data.frame(lapply(demographyParameters, function(p) toDisplayUnit(p, pop$getValues(p))))
      names(demographyValues) <- parameterNames
      
      demographyPlot <- lapply(parameterNames, function(x){})
      for (parameterName in parameterNames){
        mapping <- tlf::HistogramDataMapping$new(x = parameterName)
        demographyPlot[[parameterName]] <- plotHistogram(data = demographyValues,
                                                          dataMapping = mapping)
      }
      return(demographyPlot)
    },
    plotTimeProfile = function(){},
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