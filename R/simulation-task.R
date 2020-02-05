#' @title SimulationTask
#' @docType class
#' @description  Simulation task settings for Reporting Engine
#' @field resultsFileName string with not file extension for.  Name of CSV file to which simulation results will be saved.
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize Task settings}
#' \item{activate()}{Set Task as active in workflow}
#' \item{inactivate()}{Set Task as inactive in workflow}
#' \item{print()}{Show task settings}
#' }
#' @format NULL
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    inputFolderName = NULL,
    simulationFileName = NULL,
    populationFileName = NULL,
    resultsFolderName = NULL,
    resultsFileName = NULL,
    numberOfCores = 1,
    generatedResultFileNames = NULL,
    initialize = function(inputFolderName = getwd(),
                          simulationFileName,
                          populationFileName = NULL,
                          resultsFolderName = getwd(),
                          resultsFileName = "simulationResults",
                          numberOfCores = 1,
                          ...) {
      super$initialize(...)
      self$inputFolderName <- inputFolderName
      self$simulationFileName <- simulationFileName
      self$populationFileName <- populationFileName
      self$resultsFolderName <- resultsFolderName
      self$resultsFileName <- resultsFileName
      self$numberOfCores <- numberOfCores
    }
  )
)
