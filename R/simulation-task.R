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
    simulationFilePath = NULL,
    populationFilePath = NULL,
    resultsFolderName = NULL,
    resultsFileName = NULL,
    numberOfCores = 1,
    calculatePKParameters = FALSE,
    PKParametersFolderName = NULL,
    PKParametersFileName = NULL,
    initialize = function(simulationFilePath,
                          populationFilePath = NULL,
                          resultsFolderName = getwd(),
                          resultsFileName="simulationResults",
                          calculatePKParameters = FALSE,
                          PKParametersFolderName=getwd(),
                          PKParametersFileName="PKParameters",
                          ...) {
      super$initialize(...)
      self$simulationFilePath = simulationFilePath
      self$populationFilePath = populationFilePath
      self$resultsFolderName = resultsFolderName
      self$resultsFileName = resultsFileName
      self$calculatePKParameters = calculatePKParameters
      self$PKParametersFolderName = PKParametersFolderName
      self$PKParametersFileName = PKParametersFileName
    }
  )
)
