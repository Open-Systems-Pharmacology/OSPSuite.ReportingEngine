#' @title SensitivityAnalysisTask
#' @docType class
#' @description  Sensitivity analysis task settings for Reporting Engine
#' @field resultFileName string with not file extension for.  Name of CSV file to which simulation results will be saved.
#' @section Methods:
#' \describe{
#' \item{new()}{Initilialize Task settings}
#' \item{activate()}{Set Task as active in workflow}
#' \item{inactivate()}{Set Task as inactive in workflow}
#' \item{print()}{Show task settings}
#' }
#' @format NULL
SensitivityAnalysisTask <- R6::R6Class(
  "SensitivityAnalysisTask",
  inherit = Task,
  public = list(
    simulationFilePath = NULL,
    populationFilePath = NULL,
    resultFileName = "simulationResults",
    numberOfCores = 1,
    initialize = function(simulationFilePath,
                          populationFilePath,
                          resultFileName,
                          ...) {
      super$initialize(...)

      self$simulationFilePath = simulationFilePath
      self$populationFilePath = populationFilePath
      self$resultFileName = resultFileName

    }
  )
)
