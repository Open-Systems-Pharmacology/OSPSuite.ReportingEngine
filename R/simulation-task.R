#' @title SimulationTask
#' @description  R6 class for SimulationTask settings
#' @field inputFolderName name of input folder
#' @field simulationFileName name of simulation files
#' @field populationFileName name of population file
#' @field resultsFolderName name of folder where results are saved
#' @field resultsFileName name of file where results are saved
#' @field numberOfCores number of cores for parallel computation
#' @field generatedResultFileNames name of files where PK parameters are saved
SimulationTask <- R6::R6Class(
  "SimulationTask",
  inherit = Task,
  public = list(
    # inputFolderName = NULL,
    # simulationFileName = NULL,
    # populationFileName = NULL,
    # resultsFolderName = NULL,
    # resultsFileName = NULL,
    numberOfCores = NULL,
    # generatedResultFileNames = NULL,

    #' @description
    #' Create a `SimulationTask` object
    #' @param inputFolderName name of input folder
    #' @param simulationFileName name of simulation files
    #' @param populationFileName name of population file
    #' @param resultsFolderName name of folder where results are saved
    #' @param resultsFileName name of file where results are saved
    #' @param numberOfCores number of cores for parallel computation
    #' @param generatedResultFileNames name of files where PK parameters are saved
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SimulationTask` object
    initialize = function(numberOfCores = 1,
                          ...) {
      super$initialize(...)

      if (!is.null(numberOfCores)) {
        validateIsInteger(numberOfCores)
        validateIsOfLength(object = numberOfCores,nbElements = 1)
        self$numberOfCores <- numberOfCores
      }
    }
  )
)
