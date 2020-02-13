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
    inputFolderName = NULL,
    simulationFileName = NULL,
    populationFileName = NULL,
    resultsFolderName = NULL,
    resultsFileName = NULL,
    numberOfCores = 1,
    generatedResultFileNames = NULL,
    
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
