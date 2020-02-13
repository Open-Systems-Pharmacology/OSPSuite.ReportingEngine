#' @title SensitivityAnalysisTask
#' @description  R6 class for SensitivityAnalysisTask settings
#' @field inputFolderName input folder 
#' @field simulationFileName name of simulation files
#' @field populationFileName name of population files
#' @field resultsFolderName name of folder where results are saved
#' @field resultsFileName name of file where results are saved
#' @field numberOfCores number of cores for parallel computation
#' @field generatedResultFileNames name of files where PK parameters are saved
SensitivityAnalysisTask <- R6::R6Class(
  "SensitivityAnalysisTask",
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
    #' Create a `SensitivityAnalysisTask` object
    #' @param inputFolderName input folder 
    #' @param simulationFileName name of simulation files
    #' @param populationFileName name of population files
    #' @param resultsFolderName name of folder where results are saved
    #' @param resultsFileName name of file where results are saved
    #' @param numberOfCores number of cores for parallel computation
    #' @param generatedResultFileNames name of files where PK parameters are saved
    #' @param ... parameters inherited from R6 class `Task` object
    #' @return A new `SensitivityAnalysisTask` object
    initialize = function(inputFolderName = getwd(),
                              simulationFileName,
                              populationFileName = NULL,
                              resultsFolderName = getwd(),
                              resultsFileName = "sensitivityAnalysisResults",
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
