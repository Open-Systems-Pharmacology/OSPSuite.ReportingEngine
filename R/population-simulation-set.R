#' @title PopulationSimulationSet
#' @description R6 class representing Reporting Engine Population Model Set
#' @field populationFile name of csv file to be used for the population
#' @field populationName display name of population
#' @export
PopulationSimulationSet <- R6::R6Class(
  "PopulationSimulationSet",
  inherit = SimulationSet,
  public = list(
    populationFile = NULL,
    populationName = NULL,

    #' @description
    #' Create a new `PopulationSimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param ... inputs to SimulationSet constructor
    #' @param populationFile name of csv file to be used for the population
    #' @param populationName display name of population
    #' @return A new `MeanModelSet` object
    initialize = function(simulationSetName = NULL,
                          ...,
                          populationFile,
                          populationName = NULL) {
      super$initialize(...)

      self$populationFile <- populationFile
      self$populationName <- populationName %||% trimFileName(populationFile, extension = "csv")
      self$simulationSetName <- simulationSetName %||% paste(self$simulationName, self$populationName, sep = "-")
    },

    #' @description
    #' Copy input files into a simulation set specific filder
    #' @param inputFilesFolder where input are located
    copyInputFiles = function(inputFilesFolder) {
      if (!is.null(self$simulationFile)) {
        file.copy(self$simulationFile, file.path(inputFilesFolder, paste0(self$simulationName, ".pkml")))
      }

      if (!is.null(self$populationFile)) {
        file.copy(self$populationFile, file.path(inputFilesFolder, paste0(self$populationName, ".csv")))
      }

      if (!is.null(self$observedDataFile)) {
        file.copy(self$observedDataFile, inputFilesFolder)
      }
    }
  )
)
