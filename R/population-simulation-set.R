#' @title PopulationSimulationSet
#' @description R6 class representing Reporting Engine Population Model Set
#' @field referencePopulation logical for reference population used in Pediatric and Ratio Comparison workflows
#' @field populationFile name of csv file to be used for the population
#' @field populationName display name of population
#' @export
PopulationSimulationSet <- R6::R6Class(
  "PopulationSimulationSet",
  inherit = SimulationSet,
  public = list(
    referencePopulation = NULL,
    populationFile = NULL,
    populationName = NULL,

    #' @description
    #' Create a new `PopulationSimulationSet` object.
    #' @param referencePopulation logical for reference population used in Pediatric and Ratio Comparison workflows
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param populationFile name of csv file to be used for the population
    #' @param populationName display name of population
    #' @param ... inputs inherited from `SimulationSet`
    #' @return A new `PopulationSimulationSet` object
    initialize = function(referencePopulation = FALSE,
                              simulationSetName,
                              simulationFile,
                              populationFile,
                              populationName = NULL,
                              ...) {
      validateIsLogical(referencePopulation)
      validateIsString(c(simulationSetName, simulationFile, populationFile))
      validateIsString(populationName, nullAllowed = TRUE)
      validateIsFileExtension(populationFile, "csv")

      super$initialize(
        simulationSetName = simulationSetName,
        simulationFile = simulationFile,
        ...
      )

      self$referencePopulation <- referencePopulation
      self$populationFile <- populationFile
      self$populationName <- populationName %||% trimFileName(populationFile, extension = "csv")
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
