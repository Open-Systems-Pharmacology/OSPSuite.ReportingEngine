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
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param pathName display name for `pathID`
    #' @param pathUnit display unit for `pathID`
    #' @param pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
    #' Default value is obtained from enum(ospsuite::allPKParameterNames()).
    #' @param pkParametersNames display names for `pkParameters`
    #' @param pkParametersUnits display units for `pkParameters`
    #' @param observedDataFile name of csv file to be used for observed data
    #' @param observedMetaDataFile name of csv file to be used as dictionary of the observed data
    #' @param dataFilter characters or expression to filter the observed data
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
