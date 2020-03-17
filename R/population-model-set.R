#' @title PopulationSimulationSet
#' @description R6 class representing Reporting Engine Population Model Set
#' @field simulationFile name of pkml file to be used for the simulation
#' @field simulationName display name of simulation
#' @field pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field pathName display name for `pathID`
#' @field pathUnit display unit for `pathID`
#' @field pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
#' @field pkParametersNames display names for `pkParameters`
#' @field pkParametersUnits display units for `pkParameters`
#' @field dataFilter filter to compare with observed data
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
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param populationFile names of pkml file to be used for the population
    #' @param populationName display name of population
    #' @param pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param pathName display name for `pathID`
    #' @param pathUnit display unit for `pathID`
    #' @param pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
    #' Default value is obtained from enum(ospsuite::allPKParameterNames()).
    #' @param pkParametersNames display names for `pkParameters`
    #' @param pkParametersUnits display units for `pkParameters`
    #' @param dataFilter filter to compare with observed data
    #' @return A new `SimulationSet` object
    #'

    initialize = function(simulationSetName = NULL,
                              simulationFile,
                              simulationName = NULL,
                              pathID = NULL,
                              pathName = NULL,
                              pathUnit = NULL,
                              pkParameters = NULL,
                              pkParametersNames = NULL,
                              pkParametersUnits = NULL,
                              dataFilter = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              populationFile,
                              populationName = NULL) {
      super$initialize(
        simulationSetName = NULL,
        simulationFile,
        simulationName = NULL,
        pathID = NULL,
        pathName = NULL,
        pathUnit = NULL,
        pkParameters = enum(ospsuite::allPKParameterNames()),
        pkParametersNames = NULL,
        pkParametersUnits = NULL,
        dataFilter = NULL,
        observedDataFile = NULL,
        observedMetaDataFile = NULL
      )
      self$populationFile <- populationFile
      self$populationName <- populationName %||% trimFileName(populationFile, extension = "csv")
      self$simulationSetName <- simulationSetName %||% paste(self$simulationName, self$populationName, sep = "-")
    },

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
