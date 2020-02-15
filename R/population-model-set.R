#' @title PopModelSet
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
PopModelSet <- R6::R6Class(
  "PopModelSet",
  inherit = MeanModelSet,
  public = list(
    populationFile = NULL,
    populationName = NULL,

    #' @description
    #' Create a new `PopModelSet` object.
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param populationFile names of pkml file to be used for the population
    #' @param populationName display name of population
    #' @param pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param pathName display name for `pathID`
    #' @param pathUnit display unit for `pathID`
    #' @param pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
    #' Default value is enum `AllPKParameters`.
    #' @param pkParametersNames display names for `pkParameters`
    #' @param pkParametersUnits display units for `pkParameters`
    #' @param dataFilter filter to compare with observed data
    #' @return A new `MeanModelSet` object
    #'

    initialize = function(...,
                          populationFile,
                          populationName = NULL) {
      super$initialize(...)
      # initialize = function(simulationFile,
      #                           simulationName = NULL,
      #                           populationFile,
      #                           populationName = NULL,
      #                           pathID,
      #                           pathName = NULL,
      #                           pathUnit = NULL,
      #                           pkParameters = AllPKParameters,
      #                           pkParametersNames = NULL,
      #                           pkParametersUnits = NULL,
      #                           dataFilter = NULL) {
      # super$initialize(
      #   simulationFile,
      #   simulationName,
      #   populationFile,
      #   populationName,
      #   pathID,
      #   pathName,
      #   pathUnit,
      #   pkParameters,
      #   pkParametersNames,
      #   pkParametersUnits,
      #   dataFilter
      # )

      self$populationFile <- populationFile
      self$populationName <- populationName %||% trimFileName(populationFile, extension = "csv")

      self$simulationSetName <- simulationSetName %||% paste(simulationName,populationName,sep="-")

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
