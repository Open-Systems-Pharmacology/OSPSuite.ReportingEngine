#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field simulationName display name of simulation
#' @field pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field pathName display name for `pathID`
#' @field pathUnit display unit for `pathID`
#' @field pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
#' @field pkParametersNames display names for `pkParameters`
#' @field pkParametersUnits display units for `pkParameters`
#' @field observedDataFile name of csv file to be used for observed data
#' @field observedMetaDataFile name of csv file to be used as dictionary of the observed data
#' @field dataFilter character or expression used to filter the observed data
#' @field dataReportName display name of the observed data
#' @field timeUnit display unit for time variable
#' @export
SimulationSet <- R6::R6Class(
  "SimulationSet",
  public = list(
    simulationSetName = NULL,
    simulationFile = NULL,
    simulationName = NULL,
    pathID = NULL,
    pathName = NULL,
    pathUnit = NULL,
    pkParameters = NULL,
    pkParametersNames = NULL,
    pkParametersUnits = NULL,
    observedDataFile = NULL,
    observedMetaDataFile = NULL,
    dataFilter = NULL,
    dataReportName = NULL,
    timeUnit = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
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
    #' @param dataReportName display name of the observed data
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName = NULL,
                              simulationFile,
                              simulationName = NULL,
                              pathID = NULL,
                              pathName = NULL,
                              pathUnit = NULL,
                              pkParameters = enum(ospsuite::allPKParameterNames()),
                              pkParametersNames = NULL,
                              pkParametersUnits = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              dataFilter = NULL,
                              dataReportName = NULL,
                              timeUnit = "h") {
      self$simulationFile <- simulationFile
      self$simulationName <- simulationName %||% trimFileName(simulationFile, extension = "pkml")

      self$simulationSetName <- simulationSetName %||% self$simulationName

      self$pathID <- pathID
      self$pathName <- pathName %||% pathID
      self$pathUnit <- pathUnit

      self$pkParameters <- pkParameters
      self$pkParametersNames <- pkParametersNames %||% pkParameters
      self$pkParametersUnits <- pkParametersUnits

      self$timeUnit <- timeUnit %||% "h"

      if (!is.null(observedDataFile) & is.null(observedMetaDataFile)) {

      }
      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile
      self$dataFilter <- dataFilter
      self$dataReportName <- dataReportName %||% paste0(self$pathName, " observed data")
    },

    # createDirectories = function(rootDirectory) {
    #   self$inputFilesFolder <- file.path(rootDirectory, defaultFileNames$inputFolder())
    #   dir.create(self$inputFilesFolder)
    #   logDebug(message = paste0(self$inputFilesFolder, " was successfully created"), printConsole = TRUE)
    #
    #   self$simulationResultsFolder <- file.path(rootDirectory, defaultFileNames$simulationResultsFolder())
    #   dir.create(self$simulationResultsFolder)
    #   logDebug(message = paste0(self$simulationResultsFolder, " was successfully created"), printConsole = TRUE)
    #
    #   self$pkAnalysisResultsFolder <- file.path(rootDirectory, defaultFileNames$pkAnalysisResultsFolder())
    #   dir.create(self$pkAnalysisResultsFolder)
    #   logDebug(message = paste0(self$pkAnalysisResultsFolder, " was successfully created"), printConsole = TRUE)
    #
    #   self$sensitivityAnalysisResultsFolder <- file.path(rootDirectory, defaultFileNames$sensitivityAnalysisResultsFolder())
    #   dir.create(self$sensitivityAnalysisResultsFolder)
    #   logDebug(message = paste0(self$sensitivityAnalysisResultsFolder, " was successfully created"), printConsole = TRUE)
    # },

    #' @description
    #' Create a copy of the input files (pkml simulation and data if available) within a folder dedicated to the simulation set
    #' Rename the input file if `simulationName` is explicitely defined
    #' @param inputFilesFolder path where the files are copied
    copyInputFiles = function(inputFilesFolder) {
      if (!is.null(self$simulationFile)) {
        file.copy(self$simulationFile, file.path(inputFilesFolder, paste0(self$simulationName, ".pkml")))
      }

      if (!is.null(self$observedDataFile)) {
        file.copy(self$observedDataFile, inputFilesFolder)
      }
    }
  )
)
