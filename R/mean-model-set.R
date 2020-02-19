#' #' @title MeanModelSet
#' #' @description R6 class representing Reporting Engine Mean Model Set
#' #' @field simulationFile names of pkml file to be used for the simulation
#' #' @field simulationName display name of simulation
#' #' @field pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' #' @field pathName display name for `pathID`
#' #' @field pathUnit display unit for `pathID`
#' #' @field pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
#' #' @field pkParametersNames display names for `pkParameters`
#' #' @field pkParametersUnits display units for `pkParameters`
#' #' @field dataFilter filter to compare with observed data
#' #' @export
MeanModelSet <- R6::R6Class(
  "MeanModelSet",
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
    dataFilter = NULL,
    observedDataFile = NULL,
    observedMetaDataFile = NULL,
    # inputFilesFolder = NULL,
    # simulationResultsFolder = NULL,
    # simulationResultFileNames = NULL,
    # pkAnalysisResultsFolder = NULL,
    # pkAnalysisResultsFileNames = NULL,
    # sensitivityAnalysisResultsFolder = NULL,
    # sensitivityAnalysisResultsFileNames = NULL,
    #'     #' @description
    #'     #' Create a new `MeanModelSet` object.
    #'     #' @param simulationFile names of pkml file to be used for the simulation
    #'     #' @param simulationName display name of simulation
    #'     #' @param pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #'     #' @param pathName display name for `pathID`
    #'     #' @param pathUnit display unit for `pathID`
    #'     #' @param pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
    #'     #' Default value is enum `AllPKParameters`.
    #'     #' @param pkParametersNames display names for `pkParameters`
    #'     #' @param pkParametersUnits display units for `pkParameters`
    #'     #' @param dataFilter filter to compare with observed data
    #'     #' @return A new `MeanModelSet` object
    initialize = function(simulationSetName = NULL,
                          simulationFile,
                          simulationName = NULL,
                          pathID = NULL,
                          pathName = NULL,
                          pathUnit = NULL,
                          pkParameters = AllPKParameters,
                          pkParametersNames = NULL,
                          pkParametersUnits = NULL,
                          dataFilter = NULL,
                          observedDataFile = NULL,
                          observedMetaDataFile = NULL) {
      self$simulationFile <- simulationFile
      self$simulationName <- simulationName %||% trimFileName(simulationFile, extension = "pkml")

      self$simulationSetName <- simulationSetName %||% self$simulationName

      self$pathID <- pathID
      self$pathName <- pathName %||% pathID
      self$pathUnit <- pathUnit

      self$pkParameters <- pkParameters
      self$pkParametersNames <- pkParametersNames %||% pkParameters
      self$pkParametersUnits <- pkParametersUnits

      self$dataFilter <- dataFilter

      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile
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
