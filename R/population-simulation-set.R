#' @title PopulationSimulationSet
#' @description R6 class representing Reporting Engine Population Model Set
#' @field referencePopulation logical for reference population used in Pediatric and Ratio Comparison workflows
#' @field populationFile name of csv file to be used for the population
#' @field populationName display name of population
#' @field studyDesignFile name of study design csv file
#' @field plotReferenceObsData logical for plotting reference observed data in Pediatric and Ratio Comparison workflows
#' @export
#' @import ospsuite.utils
PopulationSimulationSet <- R6::R6Class(
  "PopulationSimulationSet",
  inherit = SimulationSet,
  public = list(
    referencePopulation = NULL,
    populationFile = NULL,
    populationName = NULL,
    studyDesignFile = NULL,
    plotReferenceObsData = NULL,

    #' @description
    #' Create a new `PopulationSimulationSet` object.
    #' @param referencePopulation logical for reference population used in Pediatric and Ratio Comparison workflows
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param populationFile name of csv file to be used for the population
    #' @param populationName display name of population
    #' @param studyDesignFile name of study design csv file
    #' @param plotReferenceObsData logical for plotting reference observed data in Pediatric and Ratio Comparison workflows
    #' @param ... inputs inherited from `SimulationSet`
    #' @return A new `PopulationSimulationSet` object
    initialize = function(referencePopulation = FALSE,
                          simulationSetName,
                          simulationFile,
                          populationFile,
                          populationName = NULL,
                          studyDesignFile = NULL,
                          plotReferenceObsData = FALSE,
                          ...) {
      validateIsLogical(referencePopulation)
      validateIsLogical(plotReferenceObsData)
      validateIsString(simulationSetName)
      validateIsString(simulationFile)
      validateIsString(populationFile)
      validateIsString(c(populationName, studyDesignFile), nullAllowed = TRUE)
      validateIsFileExtension(populationFile, "csv")

      super$initialize(
        simulationSetName = simulationSetName,
        simulationFile = simulationFile,
        ...
      )

      self$referencePopulation <- referencePopulation
      self$plotReferenceObsData <- plotReferenceObsData
      self$populationFile <- populationFile
      self$populationName <- populationName %||% trimFileName(populationFile, extension = "csv")
      self$studyDesignFile <- studyDesignFile
    },

    #' @description
    #' Copy input files into a simulation set specific folder
    #' @param inputFilesFolder where input are located
    copyInputFiles = function(inputFilesFolder) {
      if (!is.null(self$simulationFile)) {
        file.copy(self$simulationFile, file.path(inputFilesFolder, paste0(self$simulationSetName, ".pkml")))
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
