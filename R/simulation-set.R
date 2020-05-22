#' @title SimulationSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationSetName display name of simulation set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field simulationName display name of simulation
#' @field outputs list of `Output` R6 class objects
#' @field observedDataFile name of csv file to be used for observed data
#' @field observedMetaDataFile name of csv file to be used as dictionary of the observed data
#' @field timeUnit display unit for time variable
#' @export
SimulationSet <- R6::R6Class(
  "SimulationSet",
  public = list(
    simulationSetName = NULL,
    simulationFile = NULL,
    simulationName = NULL,
    outputs = NULL,
    observedDataFile = NULL,
    observedMetaDataFile = NULL,
    timeUnit = NULL,

    #' @description
    #' Create a new `SimulationSet` object.
    #' @param simulationSetName display name of simulation set
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param outputs list of `Output` R6 class objects
    #' @param observedDataFile name of csv file to be used for observed data
    #' @param observedMetaDataFile name of csv file to be used as dictionary of the observed data
    #' @param timeUnit display unit for time variable. Default is "h"
    #' @return A new `SimulationSet` object
    initialize = function(simulationSetName,
                              simulationFile,
                              simulationName = NULL,
                              outputs = NULL,
                              observedDataFile = NULL,
                              observedMetaDataFile = NULL,
                              timeUnit = "h") {
      validateIsString(c(simulationSetName, simulationFile))
      validateIsFileExtension(simulationFile, "pkml")

      # validate is applied to each element of outputs
      validateIsOfType(c(outputs), "Output", nullAllowed = TRUE)

      validateIsString(c(observedDataFile, observedMetaDataFile, timeUnit), nullAllowed = TRUE)
      if (!is.null(observedDataFile)) {
        validateObservedMetaDataFile(observedMetaDataFile, observedDataFile)
      }

      self$simulationSetName <- simulationSetName
      self$simulationFile <- simulationFile
      self$simulationName <- simulationName %||% trimFileName(simulationFile, extension = "pkml")

      self$outputs <- c(outputs)
      self$verifyOutputPathsInSimulation()

      self$observedDataFile <- observedDataFile
      self$observedMetaDataFile <- observedMetaDataFile

      self$timeUnit <- timeUnit %||% "h"
    },

    verifyOutputPathsInSimulation = function(){
      allPathsInOutputs <- sapply(self$outputs,function(x){ x$path })
      sim <- ospsuite::loadSimulation(self$simulationFile)
      ospsuite::addOutputs(quantitiesOrPaths = allPathsInOutputs,
                           simulation = sim)
      loadedOutputPaths <- sapply(sim$outputSelections$allOutputs,function(x){x$path})
      for (pth in allPathsInOutputs){
        if (!(pth %in% loadedOutputPaths)){
          logErrorThenStop(message = messages$invalidOuputPath(pth,self$simulationName))
        }
      }
    }

  )
)
