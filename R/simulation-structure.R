#' @title SimulationStructure
#' @description R6 class representing that stores file structure (file names and paths) relating to a particular instance of a simulationSet object
#' @field simulationSet the set for which the SimulationStructure object stores paths input and results
#' @field simulationResultsFolder path to folder storing results of simulations
#' @field pkAnalysisResultsFolder path to folder storing results of pk analyses
#' @field sensitivityAnalysisResultsFolder path to folder storing results of sensitivity analyses
#' @field simulationResultFileNames vector of names of CSV files storing simulation results
#' @field pkAnalysisResultsFileNames vector of names of CSV files storing results of pk analyses
#' @field sensitivityAnalysisResultsFileNames vector of names of CSV files storing results of sensitivity analyses
#' @import ospsuite
SimulationStructure <- R6::R6Class(
  "SimulationStructure",
  public = list(
    simulationSet = NULL,
    simulationResultsFolder = defaultTaskOutputFolders$simulate,
    pkAnalysisResultsFolder = defaultTaskOutputFolders$calculatePKParameters,
    sensitivityAnalysisResultsFolder = defaultTaskOutputFolders$sensitivityAnalysis,
    simulationResultFileNames = NULL,
    pkAnalysisResultsFileNames = NULL,
    sensitivityAnalysisResultsFileNames = NULL,

    #' @description
    #' Create a new `SimulationStructure` object.
    #' Build and store names of potential subfolders to hold simulation results, pkAnalysis results and sensitivityAnalysis results.
    #' @param simulationSet `MeanModelSet` or `PopModelSet` R6 class object
    #' @param workflowFolder output folder of the worklow
    initialize = function(simulationSet,
                          workflowFolder = getwd()) {
      self$simulationSet <- simulationSet
      self$simulationResultFileNames <- file.path(
        workflowFolder,
        self$simulationResultsFolder,
        ifnotnull(
          self$simulationSet$populationName,
          defaultFileNames$popSimulationResultsFile(self$simulationSet$populationName, self$simulationSet$simulationSetName),
          defaultFileNames$simulationResultsFile(self$simulationSet$simulationSetName)
        )
      )

      self$pkAnalysisResultsFileNames <- file.path(
        workflowFolder,
        self$pkAnalysisResultsFolder,
        defaultFileNames$pkAnalysisResultsFile(self$simulationSet$simulationSetName)
      )

      self$sensitivityAnalysisResultsFileNames <- file.path(
        workflowFolder,
        self$sensitivityAnalysisResultsFolder,
        defaultFileNames$sensitivityAnalysisResultsFile(self$simulationSet$simulationSetName)
      )
    }
  )
)
