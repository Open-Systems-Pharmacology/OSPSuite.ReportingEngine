#' @title SimulationStructure
#' @description R6 class representing that stores file structure (file names and paths) relating to a particular instance of a simulationSet object
#' @field workflowFolder root folder storing workflow results
#' @field simulationSet the set for which the SimulationStructure object stores paths input and results
#' @field simulationResultsFolder path to folder storing results of simulations
#' @field pkAnalysisResultsFolder path to folder storing results of pk analyses
#' @field sensitivityAnalysisResultsFolder path to folder storing results of sensitivity analyses
#' @field simulationResultFileNames vector of names of CSV files storing simulation results
#' @field pkAnalysisResultsFileNames vector of names of CSV files storing results of pk analyses
#' @field sensitivityAnalysisResultsFileNames vector of names of CSV files storing results of sensitivity analyses
#' @field popSensitivityAnalysisResultsIndexFile path to file containing index of population sensitivity analysis results
#' @field parameterDisplayPaths data.frame mapping parameters to user-defined display paths
#' @field simulationSetDescriptor Descriptor of simulation sets indicated in reports
#' @import ospsuite
#' @keywords internal
SimulationStructure <- R6::R6Class(
  "SimulationStructure",
  public = list(
    workflowFolder = NULL,
    simulationSet = NULL,
    simulationResultsFolder = defaultTaskOutputFolders$simulate,
    pkAnalysisResultsFolder = defaultTaskOutputFolders$calculatePKParameters,
    sensitivityAnalysisResultsFolder = defaultTaskOutputFolders$sensitivityAnalysis,
    simulationResultFileNames = NULL,
    pkAnalysisResultsFileNames = NULL,
    sensitivityAnalysisResultsFileNames = NULL,
    popSensitivityAnalysisResultsIndexFile = NULL,
    parameterDisplayPaths = NULL,
    simulationSetDescriptor = NULL,

    #' @description
    #' Create a new `SimulationStructure` object.
    #' Build and store names of potential subfolders to hold simulation results, pkAnalysis results and sensitivityAnalysis results.
    #' @param simulationSet A `SimulationSet` or `PopulationSimulationSet` object
    #' @param workflowFolder output folder of the worklow
    initialize = function(simulationSet,
                          workflowFolder = getwd()) {
      self$workflowFolder <- workflowFolder
      self$simulationSet <- simulationSet
      self$simulationResultFileNames <- file.path(
        workflowFolder,
        self$simulationResultsFolder,
        defaultFileNames$simulationResultsFile(self$simulationSet$simulationSetName)
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

      self$popSensitivityAnalysisResultsIndexFile <- file.path(
        workflowFolder,
        self$sensitivityAnalysisResultsFolder,
        defaultFileNames$popSensitivityResultsIndexFile(self$simulationSet$simulationSetName)
      )
    }
  )
)
