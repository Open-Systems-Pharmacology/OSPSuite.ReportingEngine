#' @title SimulationStructure
#' @description R6 class representing that stores file structure (file names and paths) relating to a particular instance of a simulationSet object
#' @field simulationSet: the set for which the SimulationStructure object stores paths input and results
#' @field inputFilesFolder: path to folder storing pkml and population data files for the simulationSet
#' @field simulationResultsFolder: path to folder storing results of simulations
#' @field pkAnalysisResultsFolder: path to folder storing results of pk analyses
#' @field sensitivityAnalysisResultsFolder: path to folder storing results of sensitivity analyses
#' @field simulationResultFileNames: vector of names of CSV files storing simulation results
#' @field pkAnalysisResultsFileNames: vector of names of CSV files storing results of pk analyses
#' @field sensitivityAnalysisResultsFileNames: vector of names of CSV files storing results of sensitivity analyses
#' @import ospsuite
SimulationStructure <- R6::R6Class(
  "SimulationSetStructure",
  public = list(
    simulationSet = NULL,
    inputFilesFolder = NULL,
    simulationResultsFolder = NULL,
    pkAnalysisResultsFolder = NULL,
    sensitivityAnalysisResultsFolder = NULL,
    simulationResultFileNames = NULL,
    pkAnalysisResultsFileNames = NULL,
    sensitivityAnalysisResultsFileNames = NULL,
    # Initialize SimulationStructure
    # Create simulationSetFolder folder to store simulationSet results.
    # Create inputFilesFolder as subfolder of simulationSetFolder
    # Copy simulation set input files to inputFilesFolder
    # Build and store names of potential subfolders to hold simulation results, pkAnalysis results and sensitivityAnalysis results.
    initialize = function(simulationSet,
                          workflowResultsFolder,
                          workflowFiguresFolder = NULL) {
      self$simulationSet <- simulationSet
      simulationSetFolder <- file.path(workflowResultsFolder, self$simulationSet$simulationSetName)

      createFolder(simulationSetFolder)

      self$inputFilesFolder <- file.path(simulationSetFolder, defaultFileNames$inputFolder())
      createFolder(self$inputFilesFolder)

      simulationSet$copyInputFiles(self$inputFilesFolder)
      self$simulationResultsFolder <- file.path(simulationSetFolder, defaultFileNames$simulationResultsFolder())
      self$pkAnalysisResultsFolder <- file.path(simulationSetFolder, defaultFileNames$pkAnalysisResultsFolder())
      self$sensitivityAnalysisResultsFolder <- file.path(simulationSetFolder, defaultFileNames$sensitivityAnalysisResultsFolder())
    }
  )
)
