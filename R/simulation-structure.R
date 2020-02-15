#' @title SimulationStructure
#' @description R6 class representing Reporting Engine generic Workflow
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field simulationSets list of `MeanModelSet` R6 class objects
#' @field observedData list of observed `data` and `metaData`
#' @field reportFolder path where report and logs are saved
#' @field resultsFolder path where results are saved
#' @field figuresFolder path where figure are saved
#' @import tlf
#' @import ospsuite
SimulationStructure <- R6::R6Class(
  "SimulationSetStructure",
  public = list(
    reportingEngineInfo = ReportingEngineInfo$new(),
    simulationSet= NULL,
    simulationSetName = NULL,
    inputFilesFolder = NULL,
    simulationResultsFolder = NULL,
    pkAnalysisResultsFolder = NULL,
    sensitivityAnalysisResultsFolder = NULL,


    #Initialize SimulationStructure
    #Create simulationSetFolder folder to store simulationSet results.
    #Create inputFilesFolder as subfolder of simulationSetFolder
    #Copy simulation set input files to inputFilesFolder
    #Build and store names of potential subfolders to hold simulation results, pkAnalysis results and sensitivityAnalysis results.
    initialize = function(simulationSet,
                          workflowResultsFolder,
                          workflowFiguresFolder = NULL) {
      self$simulationSet <- simulationSet
      simulationSetFolder <- file.path(workflowResultsFolder,self$simulationSet$simulationSetName)
      dir.create(simulationSetFolder)
      logDebug(message = paste0(simulationSetFolder, " was successfully created"), printConsole = TRUE)
      self$inputFilesFolder <- file.path(simulationSetFolder,defaultFileNames$inputFolder())

      dir.create(self$inputFilesFolder)
      logDebug(message = paste0(self$inputFilesFolder, " was successfully created"), printConsole = TRUE)
      simulationSet$copyInputFiles(self$inputFilesFolder)
      self$simulationResultsFolder <- file.path(simulationSetFolder,defaultFileNames$simulationResultsFolder())
      self$pkAnalysisResultsFolder <- file.path(simulationSetFolder,defaultFileNames$pkAnalysisResultsFolder())
      self$sensitivityAnalysisResultsFolder <- file.path(simulationSetFolder,defaultFileNames$sensitivityAnalysisResultsFolder())
    }


  )
)
