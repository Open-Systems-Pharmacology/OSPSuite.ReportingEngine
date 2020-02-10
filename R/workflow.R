#' @title Workflow
#' @description R6 class representing Reporting Engine generic Workflow
#' @field reportingEngineInfo R6 class object with relevant information about reporting engine
#' @field simulationSets list of `MeanModelSet` R6 class objects
#' @field observedData list of observed `data` and `metaData`
#' @field reportFolder path where report and logs are saved
#' @field resultsFolder path where results are saved
#' @field figuresFolder path where figure are saved
#' @import tlf
#' @import ospsuite
Workflow <- R6::R6Class(
  "Workflow",
  public = list(
    reportingEngineInfo = ReportingEngineInfo$new(),
    simulationSets = NULL,
    observedData = NULL,
    reportFolder = NULL,
    resultsFolder = NULL,
    figuresFolder = NULL,

    #' @description
    #' Create a new `Workflow` object.
    #' @param simulationSets names of pkml files to be used for simulations
    #' @param observedDataFile name of csv file to be used for observations
    #' @param observedMetaDataFile name of csv file to be used as dictionary for observed data
    #' @param reportFolder name of folder where reports and logs are saved
    #' @param resultsFolder name of folder where results are saved
    #' @param figuresFolder name of folder where figures are saved
    #' @return A new `Workflow` object
    initialize = function(simulationSets,
                          observedDataFile = NULL,
                          observedMetaDataFile = NULL,
                          reportFolder = file.path(getwd(), defaultFileNames$reportFolder()),
                          resultsFolder = file.path(getwd(), defaultFileNames$resultsFolder()),
                          figuresFolder = file.path(getwd(), defaultFileNames$figuresFolder())) {


      # Check of Workflow inputs
      for (simulationSet in simulationSets){
        validateIsOfType(simulationSet, "MeanModelSet")
      }
      validateIsOfType(observedDataFile, "character", nullAllowed = TRUE)
      validateIsOfType(observedMetaDataFile, "character", nullAllowed = TRUE)
      
      reportFolderCheck <- checkExisitingPath(reportFolder)
      if (!is.null(reportFolderCheck)){
        logDebug(message = reportFolderCheck)
        reportFolder <- paste(reportFolder, format(Sys.time(), "%H%M"), sep="-")
      }
      resultsFolderCheck <- checkExisitingPath(resultsFolder)
      if (!is.null(resultsFolderCheck)){
        logDebug(message = resultsFolderCheck)
        resultsFolder <- paste(resultsFolder, format(Sys.time(), "%H%M"), sep="-")
      }
      figuresFolderCheck <- checkExisitingPath(figuresFolder)
      if (!is.null(figuresFolderCheck)){
        logDebug(message = figuresFolderCheck)
        figuresFolder <- paste(figuresFolder, format(Sys.time(), "%H%M"), sep="-")
      }
      
      # Create workflow output structure
      dir.create(reportFolder)
      logDebug(message = paste0(reportFolder, " was successfully created"))
      dir.create(resultsFolder)
      logDebug(message = paste0(resultsFolder, " was successfully created"))
      dir.create(figuresFolder)
      logDebug(message = paste0(figuresFolder, " was successfully created"))
      
      self$reportFolder <- reportFolder
      self$resultsFolder <- resultsFolder
      self$figuresFolder <- figuresFolder

      self$simulationSets <- simulationSets
      
      if (!is.null(observedDataFile)){
        data <- read.csv(observedDataFile)
        metaData <- read.csv(observedMetaDataFile)
        
        self$observedData <- list("data" = data,
                                  "metaData" = metaData)
      }
    }
  )
)
