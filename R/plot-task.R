#' @title PlotTask
#' @description  R6 class for PlotTask settings
#' @field title section title in the report corresponding to the task
#' @field fileName name of report appendix file associated to task
#' @field getTaskResults function called by task that computes and format figure results
#' @field nameTaskResults name of the function that returns task results,
#' @export
PlotTask <- R6::R6Class(
  "PlotTask",
  inherit = Task,

  public = list(
    title = NULL,
    fileName = NULL,
    getTaskResults = NULL,
    nameTaskResults = "none",

    #' @description
    #' Create a `PlotTask` object
    #' @param reportTitle title to be printed in the report
    #' @param fileName name of report appendix file associated to task
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param nameTaskResults name of the function that returns task results,
    #' @param ... input parameters inherited from `Task` R6 class
    #' @return A new `PlotTask` object
    initialize = function(reportTitle = NULL,
                          fileName = NULL,
                          getTaskResults = NULL,
                          nameTaskResults = "none",
                          ...) {
      super$initialize(...)
      self$title <- reportTitle
      self$fileName <- file.path(self$workflowFolder, fileName)
      self$getTaskResults <- getTaskResults
      self$nameTaskResults <- nameTaskResults
    },

    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of results from task run.
    #' Currently, results contains at least 2 fields: `plots` and `tables`
    #' They are to be deprecated and replaced using `TaskResults` objects
    saveResults = function(structureSet, taskResults) {
      simulationSetName <- structureSet$simulationSet$simulationSetName
      addTextChunk(
        self$fileName,
        paste0("## ", self$title, " for ", simulationSetName),
        logFolder = self$workflowFolder
      )
      for (result in taskResults) {
        plotFileName <- getDefaultFileName(
          simulationSetName,
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        
        tableFileName <- getDefaultFileName(
          simulationSetName,
          suffix = result$id,
          extension = "csv"
        )
        tableFilePath <- self$getAbsolutePath(tableFileName)
        
        # Figure and tables paths need to be relative to the final md report
        figureFileRelativePath <- gsub(
          pattern = paste0(self$workflowFolder, "/"),
          replacement = "",
          x = figureFilePath
        )
        tableFileRelativePath <- gsub(
          pattern = paste0(self$workflowFolder, "/"),
          replacement = "",
          x = tableFilePath
        )
        
        tryCatch({
          result$saveFigure(fileName = figureFilePath, logFolder = self$workflowFolder)
        },
        error = function(e) {
          logErrorThenStop(messages$ggsaveError(figureFilePath, simulationSetName, e), logFolder)
        })
        result$addFigureToReport(
          reportFile = self$fileName,
          fileRelativePath = figureFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          logFolder = self$workflowFolder
        )
        
        result$saveTable(fileName = tableFilePath, logFolder = self$workflowFolder)
        result$addTableToReport(
          reportFile = self$fileName,
          fileRelativePath = tableFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          digits = self$settings$digits,
          scientific = self$settings$scientific,
          logFolder = self$workflowFolder
        )
        
        result$addTextChunkToReport(
          reportFile = self$fileName,
          logFolder = self$workflowFolder
        )
      }
      return(invisible())
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
    runTask = function(structureSets) {
      re.tStartAction(actionType = "TLFGeneration")
      logWorkflow(
        message = paste0("Starting: ", self$message),
        pathFolder = self$workflowFolder
      )
      resetReport(self$fileName, self$workflowFolder)
      addTextChunk(
        self$fileName,
        paste0("# ", self$title),
        logFolder = self$workflowFolder
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE)
      }

      for (set in structureSets) {
        logWorkflow(
          message = paste0(self$message, " for ", set$simulationSet$simulationSetName),
          pathFolder = self$workflowFolder
        )
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(
            set,
            self$workflowFolder,
            self$settings
          )
          self$saveResults(set, taskResults)
        }
      }
      re.tEndAction()
    }
  )
)
