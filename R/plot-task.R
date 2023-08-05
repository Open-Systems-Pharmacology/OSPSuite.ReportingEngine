#' @title PlotTask
#' @description  R6 class for PlotTask settings
#' @field title section title in the report corresponding to the task
#' @field reference id of anchor tag referencing title
#' @field fileName name of report appendix file associated to task
#' @field getTaskResults function called by task that computes and format figure results
#' @field nameTaskResults name of the function that returns task results,
#' @export
#' @family workflow tasks
PlotTask <- R6::R6Class(
  "PlotTask",
  inherit = Task,
  public = list(
    title = NULL,
    reference = NULL,
    fileName = NULL,
    getTaskResults = NULL,
    nameTaskResults = "none",

    #' @description
    #' Create a `PlotTask` object
    #' @param reportTitle title to be printed in the report
    #' @param reportReference id of anchor tag referencing title
    #' @param fileName name of report appendix file associated to task
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param nameTaskResults name of the function that returns task results,
    #' @param ... input parameters inherited from `Task` R6 class
    #' @return A new `PlotTask` object
    initialize = function(reportTitle = NULL,
                          reportReference = NULL,
                          fileName = NULL,
                          getTaskResults = NULL,
                          nameTaskResults = "none",
                          ...) {
      super$initialize(...)
      self$title <- reportTitle
      self$reference <- reportReference
      self$fileName <- file.path(self$workflowFolder, fileName)
      self$getTaskResults <- getTaskResults
      self$nameTaskResults <- nameTaskResults
    },

    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of `TaskResults` objects
    saveResults = function(structureSet, taskResults) {
      simulationSetName <- structureSet$simulationSet$simulationSetName
      # Issue #1084: new reference title get anchor at the end of title
      addTextChunk(
        fileName = self$fileName,
        text = paste(
          "##", self$title, "for", simulationSetName, 
          anchor(paste0(self$reference, "-", removeForbiddenLetters(simulationSetName)))
          )
        )
      for (result in taskResults) {
        # Get both absolute and relative paths for figures and tables
        # Absolute path is required to always find the figure/table
        # Relative path is required by the final md report
        plotFileName <- getDefaultFileName(
          removeForbiddenLetters(simulationSetName),
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        figureFileRelativePath <- self$getRelativePath(plotFileName)

        tableFileName <- getDefaultFileName(
          removeForbiddenLetters(simulationSetName),
          suffix = result$id,
          extension = "csv"
        )
        tableFilePath <- self$getAbsolutePath(tableFileName)
        tableFileRelativePath <- self$getRelativePath(tableFileName)

        # Provide error message indicating which file could not be saved
        tryCatch(
          {
            result$saveFigure(fileName = figureFilePath)
          },
          error = function(e) {
            stop(messages$ggsaveError(figureFilePath, simulationSetName, e))
          }
        )
        result$addFigureToReport(
          reportFile = self$fileName,
          fileRelativePath = figureFileRelativePath,
          fileRootDirectory = self$workflowFolder
        )

        result$saveTable(fileName = tableFilePath)
        result$addTableToReport(
          reportFile = self$fileName,
          fileRelativePath = tableFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          digits = self$settings$digits,
          scientific = self$settings$scientific
        )

        result$addTextChunkToReport(reportFile = self$fileName)
      }
      return(invisible())
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logInfo(messages$runStarting(self$message))
      t0 <- tic()
      resetReport(self$fileName)
      addTextChunk(
        fileName = self$fileName,
        text = paste("#", self$title, anchor(self$reference))
        )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE)
      }

      for (set in structureSets) {
        logInfo(messages$runStarting(self$message, set$simulationSet$simulationSetName))
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(set, self$settings)
          self$saveResults(set, taskResults)
        }
      }
      re.tEndAction(actionToken = actionToken)
      logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
    }
  )
)
