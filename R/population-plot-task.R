#' @title PopulationPlotTask
#' @description  R6 class for PopulationPlotTask settings
#' @field workflowType Type of population workflow
#' @field xParameters list of parameter paths to be plotted along x-axis
#' @field yParameters list of parameter paths to be plotted along y-axis
#' @export
#' @family workflow tasks
PopulationPlotTask <- R6::R6Class(
  "PopulationPlotTask",
  inherit = PlotTask,
  public = list(
    workflowType = NULL,
    xParameters = NULL,
    yParameters = NULL,

    #' @description
    #' Create a `PopulationPlotTask` object
    #' @param workflowType Type of population workflow. Use enum `PopulationWorkflowTypes` to get list of workflow types.
    #' @param xParameters list of parameter paths to be plotted along x-axis
    #' @param yParameters list of parameter paths to be plotted along y-axis
    #' @param ... input parameters inherited from `PlotTask` R6 class
    #' @return A new `PopulationPlotTask` object
    initialize = function(workflowType = PopulationWorkflowTypes$parallelComparison,
                          xParameters = NULL,
                          yParameters = NULL,
                          ...) {
      super$initialize(...)
      validateIsIncluded(workflowType, PopulationWorkflowTypes)
      self$workflowType <- workflowType
      self$xParameters <- xParameters
      self$yParameters <- yParameters
    },

    #' @description
    #' Save the task results
    #' @param taskResults list of `TaskResults` objects
    saveResults = function(taskResults) {
      resetReport(self$fileName)
      addTextChunk(
        fileName = self$fileName,
        text = paste("# ", self$title, anchor(self$reference), sep = "")
      )
      for (result in taskResults) {
        # Get both absolute and relative paths for figures and tables
        # Absolute path is required to always find the figure/table
        # Relative path is required by the final md report
        plotFileName <- getDefaultFileName(
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        figureFileRelativePath <- self$getRelativePath(plotFileName)

        tableFileName <- getDefaultFileName(
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
            stop(messages$ggsaveError(figureFilePath, NULL, e))
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
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logInfo(messages$runStarting(self$message))
      t0 <- tic()

      if (self$validateInput()) {
        if (!is.null(self$outputFolder)) {
          dir.create(file.path(self$workflowFolder, self$outputFolder))
        }

        taskResults <- self$getTaskResults(
          structureSets,
          self$settings,
          self$workflowType,
          self$xParameters,
          self$yParameters
        )
        self$saveResults(taskResults)
      }
      re.tEndAction(actionToken = actionToken)
      logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
    }
  )
)
