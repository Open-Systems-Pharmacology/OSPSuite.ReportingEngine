#' @title PopulationPlotTask
#' @description  R6 class for PopulationPlotTask settings
#' @field workflowType Type of population workflow
#' @field xParameters list of parameter paths to be plotted along x-axis
#' @field yParameters list of parameter paths to be plotted along y-axis
#' @export
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
    #' @param taskResults list of results from task run.
    #' Currently, results contains at least 2 fields: `plots` and `tables`
    #' They are to be deprecated and replaced using `TaskResults` objects
    saveResults = function(taskResults) {
      resetReport(self$fileName, self$workflowFolder)
      addTextChunk(
        fileName = self$fileName,
        text = c(anchor(self$reference), "", paste0("# ", self$title)),
        logFolder = self$workflowFolder
      )
      for (result in taskResults) {
        plotFileName <- getDefaultFileName(
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        
        tableFileName <- getDefaultFileName(
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
          logErrorThenStop(messages$ggsaveError(figureFilePath, NULL, e), logFolder)
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
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logWorkflow(
        message = paste0("Starting: ", self$message),
        pathFolder = self$workflowFolder
      )

      if (self$validateInput()) {
        if (!is.null(self$outputFolder)) {
          dir.create(file.path(self$workflowFolder, self$outputFolder))
        }

        taskResults <- self$getTaskResults(
          structureSets,
          self$workflowFolder,
          self$settings,
          self$workflowType,
          self$xParameters,
          self$yParameters
        )
        self$saveResults(taskResults)
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
