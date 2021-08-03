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
    #' Save results from task run.
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    #' @param self$fileName name of report
    saveResults = function(taskResults) {
      resetReport(self$fileName, self$workflowFolder)
      addTextChunk(
        self$fileName,
        paste0("# ", self$title),
        logFolder = self$workflowFolder
      )
      for (plotName in names(taskResults$plots)) {
        plotFileName <- getDefaultFileName(
          suffix = plotName,
          extension = reEnv$defaultPlotFormat$format
        )

        # TO DO: define parameters from settings/plotConfiguration
        ggplot2::ggsave(
          filename = self$getAbsolutePath(plotFileName),
          plot = taskResults$plots[[plotName]],
          width = reEnv$defaultPlotFormat$width, 
          height = reEnv$defaultPlotFormat$height, 
          dpi = reEnv$defaultPlotFormat$dpi,
          units = reEnv$defaultPlotFormat$units
        )
        re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(plotFileName))
        logWorkflow(
          message = paste0("Plot '", self$getRelativePath(plotFileName), "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )

        if (!is.null(taskResults$captions[[plotName]])) {
          addTextChunk(self$fileName, paste0("Figure: ", taskResults$captions[[plotName]]), logFolder = self$workflowFolder)
        }

        addFigureChunk(
          fileName = self$fileName,
          figureFileRelativePath = self$getRelativePath(plotFileName),
          figureFileRootDirectory = self$workflowFolder,
          logFolder = self$workflowFolder
        )

        if (!is.null(taskResults$tables[[plotName]])) {
          tableFileName <- getDefaultFileName(
            suffix = plotName,
            extension = "csv"
          )

          write.csv(taskResults$tables[[plotName]],
            file = self$getAbsolutePath(tableFileName),
            row.names = FALSE,
            fileEncoding = "UTF-8"
          )

          if (!is.null(taskResults$tableCaptions[[plotName]])) {
            addTextChunk(self$fileName, paste0("Table: ", taskResults$tableCaptions[[plotName]]), logFolder = self$workflowFolder)
          }

          addTableChunk(
            fileName = self$fileName,
            tableFileRelativePath = self$getRelativePath(tableFileName),
            tableFileRootDirectory = self$workflowFolder,
            logFolder = self$workflowFolder
          )

          re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(tableFileName))
          logWorkflow(
            message = paste0("Table '", self$getAbsolutePath(tableFileName), "' was successfully saved."),
            pathFolder = self$workflowFolder,
            logTypes = LogTypes$Debug
          )
        }
      }
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
