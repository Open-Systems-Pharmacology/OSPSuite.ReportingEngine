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
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    saveResults = function(set,
                           taskResults) {
      addTextChunk(
        self$fileName,
        paste0("## ", self$title, " for ", set$simulationSet$simulationSetName),
        logFolder = self$workflowFolder
      )
      for (plotName in names(taskResults$plots)) {
        plotFileName <- file.path(
          self$outputFolder,
          getDefaultFileName(set$simulationSet$simulationSetName,
            suffix = plotName,
            extension = ExportPlotConfiguration$format
          )
        )
        # TO DO: define parameters from settings/plotConfiguration
        ggplot2::ggsave(
          filename = file.path(self$workflowFolder, plotFileName),
          plot = taskResults$plots[[plotName]],
          width = ExportPlotConfiguration$width, height = ExportPlotConfiguration$height, units = ExportPlotConfiguration$units
        )
        re.tStoreFileMetadata(access = "write", filePath = plotFileName)
        logWorkflow(
          message = paste0("Plot '", plotFileName, "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )

        if (!is.null(taskResults$captions[[plotName]])) {
          addTextChunk(self$fileName, paste0("Figure: ", taskResults$captions[[plotName]]), logFolder = self$workflowFolder)
        }
        addFigureChunk(fileName = self$fileName, figureFile = plotFileName, logFolder = self$workflowFolder)
      }

      for (tableName in names(taskResults$tables)) {
        tableFileName <- file.path(
          self$workflowFolder,
          self$outputFolder,
          getDefaultFileName(set$simulationSet$simulationSetName,
            suffix = tableName,
            extension = "csv"
          )
        )

        write.csv(taskResults$tables[[tableName]],
          file = tableFileName,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
        re.tStoreFileMetadata(access = "write", filePath = tableFileName)
        # If the task output no plot, but tables, tables will be included in the report
        if (is.null(taskResults$plots)) {
          addTableChunk(
            fileName = self$fileName,
            tableFile = tableFileName,
            logFolder = self$workflowFolder
          )
        }

        logWorkflow(
          message = paste0("Table '", tableFileName, "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )
      }
    },

    #' @description
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    #' @param self$fileName name of report file
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
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
          message = paste0(self$message, " for ", set$simulationSet$simulationName),
          pathFolder = self$workflowFolder
        )
        if (self$validateInput()) {
          taskResults <- self$getTaskResults(
            set,
            self$workflowFolder,
            self$settings
          )
          self$saveResults(set, taskResults)
        }
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
