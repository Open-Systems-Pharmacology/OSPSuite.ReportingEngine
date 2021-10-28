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
      addTextChunk(
        self$fileName,
        paste0("## ", self$title, " for ", structureSet$simulationSet$simulationSetName),
        logFolder = self$workflowFolder
      )
      for (plotName in names(taskResults$plots)) {
        plotFileName <- getDefaultFileName(structureSet$simulationSet$simulationSetName,
          suffix = plotName,
          extension = reEnv$defaultPlotFormat$format
        )

        figureFilePath <- self$getAbsolutePath(plotFileName)
        ggplot2::ggsave(
          filename = figureFilePath,
          plot = taskResults$plots[[plotName]],
          width = reEnv$defaultPlotFormat$width,
          height = reEnv$defaultPlotFormat$height,
          dpi = reEnv$defaultPlotFormat$dpi,
          units = reEnv$defaultPlotFormat$units
        )
        re.tStoreFileMetadata(access = "write", filePath = figureFilePath)
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
      }

      for (tableName in names(taskResults$tables)) {
        tableFileName <- getDefaultFileName(structureSet$simulationSet$simulationSetName,
          suffix = tableName,
          extension = "csv"
        )

        write.csv(taskResults$tables[[tableName]],
          file = self$getAbsolutePath(tableFileName),
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )

        re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(tableFileName))
        # If the task output no plot, but tables, tables will be included in the report
        if (is.null(taskResults$plots)) {
          if (!is.null(taskResults$captions[[tableName]])) {
            addTextChunk(self$fileName, paste0("Table: ", taskResults$captions[[tableName]]), logFolder = self$workflowFolder)
          }

          addTableChunk(
            fileName = self$fileName,
            tableFileRelativePath = self$getRelativePath(tableFileName),
            tableFileRootDirectory = self$workflowFolder,
            digits = self$settings$digits,
            scientific = self$settings$scientific,
            logFolder = self$workflowFolder
          )
        }

        logWorkflow(
          message = paste0("Table '", self$getAbsolutePath(tableFileName), "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )
      }
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
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
      re.tEndAction(actionToken = actionToken)
    }
  )
)
