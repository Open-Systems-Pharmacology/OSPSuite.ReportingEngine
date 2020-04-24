#' @title PopulationPlotTask
#' @description  R6 class for PopulationPlotTask settings
#' @field workflowType Type of population workflow
#' @export
PopulationPlotTask <- R6::R6Class(
  "PopulationPlotTask",
  inherit = PlotTask,

  public = list(
    workflowType = NULL,

    #' @description
    #' Create a `PopulationPlotTask` object
    #' @param workflowType Type of population workflow. Use enum `PopulationWorkflowTypes` to get list of workflow types.
    #' @param ... input parameters inherited from `PlotTask` R6 class
    #' @return A new `PopulationPlotTask` object
    initialize = function(workflowType = PopulationWorkflowTypes$parallelComparison,
                              ...) {
      super$initialize(...)
      validateIsIncluded(workflowType, PopulationWorkflowTypes)
      self$workflowType <- workflowType
    },

    #' @description
    #' Save results from task run.
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    #' @param reportFile name of report
    saveResults = function(taskResults,
                               reportFile) {
      for (plotName in names(taskResults$plots)) {
        plotFileName <- file.path(
          self$workflowFolder,
          self$outputFolder,
          getDefaultFileName(
            suffix = plotName,
            extension = "png"
          )
        )
        # TO DO: define parameters from settings/plotConfiguration
        ggplot2::ggsave(
          filename = plotFileName,
          plot = taskResults$plots[[plotName]],
          width = 16, height = 9, units = "cm"
        )
        logWorkflow(
          message = paste0("Plot '", plotFileName, "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )

        addFigureChunk(
          fileName = reportFile,
          figureFile = plotFileName,
          logFolder = self$workflowFolder
        )

        tableFileName <- file.path(
          self$workflowFolder,
          self$outputFolder,
          getDefaultFileName(
            suffix = plotName,
            extension = "csv"
          )
        )

        write.csv(taskResults$tables[[plotName]],
          file = tableFileName,
          row.names = FALSE
        )

        addTableChunk(
          fileName = reportFile,
          tableFile = tableFileName,
          logFolder = self$workflowFolder
        )

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
    #' @param reportFileName name of report file
    runTask = function(structureSets,
                           reportFileName) {
      logWorkflow(
        message = paste0("Starting: ", self$message),
        pathFolder = self$workflowFolder
      )
      addTextChunk(
        reportFileName,
        paste0("# ", self$title),
        logFolder = self$workflowFolder
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder))
      }

      addTextChunk(
        reportFileName,
        paste0("## ", self$title),
        logFolder = self$workflowFolder
      )

      taskResults <- self$getTaskResults(
        structureSets,
        self$workflowFolder,
        self$settings,
        self$workflowType
      )

      self$saveResults(
        taskResults,
        reportFileName
      )
    }
  )
)
