#' @title PlotTask
#' @description  R6 class for PlotTask settings
#' @field title section title in the report corresponding to the task
#' @field getTaskResults function called by task that computes and format figure results
#' @field workflowFolder folder where workflow is run and saved
#' @export
PlotTask <- R6::R6Class(
  "PlotTask",
  inherit = Task,

  public = list(
    title = NULL,
    getTaskResults = NULL,
    workflowFolder = NULL,

    #' @description
    #' Create a `PlotTask` object
    #' @param reportTitle title to be printed in the report
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param workflowFolder folder where workflow is run and saved
    #' @param ... input parameters inherited from `Task` R6 class
    #' @return A new `PlotTask` object
    initialize = function(reportTitle = NULL,
                              getTaskResults = NULL,
                              workflowFolder = getwd(),
                              ...) {
      super$initialize(...)
      self$title <- reportTitle
      self$getTaskResults <- getTaskResults
      self$workflowFolder <- workflowFolder
    },

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    #' @param reportFile name of report
    #' @param workflowFolder path of workflow
    saveResults = function(set,
                               taskResults,
                               reportFile) {
      for (plotName in names(taskResults$plots)) {
        plotFileName <- file.path(
          self$output[[1]],
          getDefaultFileName(set$simulationSet$simulationSetName,
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

        addRmdFigureChunk(
          fileName = reportFile,
          figureFile = plotFileName
        )
      }

      for (tableName in names(taskResults$tables)) {
        tableFileName <- file.path(
          self$output[[1]],
          getDefaultFileName(set$simulationSet$simulationSetName,
            suffix = tableName,
            extension = "csv"
          )
        )

        write.csv(taskResults$tables[[tableName]],
          file = tableFileName,
          row.names = FALSE
        )
      }
    },

    #' @description
    #' Print `Task` features
    #' @param structureSets list of `SimulationStructure` R6 class
    #' @param reportFileName name of report file
    #' @return List of task features
    runTask = function(structureSets,
                           reportFileName) {
      logWorkflow(
        message = paste0("Starting: ", self$message),
        pathFolder = self$workflowFolder
      )
      addRmdTextChunk(
        reportFileName,
        paste0("# ", self$title)
      )
      if (length(self$output) > 0) {
        dir.create(self$output[[1]])
      }

      # Goodness of fit task creates a histogram of residuals merged accross the simulaiton
      residualsAcrossAllSimulations <- NULL

      for (set in structureSets) {
        logWorkflow(
          message = paste0(self$message, " for ", set$simulationSet$simulationName),
          pathFolder = self$workflowFolder
        )
        if (self$validateInput()) {
          addRmdTextChunk(
            reportFileName,
            paste0("## ", self$title, " for ", set$simulationSet$simulationSetName)
          )

          taskResults <- self$getTaskResults(
            set,
            self$settings$plotConfigurations
          )

          if (!is.null(taskResults[["residuals"]])) {
            residualsAcrossAllSimulations <- rbind.data.frame(
              residualsAcrossAllSimulations,
              taskResults$residuals$data
            )
          }

          self$saveResults(
            set,
            taskResults,
            reportFileName
          )
        }
      }

      if (!is.null(residualsAcrossAllSimulations)) {
        plotFileName <- file.path(
          self$output[[1]],
          getDefaultFileName(
            suffix = "residuals",
            extension = "png",
            sep = ""
          )
        )
        tableFileName <- file.path(
          self$output[[1]],
          getDefaultFileName(
            suffix = "residuals",
            extension = "csv",
            sep = ""
          )
        )

        write.csv(residualsAcrossAllSimulations,
          file = tableFileName,
          row.names = FALSE
        )

        # TO DO: integrate tlf fix of mapping/plotConfig for no group variable
        residualHistogramPlot <- tlf::plotHistogram(
          data = cbind.data.frame(residualsAcrossAllSimulations,
            " " = "Residuals"
          ),
          dataMapping = tlf::HistogramDataMapping$new(
            x = "Residuals",
            fill = " "
          ),
          plotConfiguration = self$settings$plotConfigurations[["histogram"]]
        )

        # TO DO: define parameters from settings/plotConfiguration
        ggplot2::ggsave(
          filename = plotFileName,
          plot = residualHistogramPlot,
          width = 16, height = 9, units = "cm"
        )

        addRmdTextChunk(
          reportFileName,
          "## Residuals across all simulations"
        )

        addRmdFigureChunk(
          fileName = reportFileName,
          figureFile = plotFileName
        )
      }
    }
  )
)

#' @title PlotPKParametersTask
#' @description  R6 class for PlotPKParametersTask settings
#' @export
PlotPKParametersTask <- R6::R6Class(
  "PlotPKParametersTask",
  inherit = PlotTask,

  public = list(

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    #' @param reportFile name of report
    #' @param workflowFolder path of workflow
    saveResults = function(set,
                               taskResults,
                               reportFile) {
      tableFileName <- file.path(
        self$output[[1]],
        getDefaultFileName(set$simulationSet$simulationSetName,
          suffix = "pkParametersTable",
          extension = "csv"
        )
      )
      write.csv(taskResults,
        file = tableFileName,
        row.names = FALSE
      )

      addRmdTableChunk(
        fileName = reportFile,
        tableFile = tableFileName
      )
    }
  )
)
