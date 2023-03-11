#' @title QualificationTask
#' @description  R6 class for QualificationTask settings
#' @export
#' @family workflow tasks
QualificationTask <- R6::R6Class(
  "QualificationTask",
  inherit = PlotTask,
  public = list(
    #' @description
    #' Save results from task run.
    #' @param taskResults list of `TaskResults` objects
    #' @param configurationPlan A `ConfigurationPlan` object
    saveResults = function(taskResults, configurationPlan) {
      for (result in taskResults) {
        # Get both absolute and relative paths for figures and tables
        # Absolute path is required to always find the figure/table
        # Relative path is required by the final md report
        figureFilePath <- file.path(
          configurationPlan$getSectionPath(result$sectionId),
          getDefaultFileName(
            suffix = result$id,
            extension = reEnv$defaultPlotFormat$format
          )
        )
        tableFilePath <- file.path(
          configurationPlan$getSectionPath(result$sectionId),
          getDefaultFileName(
            suffix = result$id,
            extension = "csv"
          )
        )

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

        result$saveFigure(fileName = figureFilePath)
        result$addFigureToReport(
          reportFile = configurationPlan$getSectionMarkdown(result$sectionId),
          fileRelativePath = figureFileRelativePath,
          fileRootDirectory = self$workflowFolder
        )
        result$saveTable(fileName = tableFilePath)

        result$addTableToReport(
          reportFile = configurationPlan$getSectionMarkdown(result$sectionId),
          fileRelativePath = tableFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          digits = self$settings$digits,
          scientific = self$settings$scientific
        )

        result$addTextChunkToReport(
          reportFile = configurationPlan$getSectionMarkdown(result$sectionId)
        )
      }
    },

    #' @description
    #' Run task and save its output
    #' @param configurationPlan A `ConfigurationPlan` object
    runTask = function(configurationPlan) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logInfo(messages$runStarting(self$message))
      t0 <- tic()

      if (self$validateInput()) {
        taskResults <- self$getTaskResults(
          configurationPlan,
          self$settings
        )
        self$saveResults(taskResults, configurationPlan)
      }
      re.tEndAction(actionToken = actionToken)
      logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
    }
  )
)
