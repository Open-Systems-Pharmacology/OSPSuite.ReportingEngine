#' @title PopulationSensitivityAnalysisTask
#' @description  R6 class for PopulationSensitivityAnalysisTask settings
#' @field getTaskResults function called by task that computes and format figure results
#' @field settings A `SensitivityAnalysisSettings` object
#' @importFrom ospsuite.utils %||%
PopulationSensitivityAnalysisTask <- R6::R6Class(
  "PopulationSensitivityAnalysisTask",
  inherit = SensitivityAnalysisTask,
  public = list(
    getTaskResults = NULL,
    settings = NULL,

    #' @description
    #' Create a `PopulationSensitivityAnalysisTask` object
    #' @param getTaskResults function called by task that computes and format figure results
    #' @param settings `SensitivityAnalysisSettings` object
    #' @param ... parameters inherited from R6 class `SensitivityAnalysisTask` object
    #' @return A new `PopulationSensitivityAnalysisTask` object
    initialize = function(getTaskResults = NULL,
                          settings = NULL,
                          ...) {
      validateIsOfType(settings, "SensitivityAnalysisSettings", nullAllowed = TRUE)
      super$initialize(...)
      self$settings <- settings %||% SensitivityAnalysisSettings$new()
      self$getTaskResults <- getTaskResults
    },

    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of results from task run
    saveResults = function(structureSet, taskResults) {
      results <- taskResults$populationSensitivityResults
      if (is.null(results)) {
        return()
      }
      indexDataFrame <- taskResults$indexDataFrame
      indexFileName <- taskResults$indexFileName
      for (fileName in names(results)) {
        filePath <- file.path(self$workflowFolder, self$outputFolder, fileName)
        ospsuite::exportSensitivityAnalysisResultsToCSV(
          results = results[[fileName]],
          filePath = filePath
        )
        re.tStoreFileMetadata(access = "write", filePath = filePath)
      }
      indexFilePath <- file.path(self$workflowFolder, self$outputFolder, indexFileName)
      write.csv(x = indexDataFrame, file = indexFilePath, row.names = FALSE)
      re.tStoreFileMetadata(access = "write", filePath = indexFilePath)
    }
  )
)
