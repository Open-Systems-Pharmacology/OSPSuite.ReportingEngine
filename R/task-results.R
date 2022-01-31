#' @title TaskResults
#' @description  R6 class for TaskResults settings
#' @field id unique identifer of results, helps in deriving final plot name
#' @field sectionId unique identifer of section results, used only by qualification workflows
#' @field plot `ggplot` object corresponding to the figure to be saved
#' @field plotCaption text included into the report explaining the figure
#' @field includePlot logical indicating if the plot should be included in final report
#' @field table data.frame corresponding to the table to be saved
#' @field tableCaption text included into the report explaining the table
#' @field includeTable logical indicating if the table should be included in final report
#' @field textChunk text included into the report explaining the table
#' @field includeTextChunk logical indicating if the text chunk should be included in final report
#' @importFrom ospsuite.utils %||%
#' @keywords internal
TaskResults <- R6::R6Class(
  "TaskResults",
  cloneable = FALSE,
  public = list(
    id = NULL,
    sectionId = NULL,
    plot = NULL,
    plotCaption = NULL,
    includePlot = TRUE,
    table = NULL,
    tableCaption = NULL,
    includeTable = FALSE,
    textChunk = NULL,
    includeTextChunk = FALSE,

    #' @description
    #' If table is not null, save table as a csv file at `tableFileName`
    #' @param fileName path of file corresponding to the figure to save
    #' @param logFolder folder were logs are saved
    saveFigure = function(fileName, logFolder = getwd()) {
      if (isOfLength(self$plot, 0)) {
        return()
      }
      # TODO once every plot will use tlf, deprecate the condition for null values
      ggplot2::ggsave(
        filename = fileName,
        plot = self$plot,
        width = self$plot$plotConfiguration$export$width %||% reEnv$defaultPlotFormat$width,
        height = self$plot$plotConfiguration$export$height %||% reEnv$defaultPlotFormat$height,
        dpi = reEnv$defaultPlotFormat$dpi,
        units = self$plot$plotConfiguration$export$units %||% reEnv$defaultPlotFormat$units
      )
      re.tStoreFileMetadata(access = "write", filePath = fileName)
    },

    #' @description
    #' If table is not null, save table as a csv file at `tableFileName`
    #' @param fileName path of csv file corresponding to the table to save
    #' @param logFolder folder were logs are saved
    saveTable = function(fileName, logFolder = getwd()) {
      if (isOfLength(self$table, 0)) {
        return()
      }
      write.csv(self$table, file = fileName, row.names = FALSE, fileEncoding = "UTF-8")
      re.tStoreFileMetadata(access = "write", filePath = fileName)
    },

    #' @description
    #' Add table results to a
    #' @param reportFile file in which the figure and its caption should be added
    #' @param fileRelativePath path of file relative to reportFile corresponding saved figure
    #' @param fileRootDirectory root directory of figure file path
    #' @param logFolder folder were logs are saved
    addFigureToReport = function(reportFile, fileRelativePath, fileRootDirectory, logFolder = getwd()) {
      if (isOfLength(self$plot, 0)) {
        return()
      }
      # includePlot select if the plot and its caption should be included in final report
      # However, tables in task results are saved no matter this field
      if (isFALSE(self$includePlot)) {
        return()
      }
      if (!isOfLength(self$plotCaption, 0)) {
        addTextChunk(reportFile, paste0("Figure: ", self$plotCaption), logFolder = logFolder)
      }
      addFigureChunk(
        fileName = reportFile,
        figureFileRelativePath = fileRelativePath,
        figureFileRootDirectory = fileRootDirectory,
        logFolder = logFolder
      )
    },

    #' @description
    #' Add table results to a
    #' @param reportFile file in which the table and its caption should be added
    #' @param fileRelativePath path of file relative to reportFile corresponding saved figure
    #' @param fileRootDirectory root directory of figure file path
    #' @param digits number of decimal digits in displayed numbers
    #' @param scientific logical defining if displayed numbers use scientific writing
    #' @param logFolder folder were logs are saved
    addTableToReport = function(reportFile, fileRelativePath, fileRootDirectory, digits = NULL, scientific = NULL, logFolder = getwd()) {
      if (isOfLength(self$table, 0)) {
        return()
      }
      # includeTable select if the table and its caption should be included in final report
      # However, tables in task results are saved no matter this field
      if (isFALSE(self$includeTable)) {
        return()
      }
      if (!isOfLength(self$tableCaption, 0)) {
        addTextChunk(reportFile, paste0("Table: ", self$tableCaption), logFolder = logFolder)
      }
      addTableChunk(
        fileName = reportFile,
        tableFileRelativePath = fileRelativePath,
        tableFileRootDirectory = fileRootDirectory,
        digits = digits,
        scientific = scientific,
        logFolder = logFolder
      )
    },


    #' @description
    #' Add a text chunk generated from a task to a
    #' @param reportFile file to which the text chunk should be added
    #' @param logFolder folder were logs are saved
    addTextChunkToReport = function(reportFile, logFolder = getwd()) {
      if (isOfLength(self$textChunk, 0)) {
        return()
      }
      if (isFALSE(self$includeTextChunk)) {
        return()
      }
      addTextChunk(reportFile, self$textChunk, logFolder = logFolder)
    }
  )
)

#' @title saveTaskResults
#' @description  Save task results within a `TaskResults` object
#' @param id unique identifer of results, helps in deriving final plot name
#' @param sectionId unique identifer of section results, used only by qualification workflows
#' @param plot `ggplot` object corresponding to the figure to be saved
#' @param plotCaption text included into the report explaining the figure
#' @param includePlot logical indicating if the plot should be included in final report
#' @param table data.frame corresponding to the table to be saved
#' @param tableCaption text included into the report explaining the table
#' @param includeTable logical indicating if the table should be included in final report
#' @param textChunk text included into the report explaining the table
#' @param includeTextChunk logical indicating if the text chunk should be included in final report
#' @return A `TaskResults` object
#' @keywords internal
saveTaskResults <- function(id = NULL, sectionId = NULL, plot = NULL, plotCaption = NULL, includePlot = NULL, table = NULL, tableCaption = NULL, includeTable = NULL, textChunk = NULL, includeTextChunk = NULL, taskResults = NULL) {
  taskResults <- taskResults %||% TaskResults$new()
  eval(parseVariableToObject(
    objectName = "taskResults",
    variableName = c("id", "sectionId", "plot", "plotCaption", "includePlot", "table", "tableCaption", "includeTable", "textChunk", "includeTextChunk"),
    keepIfNull = TRUE
  ))
  return(taskResults)
}
