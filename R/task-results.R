#' @title TaskResults
#' @description  R6 class for TaskResults settings
#' @field id unique identifier of results, helps in deriving final plot name
#' @field sectionId unique identifier of section results, used only by qualification workflows
#' @field plot `ggplot` object corresponding to the figure to be saved
#' @field plotCaption text included into the report explaining the figure
#' @field includePlot logical indicating if the plot should be included in final report
#' @field table data.frame corresponding to the table to be saved
#' @field tableCaption text included into the report explaining the table
#' @field includeTable logical indicating if the table should be included in final report
#' @field textChunk text included into the report explaining the table
#' @field includeTextChunk logical indicating if the text chunk should be included in final report
#' @import ospsuite.utils
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
    #' Save ggplot figure, available as plot task result, into a file
    #' @param fileName path of file corresponding to the figure to save
    saveFigure = function(fileName) {
      if (isEmpty(self$plot)) {
        return()
      }
      # TODO once every plot will use tlf, deprecate the condition for null values
      self$plot <- updatePlotDimensions(self$plot)
      # Use same dpi as RE environment settings to display correct point size
      # TODO: To remove after implementation in tlf
      if(requireNamespace("showtext", quietly = TRUE)){
        currentDPI <- showtext::showtext_opts()$dpi
        showtext::showtext_opts(dpi = reEnv$defaultPlotFormat$dpi)
      }
      ggplot2::ggsave(
        filename = fileName,
        plot = self$plot,
        width = self$plot$plotConfiguration$export$width %||% reEnv$defaultPlotFormat$width,
        height = self$plot$plotConfiguration$export$height %||% reEnv$defaultPlotFormat$height,
        dpi = reEnv$defaultPlotFormat$dpi,
        units = self$plot$plotConfiguration$export$units %||% reEnv$defaultPlotFormat$units
      )
      # Revert showtext settings
      # TODO: To remove after implementation in tlf
      if(requireNamespace("showtext", quietly = TRUE)){
        showtext::showtext_opts(dpi = currentDPI)
      }
      logDebug(paste0("Figure '", fileName, "' was successfully saved."))
      re.tStoreFileMetadata(access = "write", filePath = fileName)
    },

    #' @description
    #' Save data.frame, available as table task result, into a csv file
    #' @param fileName path of csv file corresponding to the table to save
    saveTable = function(fileName) {
      if (isEmpty(self$table)) {
        return()
      }
      write.csv(self$table, file = fileName, row.names = FALSE, fileEncoding = "UTF-8")
      logDebug(paste0("Table '", fileName, "' was successfully saved."))
      re.tStoreFileMetadata(access = "write", filePath = fileName)
    },

    #' @description
    #' Write markdown content that adds a figure, available as plot task result, into a markdown report
    #' To be displayed, figure path must be relative to report location
    #' @param reportFile markdown file in which the figure and its caption should be added
    #' @param fileRelativePath figure path relative to `reportFile` location
    #' @param fileRootDirectory root/working directory needed by `tracelib` package
    addFigureToReport = function(reportFile, fileRelativePath, fileRootDirectory) {
      if (isEmpty(self$plot)) {
        return()
      }
      # includePlot select if the plot and its caption should be included in final report
      # However, tables in task results are saved no matter this field
      if (isFALSE(self$includePlot)) {
        return()
      }
      if (!isEmpty(self$plotCaption)) {
        addTextChunk(reportFile, paste0("**Figure: ", self$plotCaption, "**"))
      }
      addFigureChunk(
        fileName = reportFile,
        figureFileRelativePath = fileRelativePath,
        figureFileRootDirectory = fileRootDirectory
      )
      # Enforce 2 blank lines using html notation to improve report clarity
      addTextChunk(reportFile, rep("<br>", reEnv$blankLinesBetweenArtifacts))
    },

    #' @description
    #' Write markdown content that adds a data.frame, available as table task result, into a markdown report
    #' @param reportFile markdown file in which the table and its caption should be added
    #' @param fileRelativePath table path relative to `reportFile` location
    #' @param fileRootDirectory root/working directory needed by `tracelib` package
    #' @param digits number of decimal digits in displayed numbers
    #' @param scientific logical defining if displayed numbers use scientific writing
    addTableToReport = function(reportFile, fileRelativePath, fileRootDirectory, digits = NULL, scientific = NULL) {
      if (isEmpty(self$table)) {
        return()
      }
      # includeTable select if the table and its caption should be included in the markdown report
      # tables in task results are still saved but won't appear in the markdown report
      if (isFALSE(self$includeTable)) {
        return()
      }
      if (!isEmpty(self$tableCaption)) {
        addTextChunk(reportFile, paste0("**Table: ", self$tableCaption, "**"))
      }
      addTableChunk(
        fileName = reportFile,
        tableFileRelativePath = fileRelativePath,
        tableFileRootDirectory = fileRootDirectory,
        digits = digits,
        scientific = scientific
      )
      # Enforce blank lines using html notation to improve report clarity
      addTextChunk(reportFile, rep("<br>", reEnv$blankLinesBetweenArtifacts))
    },


    #' @description
    #' Write markdown content that adds text, available as textChunk task result, into a markdown report
    #' @param reportFile markdown file in which the text should be added
    addTextChunkToReport = function(reportFile) {
      if (isEmpty(self$textChunk)) {
        return()
      }
      if (isFALSE(self$includeTextChunk)) {
        return()
      }
      addTextChunk(reportFile, self$textChunk)
    }
  )
)

#' @title saveTaskResults
#' @description  Save task results within a `TaskResults` object
#' @param id unique identifier of results, helps in deriving final plot name
#' @param sectionId unique identifier of section results, used only by qualification workflows
#' @param plot `ggplot` object corresponding to the figure to be saved
#' @param plotCaption text included into the report explaining the figure
#' @param includePlot logical indicating if the plot should be included in final report
#' @param table data.frame corresponding to the table to be saved
#' @param tableCaption text included into the report explaining the table
#' @param includeTable logical indicating if the table should be included in final report
#' @param textChunk text included into the report explaining the table
#' @param includeTextChunk logical indicating if the text chunk should be included in final report
#' @param taskResults A `TaskResults` object
#' @return A `TaskResults` object
#' @import ospsuite.utils
#' @export
saveTaskResults <- function(id = NULL, sectionId = NULL, plot = NULL, plotCaption = NULL, includePlot = NULL, table = NULL, tableCaption = NULL, includeTable = NULL, textChunk = NULL, includeTextChunk = NULL, taskResults = NULL) {
  taskResults <- taskResults %||% TaskResults$new()
  eval(parseVariableToObject(
    objectName = "taskResults",
    variableName = c("id", "sectionId", "plot", "plotCaption", "includePlot", "table", "tableCaption", "includeTable", "textChunk", "includeTextChunk"),
    keepIfNull = TRUE
  ))
  return(taskResults)
}
