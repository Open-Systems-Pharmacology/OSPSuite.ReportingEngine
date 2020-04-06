#' @title resetReport
#' @description Initialize a R markdown document by Writing its header in YAML
#' @param fileName name of .md file to reset
#' @param logFolder folder where the logs are saved
#' @export
resetReport <- function(fileName,
                        logFolder = getwd()) {
  if (file.exists(fileName)) {
    logWorkflow(
      message = paste0("'", fileName, "' already exists. Overwriting '", fileName, "'."),
      pathFolder = logFolder,
      logTypes = c(LogTypes$Info, LogTypes$Debug, LogTypes$Error)
    )
  }
  write("", file = fileName, sep = "\n")
  logWorkflow(
    message = paste0("Report '", fileName, "' was initialized successfully."),
    pathFolder = logFolder
  )
}

#' @title addFigureChunk
#' @description Add a figure in a .md document
#' @param fileName name of .md file
#' @param figureFile figure path to include
#' @param figureCaption caption of figure
#' @param figureWidth figure width within report. Default is "100%"
#' @param logFolder folder where the logs are saved
#' @export
addFigureChunk <- function(fileName,
                           figureFile,
                           figureCaption = "",
                           figureWidth = "100%",
                           logFolder = getwd()) {
  
  mdText <- c(
    "",
    knitr::hook_plot_md(figureFile,
                        knitr::opts_chunk$merge(list(out.width=figureWidth, include=TRUE, fig.align="center", echo=FALSE))),
    "")

  write(mdText, file = fileName, append = TRUE, sep = "\n")
  logWorkflow(
    message = paste0("Figure path '", figureFile, "' added to report '", fileName, "'."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
}

#' @title addTableChunk
#' @description Add a table in a .md document
#' @param fileName name of .md file
#' @param tableFile table path to include
#' @param tableCaption caption of the table
#' @param logFolder folder where the logs are saved
#' @export
addTableChunk <- function(fileName,
                             tableFile,
                             tableCaption = "",
                             logFolder = getwd()) {
  
  # TO DO: kable has options such as number of decimals and align, 
  # should they also be defined ? as figure width for addFigureChunk ?
  mdText <- c(
    "",
    knitr::kable(table),
    ""
  )

  write(mdText, file = fileName, append = TRUE, sep = "\n")
  logWorkflow(
    message = paste0("Table path '", tableFile, "' added to report '", fileName, "'."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
}

#' @title addTextChunk
#' @description Add a text chunk to a .md document
#' @param fileName name of .md file
#' @param text text to include in the document
#' @param logFolder folder where the logs are saved
#' @export
addTextChunk <- function(fileName,
                         text,
                         logFolder = getwd()) {
  write(c(
    "",
    text,
    ""
  ),
  file = fileName, append = TRUE, sep = "\n"
  )
  logWorkflow(
    message = paste0("Text '", text, "' added to report '", fileName, "'."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
}
