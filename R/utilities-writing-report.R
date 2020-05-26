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
  return(invisible())
}

#' @title addFigureChunk
#' @description Add a figure in a .md document
#' @param fileName name of .md file
#' @param figureFile figure path to include
#' @param figureCaption caption of figure
#' @param figureWidth figure width within report. Default is 100 percent.
#' @param logFolder folder where the logs are saved
#' @export
addFigureChunk <- function(fileName,
                           figureFile,
                           figureCaption = "",
                           figureWidth = "100%",
                           logFolder = getwd()) {
  mdText <- c(
    "",
    knitr::hook_plot_md(
      figureFile,
      knitr::opts_chunk$merge(list(out.width = figureWidth, include = TRUE, fig.align = "center", echo = FALSE))
    ),
    ""
  )

  write(mdText, file = fileName, append = TRUE, sep = "\n")
  logWorkflow(
    message = paste0("Figure path '", figureFile, "' added to report '", fileName, "'."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
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
  table <- read.csv(tableFile,
    check.names = FALSE
  )

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
  return(invisible())
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
  return(invisible())
}

#' @title mergeMarkdowndFiles
#' @description Merge all appendices reports into one final report
#' @param inputFiles name of .md files to merge
#' @param outputFile text to include in the document
#' @param logFolder folder where the logs are saved
#' @export
mergeMarkdowndFiles <- function(inputFiles, outputFile, logFolder = getwd()) {
  resetReport(outputFile)

  for (fileName in inputFiles) {
    fileContent <- readLines(fileName)
    addTextChunk(outputFile, fileContent, logFolder = logFolder)
  }

  logWorkflow(
    message = paste0("Reports '", paste0(inputFiles, collapse = "', '"), "' were successfully merged into '", outputFile, "'"),
    pathFolder = logFolder
  )
  return(invisible())
}

#' @title renderReport
#' @description Render report with number sections and table of content
#' @param fileName name of .md file to render
#' @param logFolder folder where the logs are saved
#' @export
renderReport <- function(fileName, logFolder = getwd()) {
  reportConfig <- file.path(logFolder, "reportConfig.txt")

  # Numbering of '#', '##' and figures
  # The method below might not be the most efficient
  fileContent <- readLines(fileName)
  firstElements <- NULL
  for (lineContent in fileContent) {
    firstElements <- cbind(firstElements, unlist(strsplit(lineContent, " "))[1])
  }
  firstElements <- as.character(firstElements)

  figureCount <- 1
  titleCount <- 1
  subtitleCount <- 1
  for (lineIndex in seq_along(fileContent)) {
    if (firstElements[lineIndex] %in% "Figure:") {
      fileContent[lineIndex] <- gsub(pattern = "Figure:", replacement = paste0("Figure ", figureCount, ":"), x = fileContent[lineIndex])
      figureCount <- figureCount + 1
    }
    if (firstElements[lineIndex] %in% "#") {
      fileContent[lineIndex] <- gsub(pattern = "#", replacement = paste0("# ", titleCount, ". "), x = fileContent[lineIndex])
      titleCount <- titleCount + 1
      subtitleCount <- 1
    }
    if (firstElements[lineIndex] %in% "##") {
      fileContent[lineIndex] <- gsub(pattern = "##", replacement = paste0("## ", titleCount, ".", titleCount, ". "), x = fileContent[lineIndex])
      subtitleCount <- subtitleCount + 1
    }
  }
  
  # Update file
  write(fileContent, file = fileName, sep = "\n")

  # Table of content
  write(c("toc: ", "self-contained:"), file = reportConfig, sep = "\n")
  knitr::pandoc(input = fileName, format = "gfm", config = reportConfig, ext = "md")
  unlink(reportConfig, recursive = TRUE)

  logWorkflow(
    message = paste0("Table of content added to Report '", fileName, "'"),
    pathFolder = logFolder
  )
  return(invisible())
}
