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
  fileObject <- file(fileName, encoding = "UTF-8")
  write("", file = fileObject, sep = "\n")
  close(fileObject)
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
                           logFolder = getwd()) {
  # For a figure path to be valid in markdown using ![](#figurePath)
  # %20 needs to replace spaces in that figure path
  mdFigureFile <- gsub(pattern = "[[:space:]*]", replacement = "%20", x = figureFile)
  mdText <- c(
    "",
    paste0("![", figureCaption, "](", mdFigureFile, ")"),
    ""
  )

  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(mdText, file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)
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
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  # TO DO: kable has options such as number of decimals and align,
  # should they also be defined ? as figure width for addFigureChunk ?
  mdText <- c(
    "",
    knitr::kable(table),
    ""
  )

  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(mdText, file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)
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
  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(c("", text, ""), file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)
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
    fileContent <- readLines(fileName, encoding = "UTF-8")
    addTextChunk(outputFile, fileContent, logFolder = logFolder)
  }

  logWorkflow(
    message = paste0("Reports '", paste0(inputFiles, collapse = "', '"), "' were successfully merged into '", outputFile, "'"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title renderReport
#' @description Render report with number sections and table of content
#' @param fileName name of .md file to render
#' @param logFolder folder where the logs are saved
#' @export
renderReport <- function(fileName, logFolder = getwd()) {
  # Numbering of '#', '##', '###', figures and tables
  # The method below might not be the most efficient:
  fileContent <- readLines(fileName, encoding = "UTF-8")
  tocContent <- NULL
  figureCount <- 0
  tableCount <- 0
  titleCount <- 0
  subtitleCount <- 0
  subsubtitleCount <- 0
  for (lineIndex in seq_along(fileContent)) {
    firstElement <- as.character(unlist(strsplit(fileContent[lineIndex], " ")))
    firstElement <- firstElement[1]
    if (grepl(pattern = "Figure:", x = firstElement)) {
      figureCount <- figureCount + 1
      fileContent[lineIndex] <- gsub(pattern = "Figure:", replacement = paste0("Figure ", figureCount, ":"), x = fileContent[lineIndex])
    }
    if (grepl(pattern = "Table:", x = firstElement)) {
      tableCount <- tableCount + 1
      fileContent[lineIndex] <- gsub(pattern = "Table:", replacement = paste0("Table ", tableCount, ":"), x = fileContent[lineIndex])
    }
    if (grepl(pattern = "#", x = firstElement) & !grepl("##", firstElement)) {
      titleCount <- titleCount + 1
      subtitleCount <- 0
      subsubtitleCount <- 0
      fileContent[lineIndex] <- gsub(pattern = "# ", replacement = paste0("# ", titleCount, ". "), x = fileContent[lineIndex])
      titleTocContent <- sub(pattern = "# ", replacement = "", x = fileContent[lineIndex])
      titleTocReference <- gsub(pattern = "[[:space:]*]", replacement = "-", x = tolower(titleTocContent))
      tocContent <- c(tocContent, paste0(" - [", titleTocContent, "](#", titleTocReference, ")"))
    }
    if (grepl(pattern = "##", x = firstElement) & !grepl("###", firstElement)) {
      subtitleCount <- subtitleCount + 1
      subsubtitleCount <- 0
      fileContent[lineIndex] <- gsub(pattern = "## ", replacement = paste0("## ", titleCount, ".", subtitleCount, ". "), x = fileContent[lineIndex])
      titleTocContent <- sub(pattern = "## ", replacement = "", x = fileContent[lineIndex])
      titleTocReference <- gsub(pattern = "[[:space:]*]", replacement = "-", x = tolower(titleTocContent))
      tocContent <- c(tocContent, paste0("   - [", titleTocContent, "](#", titleTocReference, ")"))
    }
    if (grepl(pattern = "###", x = firstElement) & !grepl("####", firstElement)) {
      subsubtitleCount <- subsubtitleCount + 1
      fileContent[lineIndex] <- gsub(pattern = "### ", replacement = paste0("### ", titleCount, ".", subtitleCount, ".", subsubtitleCount, ". "), x = fileContent[lineIndex])
      titleTocContent <- sub(pattern = "### ", replacement = "", x = fileContent[lineIndex])
      titleTocReference <- gsub(pattern = "[[:space:]*]", replacement = "-", x = tolower(titleTocContent))
      tocContent <- c(tocContent, paste0("   - [", titleTocContent, "](#", titleTocReference, ")"))
    }
  }
  # Include table of content and update file
  fileContent <- c(tocContent, fileContent)
  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("Numbering of sections and table of content added to Report '", fileName, "'"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  # Render docx report in the process
  renderWordReport(fileName, logFolder)

  return(invisible())
}

#' @title renderWordReport
#' @description Render docx report with number sections and table of content
#' @param fileName name of .md file to render
#' @param logFolder folder where the logs are saved
#' @export
renderWordReport <- function(fileName, logFolder = getwd()) {
  reportConfig <- file.path(logFolder, "word-report-configuration.txt")
  wordFileName <- sub(pattern = ".md", replacement = "-word.md", fileName)
  docxWordFileName <- sub(pattern = ".md", replacement = "-word.docx", fileName)
  docxFileName <- sub(pattern = ".md", replacement = ".docx", fileName)
  fileContent <- readLines(fileName, encoding = "UTF-8")

  wordFileContent <- NULL
  for (lineContent in fileContent) {
    firstElement <- as.character(unlist(strsplit(lineContent, " ")))
    firstElement <- firstElement[1]
    if (grepl(pattern = "Figure", x = firstElement) || grepl(pattern = "Table", x = firstElement)) {
      wordFileContent <- c(wordFileContent, "\\newpage")
    }
    wordFileContent <- c(wordFileContent, lineContent)
  }

  fileObject <- file(wordFileName, encoding = "UTF-8")
  write(wordFileContent, file = fileObject, sep = "\n")
  close(fileObject)

  templateReport <- system.file("extdata", "reference.docx", package = "ospsuite.reportingengine")
  pageBreakCode <- system.file("extdata", "pagebreak.lua", package = "ospsuite.reportingengine")

  write(c(
    "self-contained:", "wrap: none",
    paste0("reference-doc: ", templateReport),
    paste0("lua-filter: ", pageBreakCode),
    paste0("resource-path: ", logFolder)
  ), file = reportConfig, sep = "\n")
  knitr::pandoc(input = wordFileName, format = "docx", config = reportConfig)
  file.copy(docxWordFileName, docxFileName)
  unlink(reportConfig, recursive = TRUE)
  unlink(docxWordFileName, recursive = TRUE)

  logWorkflow(
    message = paste0("Word version of report '", fileName, "' created."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}
