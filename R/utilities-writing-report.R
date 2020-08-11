#' @title resetReport
#' @description Initialize a report, warning if a previous version already exists
#' @param fileName name of .md file to reset
#' @param logFolder folder where the logs are saved
#' @export
resetReport <- function(fileName,
                        logFolder = getwd()) {
  if (file.exists(fileName)) {
    logWorkflow(
      message = paste0("'", fileName, "' already exists. Overwriting '", fileName, "'."),
      pathFolder = logFolder
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
  resetReport(outputFile, logFolder)

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
#' @param createWordReport option for creating Markdwon-Report only but not a Word-Report
#' @export
renderReport <- function(fileName, logFolder = getwd(), createWordReport = FALSE) {
  numberTablesAndFigures(fileName, logFolder)
  tocContent <- numberSections(fileName, logFolder)
  renderWordReport(fileName, logFolder, createWordReport)
  addMarkdownToc(tocContent, fileName, logFolder)
  return(invisible())
}

#' @title renderWordReport
#' @description Render docx report with number sections and table of content
#' @param fileName name of .md file to render
#' @param logFolder folder where the logs are saved
#' @param createWordReport option for creating Markdwon-Report only but not a Word-Report
#' @export
renderWordReport <- function(fileName, logFolder = getwd(), createWordReport = FALSE) {
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

  if (createWordReport) {
    templateReport <- system.file("extdata", "reference.docx", package = "ospsuite.reportingengine")
    pageBreakCode <- system.file("extdata", "pagebreak.lua", package = "ospsuite.reportingengine")

    write(c(
      "self-contained:", "wrap: none", "toc:",
      paste0('reference-doc: "', templateReport, '"'),
      paste0('lua-filter: "', pageBreakCode, '"'),
      paste0('resource-path: "', logFolder, '"')
    ), file = reportConfig, sep = "\n")
    knitr::pandoc(input = wordFileName, format = "docx", config = reportConfig)
    file.copy(docxWordFileName, docxFileName, overwrite = TRUE)
    unlink(reportConfig, recursive = TRUE)
    unlink(docxWordFileName, recursive = TRUE)
  }
  logWorkflow(
    message = paste0("Word version of report '", fileName, "' created."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title numberTablesAndFigures
#' @description Reference tables and figures in a report
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @param figurePattern character pattern referencing figures in first element of line
#' @param tablePattern character pattern referencing tables in first element of line
numberTablesAndFigures <- function(fileName, logFolder = getwd(), figurePattern = "Figure:", tablePattern = "Table:") {
  fileContent <- readLines(fileName, encoding = "UTF-8")

  figureCount <- 0
  tableCount <- 0
  for (lineIndex in seq_along(fileContent)) {
    firstElement <- as.character(unlist(strsplit(fileContent[lineIndex], " ")))
    firstElement <- firstElement[1]
    if (grepl(pattern = figurePattern, x = firstElement)) {
      figureCount <- figureCount + 1
      fileContent[lineIndex] <- gsub(pattern = figurePattern, replacement = paste0("Figure ", figureCount, ":"), x = fileContent[lineIndex])
    }
    if (grepl(pattern = tablePattern, x = firstElement)) {
      tableCount <- tableCount + 1
      fileContent[lineIndex] <- gsub(pattern = tablePattern, replacement = paste0("Table ", tableCount, ":"), x = fileContent[lineIndex])
    }
  }

  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("In '", fileName, "', ", tableCount, " tables and ", figureCount, " figures were referenced."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title numberSections
#' @description Reference sections of a report
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @param tocPattern character pattern referencing sections in first element of line
#' @param tocLevels levels of sections in the report
#' @return Table of content referencing sections following a markdown format
numberSections <- function(fileName, logFolder = getwd(), tocPattern = "#", tocLevels = 3) {
  fileContent <- readLines(fileName, encoding = "UTF-8")

  # Initialize toc content
  tocContent <- NULL
  tocPatterns <- NULL
  tocCounts <- rep(0, tocLevels)

  for (tocLevel in seq(1, tocLevels)) {
    tocPatterns <- c(tocPatterns, paste0(rep(tocPattern, tocLevel), collapse = ""))
  }

  for (lineIndex in seq_along(fileContent)) {
    firstElement <- as.character(unlist(strsplit(fileContent[lineIndex], " ")))
    firstElement <- firstElement[1]
    for (tocLevel in rev(seq(1, tocLevels))) {
      if (grepl(pattern = tocPatterns[tocLevel], x = firstElement)) {
        tocCounts[tocLevel] <- tocCounts[tocLevel] + 1
        if (tocLevel < tocLevels) {
          tocCounts[seq(tocLevel + 1, tocLevels)] <- 0
        }

        # Number section
        titlePattern <- paste0(tocPatterns[tocLevel], " ")
        newTitlePattern <- paste0(tocCounts[seq(1, tocLevel)], collapse = ".")
        newTitlePattern <- paste0(titlePattern, newTitlePattern, ". ")
        fileContent[lineIndex] <- gsub(pattern = titlePattern, replacement = newTitlePattern, x = fileContent[lineIndex])

        # Add section reference to toc content
        titleTocContent <- sub(pattern = titlePattern, replacement = "", x = fileContent[lineIndex])
        titleTocReference <- gsub(pattern = "[^[:alnum:][:space:]\\_'-]", replacement = "", x = tolower(titleTocContent))
        titleTocReference <- gsub(pattern = "[[:space:]*]", replacement = "-", x = titleTocReference)
        tocLevelShift <- paste0(rep(" ", tocLevel), collapse = " ")
        tocContent <- c(tocContent, paste0(tocLevelShift, "* [", titleTocContent, "](#", titleTocReference, ")"))

        break
      }
    }
  }

  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("In '", fileName, "', ", tocCounts[1], " main sections were referenced"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(tocContent)
}

#' @title addMarkdownToc
#' @description Add table of content to a markdown file
#' @param tocContent Table of content referencing sections following a markdown format
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
addMarkdownToc <- function(tocContent, fileName, logFolder = getwd()) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  fileContent <- c(tocContent, fileContent)
  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("In '", fileName, "', table of content included."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}
