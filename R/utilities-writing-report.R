#' @title resetReport
#' @description Initialize a report, warning if a previous version already exists
#' @param fileName name of .md file to reset
#' @param logFolder folder where the logs are saved
#' @export
#' @examples
#' resetReport("report.md")
resetReport <- function(fileName, logFolder = getwd()) {
  if (file.exists(fileName)) {
    logWorkflow(
      message = paste0("'", fileName, "' already exists. Overwriting '", fileName, "'."),
      pathFolder = logFolder
    )
  }
  fileObject <- file(fileName, encoding = "UTF-8")
  write("", file = fileObject, sep = "\n")
  close(fileObject)
  return(invisible())
}

#' @title addFigureChunk
#' @description Add a figure in a .md document
#' @param fileName name of .md file
#' @param figureFileRelativePath path to figure relative to working directory
#' @param figureFileRootDirectory working directory
#' @param figureCaption caption of figure
#' @param logFolder folder where the logs are saved
#' @export
addFigureChunk <- function(fileName,
                           figureFileRelativePath,
                           figureFileRootDirectory,
                           figureCaption = "",
                           logFolder = getwd()) {
  # For a figure path to be valid in markdown using ![](#figurePath)
  # %20 needs to replace spaces in that figure path
  mdFigureFile <- gsub(pattern = "[[:space:]*]", replacement = "%20", x = figureFileRelativePath)
  mdText <- c(
    "",
    paste0("![", figureCaption, "](", mdFigureFile, ")"),
    ""
  )

  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(mdText, file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)

  usedFilesFileName <- sub(pattern = ".md", replacement = "-usedFiles.txt", fileName)
  fileObject <- file(usedFilesFileName, open = "at", encoding = "UTF-8")
  write(file.path(figureFileRootDirectory, figureFileRelativePath), file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("Figure path '", figureFileRelativePath, "' added to report '", fileName, "'."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title addTableChunk
#' @description Add a table in a .md document
#' @param fileName name of .md file
#' @param tableFileRelativePath path to table relative to working directory
#' @param tableFileRootDirectory working directory
#' @param digits number of decimal digits in displayed numbers
#' @param scientific logical defining if displayed numbers use scientific writing
#' @param logFolder folder where the logs are saved
#' @param na character string replacing `NA` values in table
#' @export
#' @importFrom ospsuite.utils %||%
addTableChunk <- function(fileName,
                          tableFileRelativePath,
                          tableFileRootDirectory,
                          digits = NULL,
                          scientific = NULL,
                          logFolder = getwd(),
                          na = "-") {
  # The function `formatNumerics` is now used by addTableChunk
  # colClasses = "character" is not needed anymore to enforce all table elements to be 'character'
  table <- read.csv(
    file.path(tableFileRootDirectory, tableFileRelativePath),
    check.names = FALSE,
    #colClasses = "character",
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE
  )
  table <- ospsuite.utils::formatNumerics(
    table,
    digits = digits %||% reEnv$formatNumericsDigits,
    scientific = scientific %||% reEnv$formatNumericsScientific
    )

  # Currently using default options from kable
  mdText <- c(
    "",
    knitr::kable(table),
    ""
  )
  mdText <- gsub("NA", na, mdText)

  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(mdText, file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)

  usedFilesFileName <- sub(pattern = ".md", replacement = "-usedFiles.txt", fileName)
  fileObject <- file(usedFilesFileName, open = "at", encoding = "UTF-8")
  write(file.path(tableFileRootDirectory, tableFileRelativePath), file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)

  logWorkflow(
    message = paste0("Table path '", file.path(tableFileRootDirectory, tableFileRelativePath), "' added to report '", fileName, "'."),
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
#' @examples
#' resetReport("report.md")
#' addTextChunk(fileName = "report.md", text = "new text")
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
#' @description Merge markdown files into one unique file
#' @param inputFiles names of .md files to merge
#' @param outputFile name of merged .md file
#' @param logFolder folder where the logs are saved
#' @param keepInputFiles logical option to prevent the input files to be deleted after merging them
#' @export
#' @examples
#' resetReport("chapter-1.md")
#' addTextChunk(fileName = "chapter-1.md", text = "Chapter 1")
#' resetReport("chapter-2.md")
#' addTextChunk(fileName = "chapter-2.md", text = "Chapter 2")
#' mergeMarkdowndFiles(inputFiles = c("chapter-1.md", "chapter-2.md"), outputFile = "chapters-1and2.md")
mergeMarkdowndFiles <- function(inputFiles, outputFile, logFolder = getwd(), keepInputFiles = FALSE) {
  ospsuite.utils::validateIsLogical(keepInputFiles)
  # Read all files contents first in case outputFile is within inputFiles
  filesContent <- lapply(inputFiles, function(fileName){readLines(fileName, encoding = "UTF-8")})
  resetReport(outputFile, logFolder)

  # tracelib chunk of code
  usedFilesOutputFile <- sub(pattern = ".md", replacement = "-usedFiles.txt", outputFile)
  file.create(usedFilesOutputFile)
  for (fileName in inputFiles) {
    usedFilesFileName <- sub(pattern = ".md", replacement = "-usedFiles.txt", fileName)
    if (file.exists(usedFilesFileName)) {
      file.append(usedFilesOutputFile, usedFilesFileName)
      file.remove(usedFilesFileName)
    }
  }
  # Merge input files content
  invisible(lapply(filesContent, function(fileContent){addTextChunk(outputFile, fileContent, logFolder = logFolder)}))
  if (!keepInputFiles) {
    # Use setdiff to prevent erasing output file its name is included in inputFiles
    file.remove(setdiff(inputFiles, outputFile))
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
#' @param numberSections logical defining if sections are numbered
#' @param intro name of .md file that include introduction (before toc)
#' @export
renderReport <- function(fileName, logFolder = getwd(), createWordReport = FALSE, numberSections = TRUE, intro = NULL) {
  actionToken2 <- re.tStartAction(actionType = "ReportGeneration")
  numberTablesAndFigures(fileName, logFolder)
  # TODO: number sections and intro in word report
  renderWordReport(fileName, logFolder, createWordReport)
  tocContent <- getSectionTOC(fileName, logFolder, numberSections = numberSections)
  addMarkdownToc(tocContent, fileName, logFolder)
  mergeMarkdowndFiles(inputFiles = c(intro, fileName), outputFile = fileName, logFolder = logFolder)
  re.tEndAction(actionToken = actionToken2)
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
  figureContent <- NULL
  for (lineContent in fileContent) {
    firstElement <- as.character(unlist(strsplit(lineContent, " ")))
    firstElement <- firstElement[1]
    if (grepl(pattern = "Figure", x = firstElement) || grepl(pattern = "Table", x = firstElement)) {
      wordFileContent <- c(wordFileContent, "\\newpage")
    }
    # Markdown needs to cut/paste figure content within ![]
    # The reports are currently built to print "Figure xx:" and below add figure path as "![](path)"
    # Consequently, the strategy is to read the figure content with key "Figure" and paste within key "![]"
    if (grepl(pattern = "Figure", x = firstElement)) {
      figureContent <- lineContent
      next
    }
    if (grepl(pattern = "\\!\\[\\]", x = lineContent)) {
      lineContent <- paste0("![", figureContent, "]", gsub(pattern = "\\!\\[\\]", replacement = "", x = lineContent))
      figureContent <- NULL
    }
    wordFileContent <- c(wordFileContent, lineContent)
  }

  usedFilesFileName <- sub(pattern = ".md", replacement = "-usedFiles.txt", fileName)
  usedFiles <- readLines(usedFilesFileName, encoding = "UTF-8")

  for (usedFile in usedFiles) {
    if (usedFile != "") {
      re.tStoreFileMetadata(access = "read", filePath = usedFile)
    }
  }
  file.remove(usedFilesFileName)

  fileObject <- file(wordFileName, encoding = "UTF-8")
  write(wordFileContent, file = fileObject, sep = "\n")
  close(fileObject)
  re.tStoreFileMetadata(access = "write", filePath = wordFileName)

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
    re.tStoreFileMetadata(access = "write", filePath = docxFileName)
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
#' @keywords internal
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

#' @title getSectionTOC
#' @description Reference sections of a report
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @param numberSections logical defining if numbering of section titles is performed automatically when getting the table of content.
#' When this option is `FALSE`, such as in qualification workflows,
#' all unnumbered section titles are skipped from the the table of content
#' @param tocPattern character pattern referencing sections in first element of line
#' @param tocLevels levels of sections in the report
#' @return Table of content referencing sections following a markdown format
#' @keywords internal
getSectionTOC <- function(fileName, logFolder = getwd(), numberSections = TRUE, tocPattern = "#", tocLevels = 6) {
  fileContent <- readLines(fileName, encoding = "UTF-8")

  # Initialize toc content
  tocContent <- NULL
  tocPatterns <- NULL
  tocCounts <- rep(0, tocLevels)

  for (tocLevel in seq(1, tocLevels)) {
    tocPatterns <- c(tocPatterns, paste0(rep(tocPattern, tocLevel), collapse = ""))
  }

  for (lineIndex in seq_along(fileContent)) {
    lineElements <- as.character(unlist(strsplit(fileContent[lineIndex], " ")))
    firstElement <- lineElements[1]
    secondElement <- lineElements[2]
    for (tocLevel in rev(seq(1, tocLevels))) {
      if (grepl(pattern = tocPatterns[tocLevel], x = firstElement)) {
        # Skip the section title if unnumbered and numberSection is FALSE
        if(!grepl(pattern = "[[:digit:]]", x = secondElement) & !numberSections){
          next
        }
        tocCounts[tocLevel] <- tocCounts[tocLevel] + 1
        if (tocLevel < tocLevels) {
          tocCounts[seq(tocLevel + 1, tocLevels)] <- 0
        }

        # Number section if option is true
        titlePattern <- paste0(tocPatterns[tocLevel], " ")
        if(numberSections){
          newTitlePattern <- paste0(tocCounts[seq(1, tocLevel)], collapse = ".")
          newTitlePattern <- paste0(titlePattern, newTitlePattern, " ")
          fileContent[lineIndex] <- gsub(pattern = titlePattern, replacement = newTitlePattern, x = fileContent[lineIndex])
        }

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
#' @keywords internal
addMarkdownToc <- function(tocContent, fileName, logFolder = getwd()) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  fileContent <- c("# Table of Contents", "", tocContent, fileContent)
  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)
  re.tStoreFileMetadata(access = "write", filePath = fileName)

  logWorkflow(
    message = paste0("In '", fileName, "', table of content included."),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title setSimulationDescriptor
#' @description Set workflow simulation set descriptor
#' @param workflow A `Workflow` object
#' @param text Character describing simulation sets
#' @export
#' @importFrom ospsuite.utils %||%
setSimulationDescriptor <- function(workflow, text) {
  ospsuite.utils::validateIsOfType(workflow, "Workflow")
  ospsuite.utils::validateIsString(text, nullAllowed = TRUE)

  # Allows NULL which is translated by ""
  workflow$setSimulationDescriptor(text %||% "")
  return(invisible())
}

#' @title getSimulationDescriptor
#' @description Get workflow simulation set descriptor
#' @param workflow A `Workflow` object
#' @return character describing simulation sets
#' @export
getSimulationDescriptor <- function(workflow) {
  ospsuite.utils::validateIsOfType(workflow, "Workflow")
  return(workflow$getSimulationDescriptor())
}
