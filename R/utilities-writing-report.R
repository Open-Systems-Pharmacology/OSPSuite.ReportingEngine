#' @title resetReport
#' @description Initialize a report, warning if a previous version already exists
#' @param fileName name of .md file to reset
#' @param logFolder folder where the logs are saved
#' @export
#' @examples
#' \dontrun{
#' resetReport("report.md")
#' }
resetReport <- function(fileName, logFolder = getwd()) {
  if (file.exists(fileName)) {
    logWorkflow(
      message = paste0("'", fileName, "' already exists. Overwriting '", fileName, "'."),
      pathFolder = logFolder,
      logTypes = LogTypes$Debug
    )
  }
  # When write() uses sep = "\n", 
  # Every element of the array input in write() is added in a new line
  # Thus, only "" is needed to create a new line
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
  table <- formatNumerics(
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
#' \dontrun{
#' resetReport("report.md")
#' addTextChunk(fileName = "report.md", text = "new text")
#' }
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

#' @title mergeMarkdownFiles
#' @description Merge markdown files into one unique file
#' @param inputFiles names of .md files to merge
#' @param outputFile name of merged .md file
#' @param logFolder folder where the logs are saved
#' @param keepInputFiles logical option to prevent the input files to be deleted after merging them
#' @export
#' @examples
#' \dontrun{
#' resetReport("chapter-1.md")
#' addTextChunk(fileName = "chapter-1.md", text = "Chapter 1")
#' resetReport("chapter-2.md")
#' addTextChunk(fileName = "chapter-2.md", text = "Chapter 2")
#' mergeMarkdownFiles(
#'  inputFiles = c("chapter-1.md", "chapter-2.md"), 
#'  outputFile = "chapters-1and2.md"
#' )
#' }
mergeMarkdownFiles <- function(inputFiles, outputFile, logFolder = getwd(), keepInputFiles = FALSE) {
  validateIsLogical(keepInputFiles)
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
#' @param wordConversionTemplate optional docx template for rendering a tuned Word-Report document
#' @export
renderReport <- function(fileName, logFolder = getwd(), createWordReport = FALSE, numberSections = TRUE, intro = NULL, wordConversionTemplate = NULL) {
  actionToken2 <- re.tStartAction(actionType = "ReportGeneration")
  addTableAndFigureNumbersToMarkdown(fileName, logFolder)
  # When rendering word report, the pandoc command toc automatically number the sections
  # Thus, report-word.md needs to be created before numbering the markdown sections
  renderWordReport(fileName, intro = intro, logFolder, createWordReport, wordConversionTemplate)
  if(numberSections){
    addSectionNumbersToMarkdown(fileName, logFolder)
  }
  addMarkdownToc(fileName, logFolder)
  mergeMarkdownFiles(inputFiles = c(intro, fileName), outputFile = fileName, logFolder = logFolder)
  re.tEndAction(actionToken = actionToken2)
  return(invisible())
}

#' @title renderWordReport
#' @description Render docx report with number sections and table of content
#' @param fileName name of .md file to render
#' @param intro name of .md file that include introduction (before toc)
#' @param logFolder folder where the logs are saved
#' @param createWordReport option for creating Markdwon-Report only but not a Word-Report
#' @param wordConversionTemplate optional docx template for rendering a tuned Word-Report document
#' @export
renderWordReport <- function(fileName, intro = NULL, logFolder = getwd(), createWordReport = FALSE, wordConversionTemplate = NULL) {
  reportConfig <- file.path(logFolder, "word-report-configuration.txt")
  wordFileName <- sub(pattern = ".md", replacement = "-word.md", fileName)
  docxWordFileName <- sub(pattern = ".md", replacement = "-word.docx", fileName)
  docxFileName <- sub(pattern = ".md", replacement = ".docx", fileName)
  fileContent <- readLines(fileName, encoding = "UTF-8")

  wordFileContent <- NULL
  figureContent <- NULL
  for (lineContent in fileContent) {
    firstElement <- getFirstLineElement(lineContent)
    # When finding a line referencing a table caption,
    if (grepl(pattern = "Table", x = firstElement)) {
      # The new content to write in the report is
      # - previous content = wordFileContent
      # - page break = "\\newpage"
      # - table caption = lineContent
      # - line space = "" (due to sep="\n" in function write)
      wordFileContent <- c(wordFileContent, "\\newpage", lineContent, "")
      next
    }
    # Figure: caption is after figure linked with "![](path)". Thus, break page is added before definition of figure path
    # For word report, it needs to be merged as "![caption](path)"
    if (grepl(pattern = "\\!\\[\\]", x = firstElement)) {
      # Store link to figure path in figureContent
      figureContent <- lineContent
      next
    }
    # When finding a line referencing a figure caption,
    if (grepl(pattern = "Figure", x = firstElement) & !is.null(figureContent)) {
      # The new content to write in the report is
      # - previous content = wordFileContent
      # - page break = "\\newpage"
      # - figure and its caption = "![lineContent](figureContent)"
      # with lineContent = figure caption and figureContent = figure link
      wordFileContent <- c(
        wordFileContent,
        "\\newpage",
        paste0("![", lineContent, "]", gsub(pattern = "\\!\\[\\]", replacement = "", x = figureContent))
        )
      figureContent <- NULL
      next
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

  # Include introduction as a yaml header passed as additional arguments to pandoc
  if(!isEmpty(intro)){
    introContent <- readLines(intro, encoding = "UTF-8")
    yamlContent <- introToYamlHeader(introContent)
    
    wordFileContent <- c(
      # Cover page features
      yamlContent,
      "",
      wordFileContent
    )
  }
  # Since write() uses sep = "\n", 
  # every element of array wordFileContent is added in a new line
  fileObject <- file(wordFileName, encoding = "UTF-8")
  write(wordFileContent, file = fileObject, sep = "\n")
  close(fileObject)
  re.tStoreFileMetadata(access = "write", filePath = wordFileName)

  if (createWordReport) {
    # Check if pandoc is available before trying to render word report
    tryCatch({
      command <- "pandoc --version"
      status <- system(command, show.output.on.console = FALSE)
      validateCommandStatus(command, status)
    }, error = function(e) {
      logWorkflow(
        message = "Pandoc is not installed, word report was not created",
        pathFolder = logFolder,
        logTypes = c(LogTypes$Error, LogTypes$Debug)
      )
      return(invisible())
    })

    wordConversionTemplate <- wordConversionTemplate %||% system.file("extdata", "reference.docx", package = "ospsuite.reportingengine")
    pageBreakCode <- system.file("extdata", "pagebreak.lua", package = "ospsuite.reportingengine")

    write(c(
      "self-contained:", "wrap: none", "toc:",
      paste0('reference-doc: "', wordConversionTemplate, '"'),
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

#' @title addTableAndFigureNumbersToMarkdown
#' @description Reference tables and figures in a report
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @keywords internal
addTableAndFigureNumbersToMarkdown <- function(fileName, logFolder = getwd()) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  numberOfLines <- length(fileContent)
  
  fileContent <- updateFigureNumbers(fileContent)
  # Three new lines are added by referenced figure
  figureCount <- (length(fileContent)-numberOfLines)/3
  numberOfLines <- length(fileContent)
  
  fileContent <- updateTableNumbers(fileContent)
  # Three new lines are added by referenced table
  tableCount <- (length(fileContent)-numberOfLines)/3
  numberOfLines <- length(fileContent)
  
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

#' @title addSectionNumbersToMarkdown
#' @description Update section numbers of a report
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @keywords internal
addSectionNumbersToMarkdown <- function(fileName, logFolder = getwd()){
  fileContent <- readLines(fileName, encoding = "UTF-8")
  titleInfo <- getTitleInfo(fileContent)
  for(title in titleInfo){
    titleNumber <- paste0(title$count[seq(1, title$level)], collapse = ".")
    titlePattern <- paste0(rep("#", title$level), collapse = "")
    fileContent[title$line] <- gsub(
      pattern = titlePattern,
      x = fileContent[title$line],
      replacement = paste(titlePattern, titleNumber)
    )
  }
  
  fileObject <- file(fileName, encoding = "UTF-8")
write(fileContent, file = fileObject, sep = "\n")
close(fileObject)

logWorkflow(
  message = paste0("In '", fileName, "', ", length(title), " sections were numbered"),
  pathFolder = logFolder,
  logTypes = LogTypes$Debug
)
return(invisible())
}


#' @title addMarkdownToc
#' @description Add table of content to a markdown file
#' @param tocContent Table of content referencing sections following a markdown format
#' @param fileName name of .md file to update
#' @param logFolder folder where the logs are saved
#' @keywords internal
addMarkdownToc <- function(fileName, logFolder = getwd(), tocTitle = "# Table of Contents") {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  titleInfo <- getTitleInfo(fileContent)
  # For each title, create its entry in table of content
  # By adding as many spaces as levels, then *[display title](#anchorId)
  tocContent <- sapply(
    titleInfo,
    function(title){
      titlePattern <- paste0(paste0(rep("#", title$level), collapse = ""), " ")
      titleTocContent <- gsub(pattern = titlePattern, replacement = "", x = title$content)
      tocLevelShift <- paste0(rep(" ", title$level), collapse = " ")
      return(paste0(tocLevelShift, "* [", titleTocContent, "](#", title$reference, ")"))
    }
  )
  # Update file content by adding before
  # tocTitle: "# Table of Contents"
  # line break
  # content of the table of content
  # then the report content
  fileContent <- c(tocTitle, "", tocContent, "", fileContent)
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
  validateIsOfType(workflow, "Workflow")
  validateIsString(text, nullAllowed = TRUE)

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
  validateIsOfType(workflow, "Workflow")
  return(workflow$getSimulationDescriptor())
}

#' @title adjustTitlePage
#' @description Adust Qualification Version Information to be displayed on title page 
#' @param fileName name of .md file to update
#' @param qualificationVersionInfo A `QualificationVersionInfo`object defining Qualification Version Information to be displayed on title page
#' @export
adjustTitlePage <- function(fileName, qualificationVersionInfo = NULL) {
  validateIsOfType(qualificationVersionInfo, "QualificationVersionInfo", nullAllowed = TRUE)
  validateFileExists(fileName)
  # Does not adust title page if no QualificationVersionInfo
  if(isEmpty(qualificationVersionInfo)){
    return(invisible())
  }
  fileContent <- readLines(fileName, encoding = "UTF-8")
  fileContent <- qualificationVersionInfo$updateText(fileContent)
  
  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)
  return(invisible())
}

#' @title anchor
#' @description Create an anchor tag for markdown document
#' @param name Name/identifier of the anchor tag
#' @return A character string
#' @export
#' @examples 
#' anchor("section-1")
#' 
anchor <- function(name) {
  return(paste0('<a id="', name, '"></a>'))
}

#' @title hasAnchor
#' @description Check if a character string includes an anchor tag
#' @param tag Character string
#' @return A logical
#' @export
#' @examples 
#' # Flags both anchors using id or name
#' hasAnchor('<a id="section-1"></a>')
#' hasAnchor('<a name="section-1"></a>')
#' 
#' hasAnchor("# section 1")
hasAnchor <- function(tag) {
  return(grepl(pattern = '<a (id|name)="', x = tag) & grepl(pattern = '"></a>', x = tag))
}


#' @title getAnchorName
#' @description Get the name/identifier an anchor tag
#' @param tag Character string
#' @return A character Name/identifier of the anchor tag
#' @export
#' @examples 
#' getAnchorName('<a id="section-1"></a>')
#' 
#' # Works also for tag name instead of id 
#' getAnchorName('<a name="section-1"></a>')
getAnchorName <- function(tag) {
  if(!hasAnchor(tag)){
    return()
  }
  # Keeps only what is within quotes
  tagName <- gsub(pattern = '.*<a (id|name)="', replacement = "", x = tag)
  tagName <- gsub(pattern = '"></a>.*', replacement = "", x = tagName)
  return(tagName)
}

#' @title introToYamlHeader
#' @description Translate an markdown introduction file into yaml header
#' In order to include introduction before the table of content,
#' it needs to be included as cover page features through a yaml header.
#' A yaml header provides additional arguments to pandoc when translating the md report.
#' Cover page features can be created with each their own style in the reference doc
#' @param introContent Character array of the intro content
#' @return A character array of yaml content
#' @keywords internal
introToYamlHeader <- function(introContent) {
  # Initialize empty contents for the yaml header
  titleContent <- ""
  subtitleContent <- ""
  
  # Look for title and subtitle of the intro content
  titleLine <- head(which(grepl(pattern = "# ", introContent) & !grepl(pattern = "## ", introContent)), 1)
  if(!isEmpty(titleLine)){
    titleContent <- gsub(pattern = "# ", replacement = "", x = introContent[titleLine])
    # Remove title from intro
    introContent <- introContent[-titleLine]
  }
  subtitleLine <- head(which(grepl(pattern = "## ", introContent) & !grepl(pattern = "### ", introContent)), 1)
  if(!isEmpty(subtitleLine)){
    subtitleContent <- gsub(pattern = "## ", replacement = "", x = introContent[subtitleLine])
    # Remove title from intro
    introContent <- introContent[-subtitleLine]
  }
  
  # Define cover page features
  yamlContent <- c(
    # yaml header is delimited by ---
      "---",
      paste0("title: '", titleContent, "'"),
      paste0("subtitle: '", subtitleContent, "'"),
      # Caution, yaml options on several lines require indentation
      "abstract: | ",
      paste("\t", introContent),
      "",
      # Add a page break before the table of content
      paste("\t", "\\newpage"),
      "---"
    )
  return(yamlContent)
  
}

#' @title getTitleInfo
#' @description Get section titles information from report content
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @param titlePattern character pattern referencing titles in first element of line
#' @param titleLevels levels of titles in the report
#' @return List of title information including `line`, `content`, `reference`, `count`, `level`
#' @keywords internal
getTitleInfo <- function(fileContent, titlePattern = "#", titleLevels = 6) {
  # Initialize title information
  titleInfo <- list()
  titlePatterns <- sapply(seq(1, titleLevels), function(titleLevel){paste0(rep(titlePattern, titleLevel), collapse = "")})
  titleCounts <- rep(0, titleLevels)
  titleReference <- NULL
  for (lineIndex in seq_along(fileContent)) {
    lineContent <- fileContent[lineIndex]
    firstElement <- as.character(unlist(strsplit(lineContent, " ")))[1]
    # Use anchor only if defined as first element of line
    if (grepl(pattern = "<a", x = firstElement)) {
      titleReference <- getAnchorName(lineContent)
    }
    for (titleLevel in rev(seq(1, titleLevels))) {
      # Identify section titles as lines starting with "#" characters
      if (grepl(pattern = titlePatterns[titleLevel], x = firstElement)) {
        # Prevents unreferenced title sections to appear in table of content
        if(is.null(titleReference)){
          next
        }
        # Count elements of section tree for numbering of sections
        titleCounts[titleLevel] <- titleCounts[titleLevel] + 1
        if (titleLevel < titleLevels) {
          titleCounts[seq(titleLevel + 1, titleLevels)] <- 0
        }
        
        titleInfo[[length(titleInfo)+1]] <- list(
            line = lineIndex,
            # Remove the "#" characters from the title content
            content = lineContent,
            reference = titleReference,
            count = titleCounts,
            level = titleLevel
          )
        titleReference <- NULL
        break
    }
    }
  }
  return(titleInfo)
}

#' @title getFirstLineElement
#' @description Get first element/word of a line
#' @param lineContent Character string
#' @param split character pattern to split between elements/words
#' @return Character string
#' @keywords internal
getFirstLineElement <- function(lineContent, split = " "){
  as.character(unlist(strsplit(lineContent, split)))[1]
}

#' @title updateFigureNumbers
#' @description Update figure captions and references in report
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @param pattern character pattern referencing figures in first element of line
#' @param replacement character replacing pattern in updated caption name
#' @param anchorId character pattern referencing figures in anchors
#' @return Array of character strings
#' @keywords internal
updateFigureNumbers <- function(fileContent, pattern = "Figure:", replacement = "Figure", anchorId = "figure") {
  # Only higher level titles are used for figure numbering
  titleInfo <- getTitleInfo(fileContent)
  titleInfo <- titleInfo[sapply(titleInfo, function(title)title$level==1)]
  titleLines <- sapply(titleInfo, function(title)title$line)
  # In case of unreferenced titles
  titleNumbers <- sapply(titleInfo, function(title)title$count[1])

  # Initialize
  updatedFileContent <- NULL
  count <- 1
  titleIndex <- 1
  for(lineIndex in seq_along(fileContent)){
    # Counting is performed within sections
    # Need to reset count at lines of titles
    if(lineIndex %in% titleLines){
      count <- 1
    }
    
    # If line is not related to an artifact, nothing to update
    firstElement <- getFirstLineElement(fileContent[lineIndex])
    if (!grepl(pattern = pattern, x = firstElement)) {
      updatedFileContent <- c(updatedFileContent, fileContent[lineIndex])
      next
    }
    # Get section number of figure as last value lower than line index
    # If no value found, section is empty and figure count is only global count
    section <- tail(titleNumbers[titleLines<lineIndex], 1)
    figureNumber <- paste(c(section, count), collapse = "-")
    
    # Create reference anchor with id matching figure number
    anchorContent <- anchor(paste(anchorId, figureNumber, sep = "-"))
    
    # Update caption with appropriate figure count
    updatedFigureContent <- gsub(
      pattern = pattern, 
      replacement = paste0(replacement, " ", figureNumber, ":"), 
      x = fileContent[lineIndex]
      )
    
    # Updated file content includes reference and intra section numbering
    updatedFileContent <- c(updatedFileContent, "", anchorContent, "", updatedFigureContent)
    
    count <- count + 1
  }
  return(updatedFileContent)
}

#' @title updateTableNumbers
#' @description Update table captions and references in report
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @param pattern character pattern referencing figures in first element of line
#' @param replacement character replacing pattern in updated caption name
#' @param anchorId character pattern referencing figures in anchors
#' @return Array of character strings
#' @keywords internal
updateTableNumbers <- function(fileContent, pattern = "Table:", replacement = "Table", anchorId = "table") {
  # For tables, relies on the same workflow replacing default figure patterns by table patterns
  return(updateFigureNumbers(fileContent, pattern, replacement, anchorId))
}

#' @title copyReport
#' @description Copy markdown report and its figures (using their paths)
#' @param from path of initial .md file to copy
#' @param to path of destination .md file to be copied
#' @param copyWordReport logical defining if .docx report is also copied
#' @param keep logical defining if initial .md file and figures are kept
#' @export
copyReport <- function(from, to, copyWordReport = TRUE, keep = FALSE) {
  validateIsFileExtension(from, "md")
  validateIsFileExtension(to, "md")
  validateFileExists(from)
  validateIsLogical(copyWordReport)
  validateIsLogical(keep)
  # If from and to files are identical, just return
  if(tolower(normalizePath(from, mustWork = FALSE)) == tolower(normalizePath(to, mustWork = FALSE))){
    return(invisible())
  }
  # Get directories of reports
  fromFolder <- dirname(from)
  toFolder <- dirname(to)
  fromWordReport <- gsub(pattern = ".md", replacement = ".docx", x = from)
  toWordReport <-gsub(pattern = ".md", replacement = ".docx", x = to)
  
  # If from and to locations are identical but not files, only copy report
  if(tolower(normalizePath(fromFolder, mustWork = FALSE)) == tolower(normalizePath(toFolder, mustWork = FALSE))){
    file.copy(from, to, overwrite = TRUE)
    if(copyWordReport){
      file.copy(from = fromWordReport, to = toWordReport)
    }
    if(!keep){
      unlink(from, recursive = TRUE)
      if(copyWordReport){
        unlink(fromWordReport, recursive = TRUE)
      }
    }
    return(invisible())
  }
  
  # Copy the .md report to its destination
  dir.create(toFolder, showWarnings = FALSE, recursive = TRUE)
  file.copy(from, to, overwrite = TRUE)
  if(copyWordReport){
    file.copy(from = fromWordReport, to = toWordReport)
  }
  
  # Get all file paths available in figures/file links
  fileContent <- readLines(from, encoding = "UTF-8")
  filePaths <- fileContent[grepl(pattern = '\\!\\[', x = fileContent)]
  filePaths <- gsub(pattern = '.*\\]\\(', replacement = "", x = filePaths)
  filePaths <- gsub(pattern = '\\).*', replacement = "", x = filePaths)
  filePaths <- unique(filePaths)
  
  # Create all necessary subfolders within report folder
  for(dirPath in unique(file.path(toFolder, dirname(filePaths)))){
    dir.create(dirPath, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Copy the figures in destination folder to have them available for new report
  file.copy(file.path(fromFolder, filePaths), file.path(toFolder, filePaths), overwrite = TRUE)
  
  # If keep is true, keep initial files and report
  if(keep){
    return(invisible())
  }
  
  # If keep is false, delete initial files and report
  for(filePath in filePaths){
    unlink(file.path(fromFolder, filePath), recursive = TRUE)
  }
  unlink(from, recursive = TRUE)
  if(copyWordReport){
    unlink(fromWordReport, recursive = TRUE)
  }
  return(invisible())
}
