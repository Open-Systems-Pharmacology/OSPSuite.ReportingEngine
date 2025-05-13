#' @title resetReport
#' @description Initialize a report, warning if a previous version already exists
#' @param fileName name of .md file to reset
#' @export
#' @family reporting
#' @examples
#' \dontrun{
#' resetReport("report.md")
#' }
#'
resetReport <- function(fileName) {
  if (file.exists(fileName)) {
    logDebug(paste0("'", fileName, "' already exists. Overwriting '", fileName, "'."))
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
#' @export
#' @family reporting
addFigureChunk <- function(fileName,
                           figureFileRelativePath,
                           figureFileRootDirectory,
                           figureCaption = "") {
  # For a figure path to be valid in markdown using ![](#figurePath)
  # %20 needs to replace spaces in that figure path
  mdFigureFile <- URLencode(figureFileRelativePath)

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
  return(invisible())
}

#' @title addTableChunk
#' @description Add a table in a .md document
#' @param fileName name of .md file
#' @param tableFileRelativePath path to table relative to working directory
#' @param tableFileRootDirectory working directory
#' @param digits number of decimal digits in displayed numbers
#' @param scientific logical defining if displayed numbers use scientific writing
#' @param na character string replacing `NA` values in table
#' @export
#' @import ospsuite.utils
#' @family reporting
addTableChunk <- function(fileName,
                          tableFileRelativePath,
                          tableFileRootDirectory,
                          digits = NULL,
                          scientific = NULL,
                          na = "-") {
  # The function `formatNumerics` is now used by addTableChunk
  # colClasses = "character" is not needed anymore to enforce all table elements to be 'character'
  table <- read.csv(
    file.path(tableFileRootDirectory, tableFileRelativePath),
    check.names = FALSE,
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
  return(invisible())
}

#' @title addTextChunk
#' @description Add a text chunk to a .md document
#' @param fileName name of .md file
#' @param text text to include in the document
#' @export
#' @family reporting
#' @examples
#' \dontrun{
#' resetReport("report.md")
#' addTextChunk(fileName = "report.md", text = "new text")
#' }
#'
addTextChunk <- function(fileName, text) {
  fileObject <- file(fileName, encoding = "UTF-8", open = "at")
  write(c("", text, ""), file = fileObject, append = TRUE, sep = "\n")
  close(fileObject)
  return(invisible())
}

#' @title mergeMarkdownFiles
#' @description Merge markdown files into one unique file
#' @param inputFiles names of .md files to merge
#' @param outputFile name of merged .md file
#' @param keepInputFiles logical option to prevent the input files to be deleted after merging them
#' @export
#' @family reporting
#' @examples
#' \dontrun{
#' resetReport("chapter-1.md")
#' addTextChunk(fileName = "chapter-1.md", text = "Chapter 1")
#' resetReport("chapter-2.md")
#' addTextChunk(fileName = "chapter-2.md", text = "Chapter 2")
#' mergeMarkdownFiles(
#'   inputFiles = c("chapter-1.md", "chapter-2.md"),
#'   outputFile = "chapters-1and2.md"
#' )
#' }
#'
mergeMarkdownFiles <- function(inputFiles, outputFile, keepInputFiles = FALSE) {
  validateIsLogical(keepInputFiles)
  # Read all files contents first in case outputFile is within inputFiles
  filesContent <- lapply(inputFiles, function(fileName) {
    readLines(fileName, encoding = "UTF-8")
  })
  resetReport(outputFile)

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
  invisible(lapply(filesContent, function(fileContent) {
    addTextChunk(outputFile, fileContent)
  }))
  if (!keepInputFiles) {
    # Use setdiff to prevent erasing output file its name is included in inputFiles
    file.remove(setdiff(inputFiles, outputFile))
  }

  logDebug(paste0("Reports '", paste0(inputFiles, collapse = "', '"), "' were successfully merged into '", outputFile, "'"))
  return(invisible())
}

#' @title renderReport
#' @description Render report with number sections and table of content
#' @param fileName name of .md file to render
#' @param createWordReport option for creating Markdown-Report only but not a Word-Report
#' @param numberSections logical defining if sections are numbered
#' @param intro name of .md file that include introduction (before toc)
#' @param wordConversionTemplate optional docx template for rendering a tuned Word-Report document
#' @export
#' @family reporting
renderReport <- function(fileName, createWordReport = FALSE, numberSections = TRUE, intro = NULL, wordConversionTemplate = NULL) {
  actionToken2 <- re.tStartAction(actionType = "ReportGeneration")
  addTableAndFigureNumbersToMarkdown(fileName)
  # When rendering word report, the pandoc command toc automatically number the sections
  # Thus, report-word.md needs to be created before numbering the markdown sections
  renderWordReport(fileName, intro = intro, createWordReport, wordConversionTemplate)
  if (numberSections) {
    addSectionNumbersToMarkdown(fileName)
  }
  addMarkdownToc(fileName)
  mergeMarkdownFiles(inputFiles = c(intro, fileName), outputFile = fileName)
  trimFile(fileName)
  re.tEndAction(actionToken = actionToken2)
  return(invisible())
}

#' @title translateForWord
#' @description
#' Translate markdown text into a version that Pandoc can convert to Word with appropriate formatting
#' @param mdText Character strings of markdown text
#' @return Updated md text
#' @keywords internal
translateForWord <- function(mdText) {
  wordText <- NULL
  # Page break using source code
  # https://pandoc.org/MANUAL.html#extension-raw_attribute
  artifactPageBreak <- c(
    "```{=openxml}",
    '<w:br w:type="page"/>',
    "```",
    ""
  )
  for (lineContent in mdText) {
    firstElement <- getFirstLineElement(lineContent)
    anchorName <- getAnchorName(lineContent)
    # Issue #968 Subscript and superscript rendering
    lineContent <- gsub(pattern = "<sup>|</sup>", replacement = "^", lineContent)
    lineContent <- gsub(pattern = "<sub>|</sub>", replacement = "~", lineContent)
    # Expose openxml translations for users
    # Manual page break
    lineContent <- gsub(pattern = "<pagebreak>", replacement = '<w:br w:type="page"/>`{=openxml}', lineContent)
    # Alignment
    lineContent <- gsub(pattern = "</div>", replacement = "", lineContent)
    lineContent <- gsub(pattern = '<div align=\"right\">', replacement = '`<w:pPr><w:jc w:val="right"/></w:pPr>`{=openxml}', lineContent)
    lineContent <- gsub(pattern = '<div align=\"left\">', replacement = '`<w:pPr><w:jc w:val="left"/></w:pPr>`{=openxml}', lineContent)
    lineContent <- gsub(pattern = '<div align=\"center\">', replacement = '`<w:pPr><w:jc w:val="center"/></w:pPr>`{=openxml}', lineContent)
    lineContent <- gsub(pattern = '<div align=\"justify\">', replacement = '`<w:pPr><w:jc w:val="both"/></w:pPr>`{=openxml}', lineContent)

    # For anchors, remove and replace tag by inline bookmark at the end of the line
    # By convention, no more than 1 anchor should be included in a line
    if (hasAnchor(lineContent)) {
      lineContent <- gsub(pattern = "<a.*/a>", replacement = "", lineContent)
      lineContent <- paste0(
        lineContent,
        '`<w:bookmarkStart w:id="', anchorName, '" w:name="', anchorName, '"/>',
        '<w:bookmarkEnd w:id="', anchorName, '"/>`{=openxml}'
      )
    }
    # By convention, artifacts are referenced with a tag at the beginning of the line
    # whose identifier includes "table" or "figure"
    # For them, a page break is needed before the tag to get a prettier word document
    # Since the identification is based on the html tag,
    # caption on top or below the artifact does not require any update
    isArtifactAnchor <- all(
      isIncluded(firstElement, "<a"),
      grepl(pattern = "table|figure", x = anchorName)
    )
    wordText <- c(wordText, artifactPageBreak[isArtifactAnchor], lineContent)
  }
  return(wordText)
}

#' @title renderWordReport
#' @description Render docx report with number sections and table of content
#' @param fileName name of .md file to render
#' @param intro name of .md file that include introduction (before toc)
#' @param createWordReport option for creating Markdown-Report only but not a Word-Report
#' @param wordConversionTemplate optional docx template for rendering a tuned Word-Report document
#' @export
#' @family reporting
renderWordReport <- function(fileName, intro = NULL, createWordReport = FALSE, wordConversionTemplate = NULL) {
  if (!createWordReport) {
    return(invisible())
  }
  reportConfig <- file.path(reEnv$log$folder, "word-report-configuration.txt")
  wordFileName <- sub(pattern = ".md", replacement = "-word.md", fileName)
  docxWordFileName <- sub(pattern = ".md", replacement = "-word.docx", fileName)
  docxFileName <- sub(pattern = ".md", replacement = ".docx", fileName)

  fileContent <- readLines(fileName, encoding = "UTF-8")
  wordFileContent <- translateForWord(fileContent)

  # Code for tracelib package
  usedFilesFileName <- sub(pattern = ".md", replacement = "-usedFiles.txt", fileName)
  usedFiles <- readLines(usedFilesFileName, encoding = "UTF-8")
  for (usedFile in usedFiles) {
    if (usedFile != "") {
      re.tStoreFileMetadata(access = "read", filePath = usedFile)
    }
  }
  file.remove(usedFilesFileName)

  # Include introduction as a yaml header passed as additional arguments to pandoc
  if (!isEmpty(intro)) {
    introContent <- readLines(intro, encoding = "UTF-8")
    yamlContent <- introToYamlHeader(introContent)
    # Cover page features
    wordFileContent <- c(yamlContent, "", wordFileContent)
  }
  # Since write() uses sep = "\n",
  # every element of array wordFileContent is added in a new line
  fileObject <- file(wordFileName, encoding = "UTF-8")
  write(wordFileContent, file = fileObject, sep = "\n")
  close(fileObject)
  re.tStoreFileMetadata(access = "write", filePath = wordFileName)

  # Check if pandoc is available before trying to render word report
  if (!rmarkdown::pandoc_available()) {
    logError("Pandoc is not installed, word report was not created.")
    return(invisible())
  }
  wordConversionTemplate <- wordConversionTemplate %||% system.file("extdata", "reference.docx", package = "ospsuite.reportingengine")

  # Some arguments will depend on pandoc version to prevent warnings
  # docx requires that figures are contained within document
  selfContainedArgument <- "self-contained:"
  if (rmarkdown::pandoc_version() >= "2.19") {
    selfContainedArgument <- c("embed-resources:", "standalone:")
  }
  # Create txt file that includes arguments for Pandoc
  write(
    c(
      selfContainedArgument,
      # Remove wrapping limitation of 80 characters/line
      "wrap: none",
      # Add table of content
      "toc:",
      # Add extensions to md for conversion
      # +tex_math_dollars: convert equations written between $...$ in LateX
      # https://pandoc.org/MANUAL.html#extension-tex_math_dollars
      # +raw_attribute: keep ```{=openxml} as raw openxml to include page breaks and bookmarks
      # https://pandoc.org/MANUAL.html#extension-raw_attribute
      # +superscript+subscript: convert superscript text between ^...^, subscript text between ~...~
      # https://pandoc.org/MANUAL.html#superscripts-and-subscripts
      "from: markdown+tex_math_dollars+superscript+subscript+raw_attribute",
      # Document used for styling
      paste0('reference-doc: "', wordConversionTemplate, '"'),
      # Location of resources such as figures
      paste0('resource-path: "', reEnv$log$folder, '"')
    ),
    file = reportConfig, sep = "\n"
  )

  knitr::pandoc(input = wordFileName, format = "docx", config = reportConfig)
  file.copy(docxWordFileName, docxFileName, overwrite = TRUE)
  re.tStoreFileMetadata(access = "write", filePath = docxFileName)
  unlink(reportConfig, recursive = TRUE)
  unlink(docxWordFileName, recursive = TRUE)

  logDebug(paste0("Word version of report '", fileName, "' created."))
  return(invisible())
}

#' @title addTableAndFigureNumbersToMarkdown
#' @description Reference tables and figures in a report
#' @param fileName name of .md file to update
#' @keywords internal
addTableAndFigureNumbersToMarkdown <- function(fileName) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  numberOfLines <- length(fileContent)

  fileContent <- updateFigureNumbers(fileContent)
  # Three new lines are added by referenced figure
  figureCount <- (length(fileContent) - numberOfLines) / 3
  numberOfLines <- length(fileContent)

  fileContent <- updateTableNumbers(fileContent)
  # Three new lines are added by referenced table
  tableCount <- (length(fileContent) - numberOfLines) / 3
  numberOfLines <- length(fileContent)

  fileObject <- file(fileName, encoding = "UTF-8")
  write(fileContent, file = fileObject, sep = "\n")
  close(fileObject)

  logDebug(paste0(
    "In '", fileName, "', ", tableCount, " tables and ",
    figureCount, " figures were referenced."
  ))
  return(invisible())
}

#' @title addSectionNumbersToMarkdown
#' @description Update section numbers of a report
#' @param fileName name of .md file to update
#' @keywords internal
addSectionNumbersToMarkdown <- function(fileName) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  titleInfo <- getTitleInfo(fileContent)
  for (title in titleInfo) {
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

  logDebug(paste0("In '", fileName, "', ", length(title), " sections were numbered"))
  return(invisible())
}


#' @title addMarkdownToc
#' @description Add table of content to a markdown file
#' @param fileName name of .md file to update
#' @param tocTitle Markdown text for table of content title
#' @keywords internal
addMarkdownToc <- function(fileName, tocTitle = "# Table of Contents") {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  titleInfo <- getTitleInfo(fileContent)
  # For each title, create its entry in table of content
  # By adding as many spaces as levels, then *[display title](#anchorId)
  tocContent <- sapply(
    titleInfo,
    function(title) {
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

  logDebug(paste0(
    "In '", fileName, "', table of content of ",
    length(tocContent), " lines was included."
  ))
  return(invisible())
}

#' @title setSimulationDescriptor
#' @description Set workflow simulation set descriptor
#' @param workflow A `Workflow` object
#' @param text Character describing simulation sets
#' @export
#' @import ospsuite.utils
#' @family workflow helpers
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
#' @family workflow helpers
getSimulationDescriptor <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  return(workflow$getSimulationDescriptor())
}

#' @title adjustTitlePage
#' @description Adjust Qualification Version Information to be displayed on title page
#' @param fileName name of .md file to update
#' @param qualificationVersionInfo A `QualificationVersionInfo`object defining Qualification Version Information to be displayed on title page
#' @export
#' @family qualification workflow
adjustTitlePage <- function(fileName, qualificationVersionInfo = NULL) {
  validateIsOfType(qualificationVersionInfo, "QualificationVersionInfo", nullAllowed = TRUE)
  validateFileExists(fileName)
  # Does not adust title page if no QualificationVersionInfo
  if (isEmpty(qualificationVersionInfo)) {
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
#' @family reporting
#' @examples
#'
#' anchor("section-1")
#'
anchor <- function(name) {
  return(paste0('<a id="', tolower(name), '"></a>'))
}

#' @title hasAnchor
#' @description Check if a character string includes an anchor tag
#' @param tag Character string
#' @return A logical
#' @export
#' @family reporting
#' @examples
#' # Flags both anchors using id or name
#' hasAnchor('<a id="section-1"></a>')
#' hasAnchor('<a name="section-1"></a>')
#'
#' hasAnchor("# section 1")
#'
hasAnchor <- function(tag) {
  return(grepl(pattern = '<a (id|name)="', x = tag) & grepl(pattern = '"></a>', x = tag))
}


#' @title getAnchorName
#' @description Get the name/identifier an anchor tag
#' @param tag Character string
#' @return A character Name/identifier of the anchor tag
#' @export
#' @family reporting
#' @examples
#' getAnchorName('<a id="section-1"></a>')
#'
#' # Works also for tag name instead of id
#' getAnchorName('<a name="section-1"></a>')
#'
getAnchorName <- function(tag) {
  if (!hasAnchor(tag)) {
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
  if (!isEmpty(titleLine)) {
    titleContent <- gsub(pattern = "# ", replacement = "", x = introContent[titleLine])
    # Remove title from intro
    introContent <- introContent[-titleLine]
  }
  subtitleLine <- head(which(grepl(pattern = "## ", introContent) & !grepl(pattern = "### ", introContent)), 1)
  if (!isEmpty(subtitleLine)) {
    subtitleContent <- gsub(pattern = "## ", replacement = "", x = introContent[subtitleLine])
    # Remove title from intro
    introContent <- introContent[-subtitleLine]
  }
  # Because yaml header is indentation based,
  # empty lines and spaces before text cause a yaml indentation issue as identified below
  # YAML parse exception at line XX, column XX, while parsing a block mapping: did not find expected key
  # Thus, remove left spaces
  introContent <- trimws(introContent, which = "left", whitespace = "[ \t\r]")

  # Define cover page features
  yamlContent <- c(
    # yaml header is delimited by ---
    "---",
    paste0("title: '", titleContent, "'"),
    paste0("subtitle: '", subtitleContent, "'"),
    # Caution, yaml options on several lines require indentation
    "abstract: | ",
    paste("\t", introContent),
    # Add a page break before the table of content
    # Because page break is in yaml abstract, use inline code block with same indentation
    '\t `<w:br w:type="page"/>`{=openxml}',
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
  titlePatterns <- sapply(seq(1, titleLevels), function(titleLevel) {
    paste0(rep(titlePattern, titleLevel), collapse = "")
  })
  titleCounts <- rep(0, titleLevels)
  titleReference <- NULL
  for (lineIndex in seq_along(fileContent)) {
    lineContent <- fileContent[lineIndex]
    firstElement <- as.character(unlist(strsplit(lineContent, " ")))[1]
    for (titleLevel in rev(seq(1, titleLevels))) {
      # Identify section titles as lines starting with "#" characters
      if (grepl(pattern = titlePatterns[titleLevel], x = firstElement)) {
        titleReference <- getAnchorName(lineContent)
        # Prevents unreferenced title sections to appear in table of content
        if (is.null(titleReference)) {
          next
        }
        # Count elements of section tree for numbering of sections
        titleCounts[titleLevel] <- titleCounts[titleLevel] + 1
        if (titleLevel < titleLevels) {
          titleCounts[seq(titleLevel + 1, titleLevels)] <- 0
        }

        titleInfo[[length(titleInfo) + 1]] <- list(
          line = lineIndex,
          # Remove the "#" characters from the title content
          content = gsub(lineContent, pattern = anchor(titleReference), replacement = ""),
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
getFirstLineElement <- function(lineContent, split = " ") {
  as.character(unlist(strsplit(lineContent, split)))[1]
}

#' @title updateArtifactNumbers
#' @description Update artifact (figure or table) captions and references in report
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @param pattern character pattern referencing figures in first element of line
#' @param replacement character replacing pattern in updated caption name
#' @param anchorId character pattern referencing anchor tags
#' @param captionBelow logical defining if caption is below artifact
#' @return Array of character strings
#' @keywords internal
updateArtifactNumbers <- function(fileContent, pattern, replacement, anchorId, captionBelow = FALSE) {
  # Only higher level titles are used for figure numbering
  titleInfo <- getTitleInfo(fileContent)
  titleInfo <- titleInfo[sapply(titleInfo, function(title) title$level == 1)]
  titleLines <- sapply(titleInfo, function(title) title$line)
  # In case of unreferenced titles
  titleNumbers <- sapply(titleInfo, function(title) title$count[1])

  # Initialize
  updatedFileContent <- NULL
  count <- 1
  patternFound <- TRUE
  for (lineIndex in seq_along(fileContent)) {
    # Counting is performed within sections
    # Need to reset count at lines of titles
    if (lineIndex %in% titleLines) {
      count <- 1
      patternFound <- TRUE
    }
    # Get section number of figure as last value lower than line index
    # If no value found, section is empty and figure count is only global count
    section <- tail(titleNumbers[titleLines < lineIndex], 1)
    artifactNumber <- paste(c(section, count), collapse = "-")
    # Create reference anchor with id matching figure number
    anchorContent <- anchor(paste(anchorId, artifactNumber, sep = "-"))
    # Assess if artifact from first line element
    firstElement <- getFirstLineElement(fileContent[lineIndex])
    # Artifact is Figure and update is called within Figure updates
    # Otherwise, the function will also update figures for with Table reference
    figureRequireUpdate <- all(
      grepl(pattern = "\\!\\[", x = firstElement),
      grepl(pattern = "Figure", x = pattern)
    )
    if (figureRequireUpdate) {
      # If no Figure pattern was found before the next figure
      # Updates the count and name of the figure
      if (!patternFound) {
        count <- count + 1
        artifactNumber <- paste(c(section, count), collapse = "-")
        # Create reference anchor with id matching figure number
        anchorContent <- anchor(paste(anchorId, artifactNumber, sep = "-"))
      }
      updatedFileContent <- c(
        updatedFileContent,
        anchorContent,
        "",
        fileContent[lineIndex]
      )
      patternFound <- FALSE
      next
    }
    # If line is not related to an artifact, nothing to update
    if (!grepl(pattern = pattern, x = firstElement)) {
      updatedFileContent <- c(updatedFileContent, fileContent[lineIndex])
      next
    }
    # Else line starts by "Figure:" or "Table:"
    # Update caption with appropriate figure/table count
    updatedArtifactContent <- gsub(
      pattern = pattern,
      replacement = paste0(replacement, " ", artifactNumber, ":"),
      x = fileContent[lineIndex]
    )

    # Updated file content includes reference, figure/table and intra-section numbering
    updatedFileContent <- c(
      updatedFileContent,
      # in case of caption below, the anchor was already included before artifact (figure)
      anchorContent[!captionBelow],
      "",
      updatedArtifactContent
    )
    count <- count + 1
    patternFound <- TRUE
  }
  return(updatedFileContent)
}

#' @title updateFigureNumbers
#' @description Update figure captions and references in report
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @return Array of character strings
#' @keywords internal
updateFigureNumbers <- function(fileContent) {
  return(updateArtifactNumbers(
    fileContent,
    pattern = "Figure:",
    replacement = "Figure",
    anchorId = "figure",
    captionBelow = TRUE
  ))
}

#' @title updateTableNumbers
#' @description Update table captions and references in report
#' @param fileContent Content of a markdown or text file read as an array of character strings
#' @return Array of character strings
#' @keywords internal
updateTableNumbers <- function(fileContent) {
  return(updateArtifactNumbers(
    fileContent,
    pattern = "Table:",
    replacement = "Table",
    anchorId = "table",
    captionBelow = FALSE
  ))
}

#' @title trimFile
#' @description
#' Trim all the duplicated blank lines in a markdown report file as well as pre-title blank space
#' @param fileName path of .md file to trim
#' @export
#' @family reporting
trimFile <- function(fileName) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  # Trim all the duplicated blank lines
  emptyLines <- fileContent %in% ""
  emptyPreviousLines <- c(FALSE, head(emptyLines, -1))
  toRemove <- emptyLines & emptyPreviousLines
  newFileContent <- fileContent[!toRemove]
  # If report starts with blank space, only one blank line remains at this point
  if (head(newFileContent, 1) %in% "") {
    newFileContent <- tail(newFileContent, -1)
  }
  fileObject <- file(fileName, encoding = "UTF-8")
  write(newFileContent, file = fileObject, sep = "\n")
  close(fileObject)
  return(invisible())
}

#' @title copyReport
#' @description Copy markdown report and its figures (using their paths)
#' @param from path of initial .md file to copy
#' @param to path of destination .md file to be copied
#' @param copyWordReport logical defining if .docx report is also copied
#' @param keep logical defining if initial .md file and figures are kept
#' @export
#' @family reporting
copyReport <- function(from, to, copyWordReport = TRUE, keep = FALSE) {
  validateIsFileExtension(from, "md")
  validateIsFileExtension(to, "md")
  validateFileExists(from)
  validateIsLogical(copyWordReport)
  validateIsLogical(keep)
  # If from and to files are identical, just return
  if (tolower(normalizePath(from, mustWork = FALSE)) == tolower(normalizePath(to, mustWork = FALSE))) {
    return(invisible())
  }
  # Get directories of reports
  fromFolder <- dirname(from)
  toFolder <- dirname(to)
  fromWordReport <- gsub(pattern = ".md", replacement = ".docx", x = from)
  toWordReport <- gsub(pattern = ".md", replacement = ".docx", x = to)

  # If from and to locations are identical but not files, only copy report
  if (tolower(normalizePath(fromFolder, mustWork = FALSE)) == tolower(normalizePath(toFolder, mustWork = FALSE))) {
    file.copy(from, to, overwrite = TRUE)
    if (copyWordReport) {
      file.copy(from = fromWordReport, to = toWordReport)
    }
    if (!keep) {
      unlink(from, recursive = TRUE)
      if (copyWordReport) {
        unlink(fromWordReport, recursive = TRUE)
      }
    }
    return(invisible())
  }

  # Copy the .md report to its destination
  dir.create(toFolder, showWarnings = FALSE, recursive = TRUE)
  file.copy(from, to, overwrite = TRUE)
  if (copyWordReport) {
    file.copy(from = fromWordReport, to = toWordReport, overwrite = TRUE)
  }

  # Copy the figures in destination folder to have them available for new report
  # Get all file paths available in figures/file links
  filePaths <- getFigurePathsFromReport(from)
  # Create all necessary subfolders within report folder
  for (dirPath in unique(file.path(toFolder, dirname(filePaths)))) {
    dir.create(dirPath, showWarnings = FALSE, recursive = TRUE)
  }
  # checkFileExists will warn the user if the path is corrupdted
  checkFileExists(file.path(fromFolder, filePaths))
  file.copy(
    file.path(fromFolder, filePaths),
    file.path(toFolder, filePaths),
    overwrite = TRUE
  )

  # If keep is true, keep initial files and report
  if (keep) {
    return(invisible())
  }

  # If keep is false, delete initial files and report
  for (filePath in filePaths) {
    unlink(file.path(fromFolder, filePath), recursive = TRUE)
  }
  unlink(from, recursive = TRUE)
  if (copyWordReport) {
    unlink(fromWordReport, recursive = TRUE)
  }
  return(invisible())
}


#' @title getIntroFromReportTitle
#' @description Get introduction file used as cover page of report
#' @param reportTitle Report title page
#' If `reportTitle` is an existing file, its content will be used as cover page.
#' If `reportTitle` is one character string, it is assumed as a title.
#' Thus the markdown title tag is internally added to `reportTitle`.
#' If `reportTitle` is multiple character strings, it is assumed as the cover page content.
#' and used *as is* in the cover page.
#' @param intro temporary introduction file deleted when merged to final report
#' Parameter is named intro to stay consistent with Qualification Workflow nomenclature
#' @return `intro`, path of temporary introduction file if created
#' @keywords internal
getIntroFromReportTitle <- function(reportTitle = NULL, intro = "temp-report-title.md") {
  # No cover page
  if (isEmpty(reportTitle)) {
    return(NULL)
  }
  # Create temporary cover page file to be merged
  resetReport(intro)
  # If report title are actual files, use their content as cover page
  if (all(file.exists(reportTitle))) {
    for (coverPage in reportTitle) {
      addTextChunk(
        fileName = intro,
        text = readLines(coverPage, encoding = "UTF-8", warn = FALSE)
      )
    }
    return(intro)
  }
  # If length of title is 1, it is assumed a title
  # and md title tag "#" is added
  if (isOfLength(reportTitle, 1)) {
    addTextChunk(
      fileName = intro,
      text = paste("#", reportTitle)
    )
    return(intro)
  }
  # If length of title is longer than 1,
  # it is assumed report title is the content of the page
  # and added as is in the intro file
  addTextChunk(
    fileName = intro,
    text = reportTitle
  )
  return(intro)
}

#' @title getFigurePathsFromReport
#' @description Get file paths from a report figure links
#' @param fileName name of .md file to
#' @return array of file paths corresponding to figures linked in reports
#' @export
#' @family reporting
#' @examples \dontrun{
#' # Check the figure paths of your report named "report.md"
#' getFigurePathsFromReport("report.md")
#' }
getFigurePathsFromReport <- function(fileName) {
  fileContent <- readLines(fileName, encoding = "UTF-8")
  filePaths <- fileContent[grepl(pattern = "\\!\\[", x = fileContent)]
  filePaths <- gsub(pattern = ".*\\]\\(", replacement = "", x = filePaths)
  filePaths <- gsub(pattern = "(\\))[^\\)]*$", replacement = "", x = filePaths)
  return(unique(filePaths))
}
