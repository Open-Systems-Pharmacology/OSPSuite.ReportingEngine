#' @title initializeRmdFile
#' @description Initialize a R markdown document by Writing its header in YAML
#' @param fileName name of Rmd file to initialize
#' @param title title of the document
#' @param author author of the
#' @return R markdown text and save the Rmd document
initializeRmdFile <- function(fileName,
                              title = "",
                              author = "") {
  RmdText <- c(
    "---",
    paste0('title: "', title, '"'),
    paste0('author: "', author, '"'),
    "---",
    ""
  )

  # can add an option for date in the document:
  # 'date: "`r base::Sys.time()`"'
  if (file.exists(fileName)) {
    warning(paste0(fileName, " already exists."))
    warning(paste0("Overwriting ", fileName))
  }
  write(RmdText, file = fileName, sep = "\n")

  return()
}

#' @title addRmdFigureChunk
#' @description Add a figure in a Rmd document
#' @param fileName name of Rmd file
#' @param figureFile figure to include
#' @param figureCaption caption of figure
#' @return R markdown text with figure and save the Rmd document
addRmdFigureChunk <- function(fileName,
                              figureFile,
                              figureCaption = "") {
  RmdText <- c(
    "",
    paste0('```{r, out.width="100%", include=TRUE, fig.align="center", fig.caption= "', figureCaption, '", echo=FALSE}'),
    paste0('knitr::include_graphics("', figureFile, '")'),
    "```",
    ""
  )

  write(RmdText, file = fileName, append = TRUE, sep = "\n")

  return()
}

#' @title addRmdTableChunk
#' @description Add a table in a Rmd document
#' @param fileName name of Rmd file
#' @param tableFile table to include as a csv file
#' @param tableCaption caption of table
#' @return R markdown text with figure and save the Rmd document
addRmdTableChunk <- function(fileName,
                             tableFile,
                             tableCaption = "") {
  RmdText <- c(
    "",
    '```{r, echo = FALSE, results = "as.is"}',
    paste0("table <- read.csv(", tableFile, ")"),
    paste0("knitr::kable(table, caption = ", tableCaption, ")"),
    "```",
    ""
  )

  write(RmdText, file = fileName, append = TRUE, sep = "\n")

  return()
}

#' @title addRmdTextChunk
#' @description Add markdown text to a Rmd document
#' @param fileName name of Rmd file
#' @param text text to include in the document
#' @param figureCaption caption of figure
#' @return R markdown text with figure and save the Rmd document
addRmdTextChunk <- function(fileName,
                            text) {
  write(c(
    "",
    text,
    ""
  ),
  file = fileName, append = TRUE, sep = "\n"
  )
  return()
}

#' @title renderRmdFile
#' @description Translate R markdown document into markdown and html document
#' @param fileName name of Rmd file
renderRmdFile <- function(fileName) {
  rmarkdown::render(fileName, output_format = c("html_document", "md_document"))
  return()
}
