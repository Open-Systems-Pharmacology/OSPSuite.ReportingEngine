isWindows <- Sys.info()["sysname"] %in% "Windows"

if(isWindows) {
  # This line install the java script app that takes snapshots of reports
  # Add option force=TRUE to prevent webshot to crash if the install path is not found
  webshot::install_phantomjs(force = TRUE)
} else{
  # Check Chromote availability
  tryCatch({
    testChrome <- chromote::Chromote$new()
    testChrome$close()
  }, error = function(e) {
    message("Warning: Chrome failed to initialize. Ensure Chrome is installed and accessible.")
  }) 
}



#' @title includeReportFromWorkflow
#' @description Render a `workflow` report and display its content as raw html
#' @param workflow A `Workflow` object
#' @return Character string that needs to be evaluated as is in R Markdown
includeReportFromWorkflow <- function(workflow) {
  # Before rendering include style in markdown
  reportContent <- readLines(workflow$reportFilePath)
  styledReportContent <- c(
    "", '<div style="background-color:#e6f0ff; border-radius: 5px; padding: 20px; border-style: solid none solid solid;">',
    reportContent,
    "</div>", ""
  )
  fileObject <- file(workflow$reportFilePath, encoding = "UTF-8")
  write(styledReportContent, file = fileObject, append = FALSE, sep = "\n")
  close(fileObject)

  # Turn markdown into html report
  rmarkdown::render(
    workflow$reportFilePath,
    rmarkdown::html_document(),
    quiet = TRUE
  )

  # Take snapshot of the rendered report and
  # save it within figures/ since folder is exported for articles
  reportSnapshotFile <- file.path(
    "figures",
    paste0("report-snapshot-", length(list.files("figures", pattern = "png")) + 1, ".png")
  )
  if(isWindows){
    webshot::webshot(
      url = sub(".md", ".html", workflow$reportFilePath),
      file = reportSnapshotFile
    )
  } else{
    webshot2::webshot(
      url = sub(".md", ".html", workflow$reportFilePath),
      file = reportSnapshotFile
    )
  }
  
  # Display report as an image folded under "> Report" sub section
  return(
    paste(
      "<details>",
      "<summary> __Report__ </summary>",
      paste0('<img src="', reportSnapshotFile, '" style = "width:100%; border-style: none;" />'),
      "</details>",
      sep = "\n"
    )
  )
}
