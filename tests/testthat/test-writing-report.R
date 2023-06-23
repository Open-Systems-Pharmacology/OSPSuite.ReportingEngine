context("Utilities for writing reports")

testReport <- "testReport.md"
initialReport <- getTestDataFilePath("utilities-report/initialReport.md")
testReportNumFigs <- getTestDataFilePath("utilities-report/testReportNumFigs.md")
testReportNumSecs <- getTestDataFilePath("utilities-report/testReportNumSecs.md")
testReportNumToc <- getTestDataFilePath("utilities-report/testReportNumToc.md")

# Ensure testReport is not deleted before tests
unlink(testReport, recursive = TRUE)
initialReportContent <- readLines(initialReport)

test_that("resetReport creates an empty file, overwriting pre-existing previous file", {
  resetReport(testReport)
  expect_true(file.exists(testReport))
  expect_equal(readLines(testReport), "")

  write("Not empty content", file = testReport)
  resetReport(testReport)
  expect_equal(readLines(testReport), "")
})

test_that("addTextChunk does not overwrite previous file", {
  addTextChunk(testReport, "some text")
  reportContent <- readLines(testReport)
  expect_gt(length(reportContent), 1)
  expect_equal(reportContent[3], "some text")

  addTextChunk(testReport, "some other text")
  reportNewContent <- readLines(testReport)
  expect_gt(length(reportNewContent), length(reportContent))
  expect_equal(reportNewContent[6], "some other text")
})

resetReport(testReport)
addTextChunk(testReport, initialReportContent)
addTableAndFigureNumbersToMarkdown(testReport)

test_that("numberTablesAndFigures counts correctly and update input file", {
  reportContent <- readLines(testReport)
  refReportContent <- readLines(testReportNumFigs)
  expect_equal(reportContent, refReportContent)
})

tocContent <- addSectionNumbersToMarkdown(testReport)
test_that("numberSections counts correctly and update input file", {
  reportContent <- readLines(testReport)
  refReportContent <- readLines(testReportNumSecs)
  expect_equal(reportContent, refReportContent)
})

addMarkdownToc(testReport)
test_that("Table of content has a correct format", {
  reportContent <- readLines(testReport)
  refReportContent <- readLines(testReportNumToc)
  expect_equal(reportContent, refReportContent)
})

# Clean the folder to prevent warnings and side effects
unlink("log-info.txt", recursive = TRUE)
unlink("log-debug.txt", recursive = TRUE)
unlink("log-error.txt", recursive = TRUE)
unlink("testReport.docx", recursive = TRUE)
unlink("testReport-word.md", recursive = TRUE)
unlink(testReport, recursive = TRUE)

titleFile <- getTestDataFilePath("utilities-report/titlepage.md")
refTitleFile <- getTestDataFilePath("utilities-report/updatedtitlepage.md")

test_that("versionInfo is correctly adjusted in title pages", {
  versionInfo <- QualificationVersionInfo$new("1.1", "2.2", "3.3")
  adjustTitlePage(titleFile, versionInfo)
  titleContent <- readLines(titleFile)
  refTitleContent <- readLines(refTitleFile)
  expect_equal(titleContent, refTitleContent)
})

# getIntroFromReportTitle is not an internal function
getIntroFromReportTitle <- ospsuite.reportingengine:::getIntroFromReportTitle
testIntroFile <- "test-temp-report-title.md"
test_that("getIntroFromReportTitle does nothing if reportTitle is NULL", {
  # Nothing is done if reportTitle is null
  expect_null(getIntroFromReportTitle(reportTitle = NULL, intro = testIntroFile))
  expect_false(file.exists(testIntroFile))
})

test_that("getIntroFromReportTitle add a title tag if reportTitle is one string", {
  # Should return the file path
  expect_equal(
    getIntroFromReportTitle(reportTitle = "A title", intro = testIntroFile),
    testIntroFile
  )
  # File path should exists
  expect_true(file.exists(testIntroFile))
  # Title tag is added
  expect_true(any(grepl(pattern = "# A title", x = readLines(testIntroFile))))
  # Removes file for next tests
  unlink(testIntroFile, recursive = TRUE)
})

test_that("getIntroFromReportTitle use title as is if reportTitle is multiple strings", {
  # Should return the file path
  expect_equal(
    getIntroFromReportTitle(reportTitle = c("My title", "other content"), intro = testIntroFile),
    testIntroFile
  )
  # File path should exists
  expect_true(file.exists(testIntroFile))
  # Title tag is not added
  expect_false(any(grepl(pattern = "# A title", x = readLines(testIntroFile))))
  # Content is used as is
  expect_true(any(grepl(pattern = "My title", x = readLines(testIntroFile))))
  expect_true(any(grepl(pattern = "other content", x = readLines(testIntroFile))))
  # Removes file for next tests
  unlink(testIntroFile, recursive = TRUE)
})

userCoverPage <- "input-cover-page.md"
userCoverPageContent <- "My cover page saved in a file"

write(
  x = userCoverPageContent,
  file = userCoverPage
)

test_that("getIntroFromReportTitle add file content if reportTitle is a file path", {
  # Should return the temporary file path
  # (prevents user cover page being deleted during report merging)
  expect_equal(
    getIntroFromReportTitle(reportTitle = userCoverPage, intro = testIntroFile),
    testIntroFile
  )
  # File path should exists
  expect_true(file.exists(testIntroFile))
  expect_true(any(grepl(pattern = userCoverPageContent, x = readLines(testIntroFile))))
  unlink(testIntroFile, recursive = TRUE)
  unlink(userCoverPage, recursive = TRUE)
})

figureLinksReport <- getTestDataFilePath("utilities-report/figureLinks.md")
referencePaths <- c(
  "link/to/figure/1.png",
  "link/to/figure(2).png",
  "link/to table (1).pdf",
  "link/to figure 3.png"
)
test_that("getFigurePathsFromReport gets the correct file paths", {
  filePaths <- getFigurePathsFromReport(figureLinksReport)
  expect_equal(referencePaths, referencePaths)
  # and assess that warning will be thrown if files do not exist
  expect_warning(ospsuite.reportingengine:::checkFileExists(filePaths))
})

