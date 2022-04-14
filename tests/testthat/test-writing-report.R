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
numberTablesAndFigures(testReport)

test_that("numberTablesAndFigures counts correctly and update input file", {
  reportContent <- readLines(testReport)
  refReportContent <- readLines(testReportNumFigs)
  expect_equal(reportContent, refReportContent)
})

tocContent <- getSectionTOC(testReport)
test_that("numberSections counts correctly and update input file", {
  reportContent <- readLines(testReport)
  refReportContent <- readLines(testReportNumSecs)
  expect_equal(reportContent, refReportContent)
})

addMarkdownToc(tocContent, testReport)
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