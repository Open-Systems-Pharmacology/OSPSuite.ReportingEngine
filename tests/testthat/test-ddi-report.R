context("Qualification Workflow")

reOutputFolder <- "ddi-report-qualification"
configurationPlanFile <- getTestDataFilePath("qualification/configuration-plan-ddi-ratio.json")

reOutputFolderReferenceFiles <- c(
  "images", "SimulationResults", "PKAnalysisResults", "testcontent.md",
  "log-debug.txt", "log-info.txt", "Report.md"
)

# Load and run test qualification workflow
workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)
workflow$createWordReport <- FALSE
workflow$runWorkflow()

test_that("Output folder has the appropriate structure", {
  expect_setequal(list.files(reOutputFolder), reOutputFolderReferenceFiles)
})

test_that("All results were saved", {
  expect_length(list.files(file.path(reOutputFolder, "SimulationResults")), 2)
  expect_length(list.files(file.path(reOutputFolder, "PKAnalysisResults")), 2)
  expect_length(
    list.files(file.path(reOutputFolder, "images", "001_section_test-ddi"), pattern = "csv"),
    19
    )
  expect_length(
    list.files(file.path(reOutputFolder, "images", "001_section_test-ddi"), pattern = "png"),
    24
  )
})

reportContent <- readLines(workflow$reportFilePath)

test_that("Subunits are alphabetically ordered", {
  # Expected subunits alphabetically ordered
  subunitTitles <- c(
    "## 1.1 Mechanism",
    "### 1.1.1 M1",
    "## 1.2 Perpetrator",
    "### 1.2.1 P1",
    "### 1.2.2 P2",
    "## 1.3 Victim",
    "### 1.3.1 V1",
    "### 1.3.2 V2"
  )
  previousSubunitLine <- 0
  for(subunit in subunitTitles){
    subunitLine <- grep(pattern = subunit, x = reportContent)
    testthat::expect_gt(subunitLine, previousSubunitLine)
    previousSubunitLine <- subunitLine
  }
})

test_that("Subunits are included in TOC", {
  tocSubunits <- c(
    "\\* \\[1.1 Mechanism \\]",
    "\\* \\[1.1.1 M1 \\]",
    "\\* \\[1.2 Perpetrator \\]",
    "\\* \\[1.2.1 P1 \\]",
    "\\* \\[1.2.2 P2 \\]",
    "\\* \\[1.3 Victim \\]",
    "\\* \\[1.3.1 V1 \\]",
    "\\* \\[1.3.2 V2 \\]"
  )
  for(subunit in tocSubunits){
    subunitLine <- grep(pattern = subunit, x = reportContent)
    testthat::expect_true(
      any(grepl(
        pattern = subunit, 
        x = reportContent
        ))
    )
  }
})

# Clear the output
unlink(reOutputFolder, recursive = TRUE)