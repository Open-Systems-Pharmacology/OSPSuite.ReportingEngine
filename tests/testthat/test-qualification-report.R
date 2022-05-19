context("Qualification Report Paths")

reOutputFolder <- "report-qualification"
configurationPlanFile <- getTestDataFilePath("qualification/report-configuration-plan.json")

workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)

test_that("Default markdown and word reports work as expected", {
  workflow$createWordReport <- TRUE
  workflow$runWorkflow()
  
  expect_equal(workflow$reportFolder, workflow$workflowFolder)
  expect_equal(workflow$reportFileName, "Report.md")
  expect_equal(workflow$reportFilePath, file.path(workflow$workflowFolder, "Report.md"))
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_true(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  
  workflow$createWordReport <- FALSE
  workflow$runWorkflow()
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_false(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
})

workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)

test_that("Same folder but different report name works as expected", {
  workflow$createWordReport <- TRUE
  workflow$reportFileName <- "My_report.md"
  workflow$runWorkflow()
  
  expect_equal(workflow$reportFolder, workflow$workflowFolder)
  expect_equal(workflow$reportFileName, "My_report.md")
  expect_equal(workflow$reportFilePath, file.path(workflow$workflowFolder, "My_report.md"))
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_true(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  
  workflow$createWordReport <- FALSE
  workflow$runWorkflow()
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_false(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
})

workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)

test_that("Other folder but same report name works as expected", {
  workflow$createWordReport <- TRUE
  workflow$reportFolder <- "Report"
  workflow$runWorkflow()
  
  expect_equal(workflow$reportFolder, "Report")
  expect_equal(workflow$reportFileName, "Report.md")
  expect_equal(workflow$reportFilePath, file.path("Report", "Report.md"))
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_true(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  unlink("Report", recursive = TRUE)
  
  workflow$createWordReport <- FALSE
  workflow$runWorkflow()
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_false(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  unlink("Report", recursive = TRUE)
})

workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)

test_that("Other folder and other report name works as expected", {
  workflow$createWordReport <- TRUE
  workflow$reportFilePath <- "Report/My_report.md"
  workflow$runWorkflow()
  
  expect_equal(workflow$reportFolder, "Report")
  expect_equal(workflow$reportFileName, "My_report.md")
  expect_equal(workflow$reportFilePath, file.path("Report", "My_report.md"))
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_true(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  unlink("Report", recursive = TRUE)
  
  workflow$createWordReport <- FALSE
  workflow$runWorkflow()
  
  expect_true(file.exists(workflow$reportFilePath))
  expect_false(file.exists(gsub(pattern = ".md", replacement = ".docx", workflow$reportFilePath)))
  
  unlink(reOutputFolder, recursive = TRUE)
  unlink("Report", recursive = TRUE)
})