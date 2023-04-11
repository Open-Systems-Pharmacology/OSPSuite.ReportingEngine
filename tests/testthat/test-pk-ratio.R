context("Qualification PK Ratio")

reOutputFolder <- "report-qualification"
configurationPlanFile <- getTestDataFilePath("qualification/configuration-plan-pk-ratio.json")

# Load and run test qualification workflow
workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)

test_that("Errors and Warnings from ill formatted ObservedData/Configuration Plan are appropriately caught", {
  expect_error(workflow$runWorkflow())
  # Note that all things saved in log-error are displayed on console as a Warning message
  errorsLog <- readLines(file.path(reOutputFolder, "log-error.txt"))
  # expect_match is a practical way of querying the report or log files
  # and check if a line included specific elements
  expect_match(
    object = errorsLog,
    regexp = "In PK Ratio Plots|'CMAX Avg' column is not present in PK Ratio Dataset columns",
    all = FALSE
  )
  expect_match(
    object = errorsLog,
    regexp = "In PK Ratio Plots|0 data record|ObservedDataRecordId '12'",
    all = FALSE
    )
})

unlink(reOutputFolder, recursive = TRUE)
