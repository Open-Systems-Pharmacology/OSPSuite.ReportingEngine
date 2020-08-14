context("Run workflow with mass balance task")

simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")

# Output reference absorption time profiles
refOutputMassBalance <- getTestDataFilePath("mass-balance/Larson-timeProfiles.csv")

refWorkflowStructure <- sort(c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "MassBalance"
))
massBalanceStructure <- sort(c(
  "Larson-cumulativeTimeProfile.png",
  "Larson-normalizedCumulativeTimeProfile.png",
  "Larson-normalizedTimeProfile.png",
  "Larson-pieChart.png",
  "Larson-timeProfile.png",
  "Larson-timeProfiles.csv"
))

setMB <- SimulationSet$new(
  simulationSetName = "Larson",
  simulationFile = simulationFile
)
workflowFolderMB <- "test-mass-balance"
workflowMB <- MeanModelWorkflow$new(simulationSets = setMB, workflowFolder = workflowFolderMB)

workflowMB$inactivateTasks()
workflowMB$activateTasks("plotMassBalance")
workflowMB$runWorkflow()

test_that("Workflow generates appropriate files and folders", {
  expect_equal(list.files(workflowMB$workflowFolder), refWorkflowStructure)
})

test_that("MassBalance directory includes appropriate files and folders", {
  expect_equal(list.files(file.path(workflowMB$workflowFolder, "MassBalance")), massBalanceStructure)
})

test_that("Saved mass balance time profiles have correct values", {
  expect_equal(
    readObservedDataFile(file.path(workflowMB$workflowFolder, "MassBalance", "Larson-timeProfiles.csv")),
    readObservedDataFile(refOutputMassBalance)
  )
})

# Clear test workflow folders
unlink(workflowMB$workflowFolder, recursive = TRUE)
