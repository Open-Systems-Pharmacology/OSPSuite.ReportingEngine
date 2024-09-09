context("Run workflow with mass balance task")

simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")

# Output reference absorption time profiles
refOutputMassBalance <- getTestDataFilePath("mass-balance/Larson-timeProfiles.csv")

setMB <- SimulationSet$new(
  simulationSetName = "Larson",
  simulationFile = simulationFile
)
workflowFolderMB <- "test-mass-balance"
workflowMB <- MeanModelWorkflow$new(simulationSets = setMB, workflowFolder = workflowFolderMB)

workflowMB$inactivateTasks()
workflowMB$activateTasks("plotMassBalance")
workflowMB$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files include warning as exosome does not contain any path in simulation
  expect_length(list.files(workflowMB$workflowFolder, pattern = ".txt"), 3)
  # Reports
  expect_length(list.files(workflowMB$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowMB$workflowFolder, pattern = ".docx"), 1)
})

massBalancePath <- file.path(workflowMB$workflowFolder, "MassBalance")

test_that("MassBalance directory includes appropriate number of files", {
  # Figures
  expect_length(list.files(massBalancePath, pattern = ".png"), 5)
  # Exported results
  expect_length(list.files(massBalancePath, pattern = ".csv"), 1)
})

test_that("Saved mass balance time profiles have correct values", {
  exportedFile <- file.path(
    massBalancePath,
    list.files(massBalancePath, pattern = ".csv")
  )
  expect_equal(
    readObservedDataFile(exportedFile),
    readObservedDataFile(refOutputMassBalance),
    tolerance = comparisonTolerance()
  )
})

# Clear test workflow folders
unlink(workflowMB$workflowFolder, recursive = TRUE)
