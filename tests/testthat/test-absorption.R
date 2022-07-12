context("Run workflow with absorption task")

simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")

# Output reference absorption time profiles
refOutputAbsorption <- getTestDataFilePath("absorption/Larson-Raltegravir.csv")

setAbs <- SimulationSet$new(
  simulationSetName = "Larson",
  simulationFile = simulationFile
)
workflowFolderAbs <- "test-absorption"
workflowAbs <- MeanModelWorkflow$new(simulationSets = setAbs, workflowFolder = workflowFolderAbs)

workflowAbs$inactivateTasks()
workflowAbs$activateTasks("plotAbsorption")
workflowAbs$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowAbs$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowAbs$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowAbs$workflowFolder, pattern = ".docx"), 1)
})

absorptionPath <- file.path(workflowAbs$workflowFolder, "Absorption")
           
test_that("Absorption directory includes appropriate number of files", {
  # Figures
  expect_length(list.files(absorptionPath, pattern = ".png"), 1)
  # Exported results
  expect_length(list.files(absorptionPath, pattern = ".csv"), 1)
})

test_that("Saved absorption time profiles have correct values", {
  exportedFile <- file.path(
    absorptionPath,
    list.files(absorptionPath, pattern = ".csv")
  )
  expect_equal(
    readObservedDataFile(exportedFile),
    readObservedDataFile(refOutputAbsorption),
    tolerance = comparisonTolerance()
  )
})

# Clear test workflow folders
unlink(workflowAbs$workflowFolder, recursive = TRUE)
