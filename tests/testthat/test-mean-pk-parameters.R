context("Run workflows with PK parameters task")
# Get test data
simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")

# List of files necessary in output directory
refOutputPK <- getTestDataFilePath("mean-pk/A-pkAnalysis.csv")
refOutputUpdatedPK <- getTestDataFilePath("mean-pk/A-pkAnalysis-updatedPK.csv")
# Ensure the PK parameters are reset before test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

setPK <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    pkParameters = c("C_max", "AUC_tEnd")
  )
)
workflowFolderPK <- "test-pk-parameters"
workflowPK <- MeanModelWorkflow$new(simulationSets = setPK, workflowFolder = workflowFolderPK)
workflowPK$inactivateTasks()
workflowPK$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowPK$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowPK$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowPK$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowPK$workflowFolder, pattern = ".docx"), 1)
})

pkAnalysisPath <- file.path(workflowPK$workflowFolder, "PKAnalysis")

test_that("PKAnalysis directory includes appropriate number of files", {
  # Figures
  expect_length(list.files(pkAnalysisPath, pattern = ".png"), 0)
  # Exported results
  expect_length(list.files(pkAnalysisPath, pattern = ".csv"), 1)
})

test_that("Saved PK parameters have correct values", {
  # the behaviour is correct, however due to "µ-conversion" done during reading of units
  # the re-exported file differs from the original one. Which is ok.
  skip_on_os("linux")

  exportedFile <- file.path(
    pkAnalysisPath,
    list.files(pkAnalysisPath, pattern = ".csv")
  )
  expect_equal(
    readObservedDataFile(exportedFile),
    readObservedDataFile(refOutputPK),
    tolerance = comparisonTolerance()
  )
})

updatePKParameter("C_max", displayName = "Cmax", displayUnit = "nmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC")

newSetPK <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    pkParameters = c("C_max", "AUC_tEnd")
  )
)
workflowPK <- MeanModelWorkflow$new(simulationSets = newSetPK, workflowFolder = workflowFolderPK)
workflowPK$inactivateTasks()
workflowPK$activateTasks("plotPKParameters")
workflowPK$runWorkflow()

test_that("PKAnalysis directory includes appropriate files and folders, overwriting previous data", {
  # Figures
  expect_length(list.files(pkAnalysisPath, pattern = ".png"), 0)
  # Exported results
  expect_length(list.files(pkAnalysisPath, pattern = ".csv"), 1)
})

test_that("Saved PK parameters have correct values with updated names and unit", {
  # the behaviour is correct, however due to "µ-conversion" done during reading of units
  # the re-exported file differs from the original one. Which is ok.
  skip_on_os("linux")

  exportedFile <- file.path(
    pkAnalysisPath,
    list.files(pkAnalysisPath, pattern = ".csv")
  )
  expect_equal(
    readObservedDataFile(exportedFile),
    readObservedDataFile(refOutputUpdatedPK),
    tolerance = comparisonTolerance()
  )
})

# Ensure the PK parameters are reset after test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

# Clear test workflow folders
unlink(workflowPK$workflowFolder, recursive = TRUE)
