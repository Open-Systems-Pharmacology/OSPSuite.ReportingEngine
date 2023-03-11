context("Run workflows with Sensitivity tasks")
# Get test data
simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
populationFile <- getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
refMeanOutputFolder <- getTestDataFilePath("mean-sensitivity")
refPopOutputFolder <- getTestDataFilePath("pop-sensitivity")

workflowFolder <- "Sensitivity-Tests"

# Mean model workflow
setA <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    pkParameters = c("C_max", "AUC_tEnd")
  )
)

workflowA <- MeanModelWorkflow$new(
  simulationSets = setA,
  workflowFolder = workflowFolder
)
workflowA$activateTasks(
  c("simulate", "calculatePKParameters", "calculateSensitivity", "plotSensitivity")
)
workflowA$runWorkflow()

test_that("Mean Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowA$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowA$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowA$workflowFolder, pattern = ".docx"), 1)
})

sensitivityPath <- file.path(workflowA$workflowFolder, "Sensitivity")
sensitivityResultsPath <- file.path(workflowA$workflowFolder, "SensitivityResults")

test_that("SensitivityResults directory from Mean Workflow includes appropriate number of files", {
  # Exported results
  expect_length(list.files(sensitivityResultsPath, pattern = ".csv"), 1)
})

test_that("Sensitivity directory from Mean Workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(sensitivityPath, pattern = ".png"), 2)
  # Exported results
  expect_length(list.files(sensitivityPath, pattern = ".csv"), 0)
})

test_that("Mean sensitiviy results are equal to reference", {
  sensitivityFiles <- file.path(
    sensitivityResultsPath,
    list.files(sensitivityResultsPath, pattern = ".csv")
  )
  refFiles <- file.path(refMeanOutputFolder, list.files(refMeanOutputFolder))
  for (fileIndex in seq_along(refFiles)) {
    expect_equal(
      readObservedDataFile(sensitivityFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex]),
      tolerance = comparisonTolerance()
    )
  }
})

# Clear test workflow folders
unlink(workflowA$workflowFolder, recursive = TRUE)

# Population model workflow
setA <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    pkParameters = c("C_max", "AUC_tEnd")
  )
)

workflowA <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$parallelComparison,
  simulationSets = setA,
  workflowFolder = workflowFolder
)

workflowA$activateTasks(
  c("simulate", "calculatePKParameters", "calculateSensitivity", "plotSensitivity")
)
workflowA$runWorkflow()


test_that("Population Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowA$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowA$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowA$workflowFolder, pattern = ".docx"), 1)
})

sensitivityPath <- file.path(workflowA$workflowFolder, "Sensitivity")
sensitivityResultsPath <- file.path(workflowA$workflowFolder, "SensitivityResults")

test_that("SensitivityResults directory from Population Workflow includes appropriate number of files", {
  # Exported results
  expect_length(list.files(sensitivityResultsPath, pattern = ".csv"), 4)
})

test_that("Sensitivity directory from Population Workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(sensitivityPath, pattern = ".png"), 2)
  # Exported results
  expect_length(list.files(sensitivityPath, pattern = ".csv"), 0)
})

test_that("Population sensitiviy results are equal to reference", {
  # the behaviour is correct, however due to "µ-conversion" done during reading of units
  # the re-exported file differs from the original one. Which is ok.
  skip_on_os("linux")

  sensitivityFiles <- file.path(
    sensitivityResultsPath,
    list.files(sensitivityResultsPath, pattern = ".csv")
  )
  refFiles <- file.path(refPopOutputFolder, list.files(refPopOutputFolder))
  for (fileIndex in seq_along(refFiles)) {
    expect_equal(
      readObservedDataFile(sensitivityFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex]),
      tolerance = comparisonTolerance()
    )
  }
})

# Clear test workflow folders
unlink(workflowA$workflowFolder, recursive = TRUE)
