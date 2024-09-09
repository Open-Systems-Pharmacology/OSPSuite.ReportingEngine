context("Run Ratio Comparison workflows")
# Get test data
simulationFile1 <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
simulationFile2 <- getTestDataFilePath("input-data/RaltegravirSim.pkml")
populationFile1 <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")
populationFile2 <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")

# List of necessary in results to test
refOutput <- getTestDataFilePath("ratio-comparison")
# Sort files because list.files ordering is character by character
getOrderedFiles <- function(dirPath, pattern) {
  fileNames <- list.files(dirPath, pattern = pattern)
  # Keep only numbers included in filenames
  fileNumbers <- as.numeric(gsub("[^[:digit:]]", "\\1", fileNames))
  return(file.path(dirPath, fileNames[order(fileNumbers)]))
}

# Ensure the PK parameters are reset before test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")
output1 <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c("AUC_tEnd", "C_max")
)

refSet <- PopulationSimulationSet$new(
  referencePopulation = TRUE,
  simulationSetName = "Ref Set",
  simulationFile = simulationFile1,
  populationFile = populationFile1,
  outputs = output1
)
sameSet <- PopulationSimulationSet$new(
  simulationSetName = "Same Set",
  simulationFile = simulationFile2,
  populationFile = populationFile1,
  outputs = output1
)
diffSet <- PopulationSimulationSet$new(
  simulationSetName = "Diff Set",
  simulationFile = simulationFile1,
  populationFile = populationFile2,
  outputs = output1
)

workflowFolderRatioSame <- "test-ratio-same"
workflowFolderRatioDiff <- "test-ratio-monte"

workflowSame <- PopulationWorkflow$new(
  createWordReport = FALSE,
  workflowType = PopulationWorkflowTypes$ratioComparison,
  simulationSets = c(refSet, sameSet),
  workflowFolder = workflowFolderRatioSame
)
workflowDiff <- PopulationWorkflow$new(
  createWordReport = FALSE,
  workflowType = PopulationWorkflowTypes$ratioComparison,
  simulationSets = c(refSet, diffSet),
  workflowFolder = workflowFolderRatioDiff
)

workflowSame$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowDiff$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowDiff$calculatePKParameters$settings$mcRepetitions <- 100

workflowSame$runWorkflow()
workflowDiff$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowSame$workflowFolder, pattern = ".txt"), 2)
  expect_length(list.files(workflowDiff$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowSame$workflowFolder, pattern = ".md"), 1)
  expect_length(list.files(workflowDiff$workflowFolder, pattern = ".md"), 1)
})

pkFolderSame <- file.path(workflowSame$workflowFolder, "PKAnalysisResults")
pkFolderDiff <- file.path(workflowDiff$workflowFolder, "PKAnalysisResults")
test_that("PKAnalysisResults includes appropriate number of files", {
  expect_length(list.files(pkFolderSame, pattern = ".csv"), 3)
  expect_length(list.files(pkFolderSame, pattern = ".csv"), 3)
})

pkFolderSame <- file.path(workflowSame$workflowFolder, "PKAnalysis")
pkFolderDiff <- file.path(workflowDiff$workflowFolder, "PKAnalysis")
test_that("PKAnalysis folder includes appropriate number of files", {
  # Figures
  expect_length(list.files(pkFolderSame, pattern = ".png"), 12)
  expect_length(list.files(pkFolderDiff, pattern = ".png"), 12)
  # Exported results
  expect_length(list.files(pkFolderSame, pattern = ".csv"), 6)
  expect_length(list.files(pkFolderDiff, pattern = ".csv"), 6)
})

test_that("Saved PK parameters summaries have correct values", {
  sameFiles <- getOrderedFiles(pkFolderSame, pattern = ".csv")
  diffFiles <- getOrderedFiles(pkFolderDiff, pattern = ".csv")
  refFiles <- getOrderedFiles(refOutput, pattern = ".csv")
  for (fileIndex in seq_along(sameFiles)) {
    expect_equivalent(
      readObservedDataFile(sameFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex]),
      tolerance = comparisonTolerance()
    )
    expect_equivalent(
      readObservedDataFile(diffFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex + length(sameFiles)]),
      tolerance = comparisonTolerance()
    )
  }
})

# Ensure the PK parameters are reset after test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

# Clear test workflow folders
unlink(workflowSame$workflowFolder, recursive = TRUE)
unlink(workflowDiff$workflowFolder, recursive = TRUE)
