context("Population workflows with different PK parameters")
# Get test data
simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
populationFilePeds <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
populationFileAdults <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")

# List of necessary in results to test
refOutput <- getTestDataFilePath("pop-pk-diff")
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
initialOutput <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c("AUC_tEnd", "C_max")
)
# New PK parameter
outputPK <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c("AUC_tEnd", "C_max", "Thalf")
)
# New output path
outputPath <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma Unbound (Peripheral Venous Blood)",
  displayName = "Unbound Venous Blood",
  pkParameters = c("AUC_tEnd", "C_max")
)

setPeds <- PopulationSimulationSet$new(
  simulationSetName = "Pediatric",
  simulationFile = simulationFile,
  populationFile = populationFilePeds,
  outputs = c(outputPK, outputPath)
)
setAdults <- PopulationSimulationSet$new(
  referencePopulation = TRUE,
  simulationSetName = "Adults",
  simulationFile = simulationFile,
  populationFile = populationFileAdults,
  outputs = initialOutput
)

workflowFolderPediatric <- "test-pk-parameters-pediatric"
workflowFolderParallel <- "test-pk-parameters-parallel"

workflowPediatric <- PopulationWorkflow$new(
  createWordReport = FALSE,
  workflowType = PopulationWorkflowTypes$pediatric,
  simulationSets = c(setAdults, setPeds),
  workflowFolder = workflowFolderPediatric
)
workflowParallel <- PopulationWorkflow$new(
  createWordReport = FALSE,
  workflowType = PopulationWorkflowTypes$parallelComparison,
  simulationSets = c(setAdults, setPeds),
  workflowFolder = workflowFolderParallel
)

# Save time preventing run of same simulations and pk parameter analyses
workflowPediatric$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowParallel$inactivateTasks()
workflowParallel$activateTasks("plotPKParameters")

workflowPediatric$runWorkflow()
# Before running parallel and ratio, save time by copying/pasting simulations and pk parameter analyses
file.copy(from = "test-pk-parameters-pediatric/SimulationResults", to = "test-pk-parameters-parallel", recursive = TRUE)
file.copy(from = "test-pk-parameters-pediatric/PKAnalysisResults", to = "test-pk-parameters-parallel", recursive = TRUE)
workflowParallel$runWorkflow()

test_that("Workflows generate appropriate number of files", {
  # Log files is 3 because of PK parameters union warnings
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".txt"), 3)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".txt"), 3)
  # Reports
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".md"), 1)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".md"), 1)
})

pkAnalysisResultsPath <- file.path(workflowPediatric$workflowFolder, "PKAnalysisResults")
test_that("PKAnalysisResults includes appropriate number of files", {
  expect_length(list.files(pkAnalysisResultsPath, pattern = ".csv"), 2)
})

pediatricPath <- file.path(workflowPediatric$workflowFolder, "PKAnalysis")
parallelPath <- file.path(workflowParallel$workflowFolder, "PKAnalysis")
test_that("PKAnalysis includes appropriate number of files", {
  # Figures
  expect_length(list.files(pediatricPath, pattern = ".png"), 86)
  expect_length(list.files(parallelPath, pattern = ".png"), 14)
  # Exported results
  expect_length(list.files(pediatricPath, pattern = ".csv"), 7)
  expect_length(list.files(parallelPath, pattern = ".csv"), 7)
})

test_that("Saved PK parameters summaries have correct values", {
  pediatricFiles <- getOrderedFiles(pediatricPath, pattern = ".csv")
  parallelFiles <- getOrderedFiles(parallelPath, pattern = ".csv")
  refFiles <- getOrderedFiles(refOutput, pattern = ".csv")
  for (fileIndex in seq_along(refFiles)) {
    expect_equivalent(
      readObservedDataFile(pediatricFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex]),
      tolerance = comparisonTolerance()
    )
    expect_equivalent(
      readObservedDataFile(parallelFiles[fileIndex]),
      readObservedDataFile(refFiles[fileIndex]),
      tolerance = comparisonTolerance()
    )
  }
})

# Ensure the PK parameters are reset after test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

# Clear test workflow folders
unlink(workflowPediatric$workflowFolder, recursive = TRUE)
unlink(workflowParallel$workflowFolder, recursive = TRUE)
