context("Run population workflows with PK parameters task")
# Get test data
simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
populationFilePeds <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
populationFileAdults <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")

# List of necessary in results to test
refOutput <- getTestDataFilePath("pop-pk")
# Sort files because list.files ordering is character by character
getOrderedFiles <- function(dirPath, pattern){
  fileNames <- list.files(dirPath, pattern = pattern)
  # Keep only numbers included in filenames
  fileNumbers <- as.numeric(gsub("[^[:digit:]]", "\\1", fileNames))
  return(file.path(dirPath, fileNames[order(fileNumbers)]))
}

# Ensure the PK parameters are reset before test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

setPeds <- PopulationSimulationSet$new(
  simulationSetName = "Pediatric",
  simulationFile = simulationFile,
  populationFile = populationFilePeds,
  outputs = Output$new(
    path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    displayName = "Venous Blood",
    pkParameters = c("AUC_tEnd", "C_max")
  )
)
setAdults <- PopulationSimulationSet$new(
  referencePopulation = TRUE,
  simulationSetName = "Adults",
  simulationFile = simulationFile,
  populationFile = populationFileAdults,
  outputs = Output$new(
    path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
    displayName = "Venous Blood",
    pkParameters = c("AUC_tEnd", "C_max")
  )
)

workflowFolderPediatric <- "test-pk-parameters-pediatric"
workflowFolderParallel <- "test-pk-parameters-parallel"
workflowFolderRatio <- "test-pk-parameters-ratio"

workflowPediatric <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$pediatric,
  simulationSets = c(setAdults, setPeds),
  workflowFolder = workflowFolderPediatric
)
workflowParallel <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$parallelComparison,
  simulationSets = c(setAdults, setPeds),
  workflowFolder = workflowFolderParallel
)
workflowRatio <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$ratioComparison,
  simulationSets = c(setAdults, setPeds),
  workflowFolder = workflowFolderRatio
)

workflowPediatric$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
# Save time preventing run of same simulations and pk parameter analyses
workflowParallel$inactivateTasks()
workflowRatio$inactivateTasks()
workflowParallel$activateTasks("plotPKParameters")
workflowRatio$activateTasks("plotPKParameters")

workflowPediatric$runWorkflow()
# Before running parallel and ratio, save time by copying/pasting simulations and pk parameter analyses
file.copy(from = "test-pk-parameters-pediatric/SimulationResults", to = "test-pk-parameters-parallel", recursive = TRUE)
file.copy(from = "test-pk-parameters-pediatric/PKAnalysisResults", to = "test-pk-parameters-parallel", recursive = TRUE)
file.copy(from = "test-pk-parameters-pediatric/SimulationResults", to = "test-pk-parameters-ratio", recursive = TRUE)
file.copy(from = "test-pk-parameters-pediatric/PKAnalysisResults", to = "test-pk-parameters-ratio", recursive = TRUE)
workflowParallel$runWorkflow()
workflowRatio$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".txt"), 2)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".docx"), 1)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".docx"), 1)
  expect_length(list.files(workflowRatio$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowRatio$workflowFolder, pattern = ".docx"), 1)
})

pkAnalysisPediatricPath <- file.path(workflowPediatric$workflowFolder, "PKAnalysis")
test_that("PKAnalysis directory from Pediatric workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(pkAnalysisPediatricPath, pattern = ".png"), 52)
  # Exported results
  expect_length(list.files(pkAnalysisPediatricPath, pattern = ".csv"), 4)
})

pkAnalysisParallelPath <- file.path(workflowParallel$workflowFolder, "PKAnalysis")
test_that("PKAnalysis directory from Parallel workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(pkAnalysisParallelPath, pattern = ".png"), 4)
  # Exported results
  expect_length(list.files(pkAnalysisParallelPath, pattern = ".csv"), 4)
})

pkAnalysisRatioPath <- file.path(workflowRatio$workflowFolder, "PKAnalysis")
test_that("PKAnalysis directory from Ratio workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(pkAnalysisRatioPath, pattern = ".png"), 8)
  # Exported results
  expect_length(list.files(pkAnalysisRatioPath, pattern = ".csv"), 6)
})

test_that("Saved PK parameters data from workflows have correct values", {
  pediatricFiles <- getOrderedFiles(pkAnalysisPediatricPath, pattern = ".csv")
  parallelFiles <- getOrderedFiles(pkAnalysisParallelPath, pattern = ".csv")
  refFiles <- getOrderedFiles(refOutput, pattern = "pk_parameters")
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
  # For ratio, 2 additional tables are exported
  ratioFiles <- getOrderedFiles(pkAnalysisRatioPath, pattern = ".csv")
  refFiles <- getOrderedFiles(refOutput, pattern = ".csv")
  for (fileIndex in seq_along(refFiles)) {
    expect_equivalent(
      readObservedDataFile(ratioFiles[fileIndex]),
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
unlink(workflowRatio$workflowFolder, recursive = TRUE)
