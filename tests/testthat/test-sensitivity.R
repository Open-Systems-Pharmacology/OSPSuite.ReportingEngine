context("Run workflows with Sensitivity tasks")
# Get test data
simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
populationFile <- getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
refOutputFolder <- getTestDataFilePath("mean-sensitivity")

# List of files necessary in output directory
refWorkflowStructure <- c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "PKAnalysisResults", "SensitivityResults",
  "Sensitivity"
)
# List of files necessary in Sensitivity directory
refSensitivityStructure <- c(
  "A-Organism_A_Concentration_in_container-AUC_tEnd.png",
  "A-Organism_A_Concentration_in_container-C_max.png"
)
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

test_that("Mean workflows generate appropriate files and folders", {
  expect_setequal(
    list.files(workflowA$workflowFolder),
    refWorkflowStructure
  )
  expect_setequal(
    list.files(file.path(workflowA$workflowFolder, "SensitivityResults")),
    list.files(refOutputFolder)
  )
  expect_setequal(
    list.files(file.path(workflowA$workflowFolder, "Sensitivity")),
    refSensitivityStructure
  )
})

test_that("Mean sensitiviy results are equal to reference", {
  for(fileName in list.files(refOutputFolder)){
    refData <- readObservedDataFile(file.path(refOutputFolder, fileName))
    testData <- readObservedDataFile(file.path(workflowA$workflowFolder, "SensitivityResults", fileName))
    expect_equal(
      refData,
      testData,
      tolerance = comparisonTolerance()
    )
  }
})

# Clear test workflow folders
unlink(workflowA$workflowFolder, recursive = TRUE)

# Population model workflow
refOutputFolder <- getTestDataFilePath("pop-sensitivity")

# List of files necessary in Sensitivity directory
refSensitivityStructure <- c(
  "Organism_A_Concentration_in_container-AUC_tEnd.png",
  "Organism_A_Concentration_in_container-C_max.png"
)

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

test_that("Population workflows generate appropriate files and folders", {
  expect_setequal(
    list.files(workflowA$workflowFolder),
    refWorkflowStructure
  )
  expect_setequal(
    list.files(file.path(workflowA$workflowFolder, "SensitivityResults")),
    list.files(refOutputFolder)
  )
  expect_setequal(
    list.files(file.path(workflowA$workflowFolder, "Sensitivity")),
    refSensitivityStructure
  )
})

test_that("Population sensitiviy results are equal to reference", {
  skip_on_os("linux") # the behaviour is correct, however due to "µ-conversion" done during reading of units
  # the re-exported file differs from the original one. Which is ok.
  for(fileName in list.files(refOutputFolder)){
    refData <- readObservedDataFile(file.path(refOutputFolder, fileName))
    testData <- readObservedDataFile(file.path(workflowA$workflowFolder, "SensitivityResults", fileName))
    expect_equal(
      refData,
      testData,
      tolerance = comparisonTolerance()
    )
  }
})

# Clear test workflow folders
unlink(workflowA$workflowFolder, recursive = TRUE)
