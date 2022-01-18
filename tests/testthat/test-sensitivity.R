context("Run workflows with Sensitivity tasks")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
populationFile <- getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
refOutputFolder <- getTestDataFilePath("mean-sensitivity")

refWorkflowStructure <- c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "PKAnalysisResults", "SensitivityResults",
  "Sensitivity"
)
refSensitivityStructure <- c(
  "A-AUC_tEnd-Concentration in container.png",
  "A-C_max-Concentration in container.png"
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
refSensitivityStructure <- c(
  "AUC_tEnd_Organism-A-Concentration in container.png",
  "C_max_Organism-A-Concentration in container.png"
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

