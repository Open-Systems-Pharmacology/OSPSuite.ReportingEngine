context("Run population workflows with PK parameters task")

simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
populationFilePeds <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
populationFileAdults <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")

# Output reference
refOutputParallelCmax <- getTestDataFilePath("pop-pk/Plasma-C_max.csv")
refOutputParallelAUC <- getTestDataFilePath("pop-pk/Plasma-AUC_tEnd.csv")
refOutputRatioCmax <- getTestDataFilePath("pop-pk/Plasma-C_max-ratio.csv")
refOutputRatioAUC <- getTestDataFilePath("pop-pk/Plasma-AUC_tEnd-ratio.csv")

# Ensure the PK parameters are reset before test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

refWorkflowStructure <- sort(c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "PKAnalysisResults", "PKAnalysis"
))

pkParametersStructure <- sort(c(
  paste("Plasma (Peripheral Venous Blood)-AUC_tEnd", c(".csv", ".png", "-log.png"), sep = ""),
  paste("Plasma (Peripheral Venous Blood)-C_max", c(".csv", ".png", "-log.png"), sep = "")
))

pkParametersStructurePeds <- pkParametersStructure
for (popName in c("Adults", "Pediatric", "Pediatric-vs-ref")) {
  for (parName in c("Age", "BMI", "Height", "Weight")) {
    pkParametersStructurePeds <- c(
      pkParametersStructurePeds,
      paste(popName, "-", c("AUC_tEnd", "C_max"), "-vs-", parName, ".png", sep = ""),
      paste(popName, "-", c("AUC_tEnd", "C_max"), "-vs-", parName, "-log.png", sep = "")
    )
  }
}
pkParametersStructurePeds <- sort(pkParametersStructurePeds)

pkParametersStructureRatio <- sort(c(
  pkParametersStructure,
  paste("Plasma (Peripheral Venous Blood)-AUC_tEnd", "-ratio", c(".csv", ".png", "-log.png"), sep = ""),
  paste("Plasma (Peripheral Venous Blood)-C_max", "-ratio", c(".csv", ".png", "-log.png"), sep = "")
))

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
workflowParallel$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowRatio$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))

workflowPediatric$runWorkflow()
workflowParallel$runWorkflow()
workflowRatio$runWorkflow()

test_that("Workflows generate appropriate files and folders", {
  expect_equal(list.files(workflowPediatric$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowParallel$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowRatio$workflowFolder), refWorkflowStructure)
})

test_that("PKAnalysis directory includes appropriate files and folders", {
  expect_equal(sort(list.files(file.path(workflowPediatric$workflowFolder, "PKAnalysis"))), sort(pkParametersStructurePeds))
  expect_equal(sort(list.files(file.path(workflowParallel$workflowFolder, "PKAnalysis"))), sort(pkParametersStructure))
  expect_equal(sort(list.files(file.path(workflowRatio$workflowFolder, "PKAnalysis"))), sort(pkParametersStructureRatio))
})

test_that("Saved PK parameters data have correct values", {
  expect_equal(
    readObservedDataFile(file.path(workflowParallel$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-AUC_tEnd.csv")),
    readObservedDataFile(refOutputParallelAUC),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowParallel$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-C_max.csv")),
    readObservedDataFile(refOutputParallelCmax),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowPediatric$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-AUC_tEnd.csv")),
    readObservedDataFile(refOutputParallelAUC),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowPediatric$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-C_max.csv")),
    readObservedDataFile(refOutputParallelCmax),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowRatio$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-AUC_tEnd.csv")),
    readObservedDataFile(refOutputParallelAUC),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowRatio$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-C_max.csv")),
    readObservedDataFile(refOutputParallelCmax),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowRatio$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-AUC_tEnd-ratio.csv")),
    readObservedDataFile(refOutputRatioAUC),
    tolerance = comparisonTolerance()
  )

  expect_equal(
    readObservedDataFile(file.path(workflowRatio$workflowFolder, "PKAnalysis", "Plasma (Peripheral Venous Blood)-C_max-ratio.csv")),
    readObservedDataFile(refOutputRatioCmax),
    tolerance = comparisonTolerance()
  )
})

# Ensure the PK parameters are reset after test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "µmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "µmol*min/l")

# Clear test workflow folders
unlink(workflowPediatric$workflowFolder, recursive = TRUE)
unlink(workflowParallel$workflowFolder, recursive = TRUE)
unlink(workflowRatio$workflowFolder, recursive = TRUE)
