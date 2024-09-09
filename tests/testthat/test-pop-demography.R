context("Run population workflows with Demography task")

# Get test data
simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
populationFilePeds <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
populationFileAdults <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")

# Define list of files necessary in Demography directory
setPeds <- PopulationSimulationSet$new(
  simulationSetName = "Pediatric",
  simulationFile = simulationFile,
  populationFile = populationFilePeds
)
setAdults <- PopulationSimulationSet$new(
  referencePopulation = TRUE,
  simulationSetName = "Adults",
  simulationFile = simulationFile,
  populationFile = populationFileAdults
)

workflowFolderPediatric <- "test-demography-pediatric"
workflowFolderParallel <- "test-demography-parallel"
workflowFolderRatio <- "test-demography-ratio"

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

test_that("Reference population is well defined", {
  expect_equal("Adults", getWorkflowReferencePopulationName(workflowParallel))
  expect_equal("Adults", getWorkflowReferencePopulationName(workflowRatio))
  expect_equal("Adults", getWorkflowReferencePopulationName(workflowPediatric))
})

test_that("Default Demography parameters are well defined", {
  expect_equal(
    getDefaultDemographyXParameters(PopulationWorkflowTypes$parallelComparison),
    getXParametersForDemographyPlot(workflowParallel)
  )
  expect_equal(
    getDefaultDemographyXParameters(PopulationWorkflowTypes$ratioComparison),
    getXParametersForDemographyPlot(workflowRatio)
  )
  expect_equal(
    getDefaultDemographyXParameters(PopulationWorkflowTypes$pediatric),
    getXParametersForDemographyPlot(workflowPediatric)
  )
})

workflowPediatric$inactivateTasks()
workflowParallel$inactivateTasks()
workflowRatio$inactivateTasks()
workflowPediatric$activateTasks("plotDemography")
workflowParallel$activateTasks("plotDemography")
workflowRatio$activateTasks("plotDemography")

workflowPediatric$runWorkflow()
workflowParallel$runWorkflow()
workflowRatio$runWorkflow()

test_that("Workflow generates appropriate number of files", {
  # Log files
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".txt"), 2)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".txt"), 2)
  expect_length(list.files(workflowRatio$workflowFolder, pattern = ".txt"), 2)
  # Reports
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowPediatric$workflowFolder, pattern = ".docx"), 1)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowParallel$workflowFolder, pattern = ".docx"), 1)
  expect_length(list.files(workflowRatio$workflowFolder, pattern = ".md"), 2)
  expect_length(list.files(workflowRatio$workflowFolder, pattern = ".docx"), 1)
})

demographyPediatricPath <- file.path(workflowPediatric$workflowFolder, "Demography")
test_that("Demography directory from Pediatric workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(demographyPediatricPath, pattern = ".png"), 9)
  # Exported results
  expect_length(list.files(demographyPediatricPath, pattern = ".csv"), 0)
})

demographyParallelPath <- file.path(workflowParallel$workflowFolder, "Demography")
test_that("Demography directory from Parallel workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(demographyParallelPath, pattern = ".png"), 10)
  # Exported results
  expect_length(list.files(demographyParallelPath, pattern = ".csv"), 0)
})

demographyRatioPath <- file.path(workflowRatio$workflowFolder, "Demography")
test_that("Demography directory from Ratio workflow includes appropriate number of files", {
  # Figures
  expect_length(list.files(demographyRatioPath, pattern = ".png"), 10)
  # Exported results
  expect_length(list.files(demographyRatioPath, pattern = ".csv"), 0)
})

# Clear test workflow folders
unlink(workflowPediatric$workflowFolder, recursive = TRUE)
unlink(workflowParallel$workflowFolder, recursive = TRUE)
unlink(workflowRatio$workflowFolder, recursive = TRUE)
