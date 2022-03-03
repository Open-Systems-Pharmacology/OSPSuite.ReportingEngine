context("Run population workflows with Demography task")

# Get test data
simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
populationFilePeds <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
populationFileAdults <- getTestDataFilePath("input-data/Raltegravir Adult Population.csv")

# List of files necessary in output directory
refWorkflowStructure <- c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "Demography"
)

# Define list of files necessary in Demography directory
demographyStructure <- c(
  paste(c("Gender", "Organism_Age", "Organism_BMI", "Organism_Height", "Organism_Weight"), "Adults.png", sep = "-"),
  paste(c("Gender", "Organism_Age", "Organism_BMI", "Organism_Height", "Organism_Weight"), "Pediatric.png", sep = "-")
)

demographyStructurePeds <- NULL
for (popName in c("Adults", "Pediatric", "Pediatric-vs-ref")) {
  for (parName in c("Organism_BMI", "Organism_Height", "Organism_Weight")) {
    demographyStructurePeds <- c(
      demographyStructurePeds,
      paste(popName, parName, c("vs-Organism_Age.png", "vs-Organism_Age-log.png"), sep = "-")
    )
  }
}

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

workflowPediatric$inactivateTasks()
workflowParallel$inactivateTasks()
workflowRatio$inactivateTasks()
workflowPediatric$activateTasks("plotDemography")
workflowParallel$activateTasks("plotDemography")
workflowRatio$activateTasks("plotDemography")

workflowPediatric$runWorkflow()
workflowParallel$runWorkflow()
workflowRatio$runWorkflow()

test_that("Workflows generate appropriate files and folders", {
  expect_setequal(list.files(workflowPediatric$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowParallel$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowRatio$workflowFolder), refWorkflowStructure)
})

test_that("Demography directory includes appropriate files and folders", {
  expect_setequal(list.files(file.path(workflowPediatric$workflowFolder, "Demography")), demographyStructurePeds)
  expect_setequal(list.files(file.path(workflowParallel$workflowFolder, "Demography")), demographyStructure)
  expect_setequal(list.files(file.path(workflowRatio$workflowFolder, "Demography")), demographyStructure)
})

# Clear test workflow folders
unlink(workflowPediatric$workflowFolder, recursive = TRUE)
unlink(workflowParallel$workflowFolder, recursive = TRUE)
unlink(workflowRatio$workflowFolder, recursive = TRUE)
