context("Run workflow with absorption task")

simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")

# Output reference absorption time profiles
refOutputAbsorption <- getTestDataFilePath("absorption-results/Larson-Raltegravir.csv")

refWorkflowStructure <- sort(c(
  "appendix-absorption.md",
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "Absorption"
))
absorptionStructure <- sort(c(
  "Larson-Raltegravir.png",
  "Larson-Raltegravir.csv"
))

setAbs <- SimulationSet$new(
  simulationSetName = "Larson",
  simulationFile = simulationFile
)
workflowFolderAbs <- "test-absorption"
workflowAbs <- MeanModelWorkflow$new(simulationSets = setAbs, workflowFolder = workflowFolderAbs)

workflowAbs$inactivateTasks()
workflowAbs$activateTasks("plotAbsorption")
workflowAbs$runWorkflow()


test_that("Workflow generates appropriate files and folders", {
  expect_equal(list.files(workflowAbs$workflowFolder), refWorkflowStructure)
})

test_that("Absorption directory includes appropriate files and folders", {
  expect_equal(list.files(file.path(workflowAbs$workflowFolder, "Absorption")), absorptionStructure)
})

test_that("Saved absorption time profiles have correct values", {
  expect_equal(
    readObservedDataFile(file.path(workflowAbs$workflowFolder, "Absorption", "Larson-Raltegravir.csv")),
    readObservedDataFile(refOutputAbsorption)
  )
})

# Clear test workflow folders
unlink(workflowAbs$workflowFolder, recursive = TRUE)
