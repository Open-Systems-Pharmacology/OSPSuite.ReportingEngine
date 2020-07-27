context("Run workflows with PK parameters task")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")

# Output reference absorption time profiles
refOutputPK <- getTestDataFilePath("mean-pk-parameters-results/A-pkAnalysis.csv")
refOutputUpdatedPK <- getTestDataFilePath("mean-pk-parameters-results/A-pkAnalysis-updatedPK.csv")
# Ensure the PK parameters are reset before test
updatePKParameter("C_max", displayName = "C_max", displayUnit = "μmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC_tEnd", displayUnit = "μmol*min/l")

refWorkflowStructure <- sort(c(
  "appendix-pk-parameters.md",
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "PKAnalysisResults", "PKAnalysis"
))
pkStructure <- sort(c(
  "A-pkAnalysis.csv"
))

setPK <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(path = "Organism|A|Concentration in container",
                       pkParameters = c("C_max", "AUC_tEnd"))
)
workflowFolderPK <- "test-pk-parameters"
workflowPK <- MeanModelWorkflow$new(simulationSets = setPK, workflowFolder = workflowFolderPK)
workflowPK$inactivateTasks()
workflowPK$activateTasks(c("simulate", "calculatePKParameters", "plotPKParameters"))
workflowPK$runWorkflow()

test_that("Workflow generates appropriate files and folders", {
  expect_equal(list.files(workflowPK$workflowFolder), refWorkflowStructure)
})

test_that("PKAnalysis directory includes appropriate files and folders", {
  expect_equal(list.files(file.path(workflowPK$workflowFolder, "PKAnalysis")), pkStructure)
})

test_that("Saved PK parameters have correct values", {
  expect_equal(
    readObservedDataFile(file.path(workflowPK$workflowFolder, "PKAnalysis", "A-pkAnalysis.csv")),
    readObservedDataFile(refOutputPK)
  )
})

updatePKParameter("C_max", displayName = "Cmax", displayUnit = "nmol/l")
updatePKParameter("AUC_tEnd", displayName = "AUC")
newSetPK <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(path = "Organism|A|Concentration in container",
                       displayName = "Concentration of A",
                       pkParameters = c("C_max", "AUC_tEnd"))
)
workflowPK <- MeanModelWorkflow$new(simulationSets = newSetPK, workflowFolder = workflowFolderPK)
workflowPK$inactivateTasks()
workflowPK$activateTasks("plotPKParameters")
workflowPK$runWorkflow()

test_that("PKAnalysis directory includes appropriate files and folders, overwriting previous data", {
  expect_equal(list.files(file.path(workflowPK$workflowFolder, "PKAnalysis")), pkStructure)
})

test_that("Saved PK parameters have correct values with updated names and unit", {
  expect_equal(
    readObservedDataFile(file.path(workflowPK$workflowFolder, "PKAnalysis", "A-pkAnalysis.csv")),
    readObservedDataFile(refOutputUpdatedPK)
  )
})

# Clear test workflow folders
unlink(workflowPK$workflowFolder, recursive = TRUE)