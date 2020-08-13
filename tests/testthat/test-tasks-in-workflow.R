context("Tasks in workflows")

meanTestFolder <- "mean-Results"
popTestFolder <- "pop-Results"

meanSimSet <- SimulationSet$new(
  simulationSetName = "myTest",
  simulationFile = getTestDataFilePath("input-data/MiniModel2.pkml")
)
popSimSet <- PopulationSimulationSet$new(
  simulationSetName = "myTest",
  simulationFile = getTestDataFilePath("input-data/MiniModel2.pkml"),
  populationFile = getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
)

mWorkflow <- MeanModelWorkflow$new(
  simulationSets = meanSimSet,
  workflowFolder = meanTestFolder
)
pWorkflow <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$parallelComparison,
  simulationSets = popSimSet,
  workflowFolder = popTestFolder
)

test_that("Mean model workflow tasks are defined with appropriate names and can be activated/inactivated using their name", {
  taskNames <- mWorkflow$getAllTasks()
  plotTaskNames <- mWorkflow$getAllPlotTasks()
  expect_is(taskNames, "character")
  expect_is(plotTaskNames, "character")
  expect_equal(sort(taskNames), sort(c(
    "plotSensitivity", "plotPKParameters", "plotAbsorption", "plotMassBalance", "plotTimeProfilesAndResiduals",
    "calculateSensitivity", "calculatePKParameters", "simulate"
  )))
  expect_equal(sort(plotTaskNames), sort(c("plotSensitivity", "plotPKParameters", "plotAbsorption", "plotMassBalance", "plotTimeProfilesAndResiduals")))
  expect_equal(mWorkflow$getActiveTasks(), "simulate")
  # So far nothing is printed when the method activate/inactivate is called,
  # Let me know if this should change
  expect_silent(mWorkflow$inactivateTasks("simulate"))
  expect_null(mWorkflow$getActiveTasks())
  expect_silent(mWorkflow$activateTasks("plotTimeProfilesAndResiduals"))
  expect_equal(mWorkflow$getActiveTasks(), "plotTimeProfilesAndResiduals")
  expect_silent(mWorkflow$activateTasks())
  expect_equal(mWorkflow$getActiveTasks(), mWorkflow$getAllTasks())
  expect_silent(mWorkflow$inactivateTasks())
  expect_null(mWorkflow$getActiveTasks())
})

test_that("Population workflow tasks are defined with appropriate names and can be activated/inactivated using their name", {
  taskNames <- pWorkflow$getAllTasks()
  plotTaskNames <- pWorkflow$getAllPlotTasks()
  expect_is(taskNames, "character")
  expect_is(plotTaskNames, "character")
  expect_equal(sort(taskNames), sort(c(
    "plotSensitivity", "plotPKParameters", "plotTimeProfilesAndResiduals", "plotDemography",
    "calculateSensitivity", "calculatePKParameters", "simulate"
  )))
  expect_equal(sort(plotTaskNames), sort(c("plotSensitivity", "plotPKParameters", "plotTimeProfilesAndResiduals", "plotDemography")))
  expect_equal(pWorkflow$getActiveTasks(), "simulate")
  # So far nothing is printed when the method activate/inactivate is called,
  # Let me know if this should change
  expect_silent(pWorkflow$inactivateTasks("simulate"))
  expect_null(pWorkflow$getActiveTasks())
  expect_silent(pWorkflow$activateTasks("plotTimeProfilesAndResiduals"))
  expect_equal(pWorkflow$getActiveTasks(), "plotTimeProfilesAndResiduals")
  expect_silent(pWorkflow$activateTasks())
  expect_equal(pWorkflow$getActiveTasks(), pWorkflow$getAllTasks())
  expect_silent(pWorkflow$inactivateTasks())
  expect_null(pWorkflow$getActiveTasks())
})

test_that("Mean model workflow task required inputs are appropriately defined", {
  expect_null(getTaskInputs(mWorkflow$simulate))
  expect_equal(checkTaskInputsExist(mWorkflow$simulate), list())
  expect_null(getTaskInputs(mWorkflow$calculateSensitivity))
  expect_null(getTaskInputs(mWorkflow$plotAbsorption))
  expect_null(getTaskInputs(mWorkflow$plotMassBalance))

  expect_equal(getTaskInputs(mWorkflow$calculatePKParameters), file.path(mWorkflow$workflowFolder, "SimulationResults", "myTest-SimulationResults.csv"))
  expect_false(checkTaskInputsExist(mWorkflow$calculatePKParameters))
  expect_equal(getTaskInputs(mWorkflow$plotTimeProfilesAndResiduals), file.path(mWorkflow$workflowFolder, "SimulationResults", "myTest-SimulationResults.csv"))
  expect_equal(getTaskInputs(mWorkflow$plotPKParameters), file.path(mWorkflow$workflowFolder, "PKAnalysisResults", "myTest-PKAnalysisResults.csv"))
  expect_equal(getTaskInputs(mWorkflow$plotSensitivity), file.path(mWorkflow$workflowFolder, "SensitivityResults", "myTest-SensitivityAnalysisResults.csv"))
})

test_that("Population workflow task required inputs are appropriately defined", {
  expect_null(getTaskInputs(pWorkflow$simulate))
  expect_equal(checkTaskInputsExist(pWorkflow$simulate), list())
  expect_null(getTaskInputs(pWorkflow$plotDemography))

  expect_equal(getTaskInputs(pWorkflow$calculatePKParameters), file.path(pWorkflow$workflowFolder, "SimulationResults", "myTest-SimulationResults.csv"))
  expect_equal(getTaskInputs(pWorkflow$calculateSensitivity), file.path(pWorkflow$workflowFolder, "PKAnalysisResults", "myTest-PKAnalysisResults.csv"))
  expect_false(checkTaskInputsExist(pWorkflow$calculatePKParameters))
  expect_equal(getTaskInputs(pWorkflow$plotTimeProfilesAndResiduals), file.path(pWorkflow$workflowFolder, "SimulationResults", "myTest-SimulationResults.csv"))
  expect_equal(getTaskInputs(pWorkflow$plotPKParameters), file.path(pWorkflow$workflowFolder, "PKAnalysisResults", "myTest-PKAnalysisResults.csv"))
  expect_equal(getTaskInputs(pWorkflow$plotSensitivity), file.path(pWorkflow$workflowFolder, "SensitivityResults", "myTest-popSensitivityResultsIndex.csv"))
})

# Remove folder created  by test process
unlink(meanTestFolder, recursive = TRUE)
unlink(popTestFolder, recursive = TRUE)
