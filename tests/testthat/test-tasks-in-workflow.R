context("Tasks in workflows")
# It seems that the SimulationSet instance crashes if library(ospsuite) is not loaded
library(ospsuite)

# Make sure testFolder is not there
testFolder <- "testFolder"
unlink(testFolder, recursive = TRUE)

simSet <- SimulationSet$new(simulationFile = "test.pkml")

mWorkflow <- MeanModelWorkflow$new(
  simulationSets = list(simSet),
  workflowFolder = testFolder
)

test_that("Workflow tasks are defined with appropriate names and can be activated/inactivated through their name", {
  taskNames <- mWorkflow$getAllTasks()
  expect_is(taskNames, "character")

  expect_equal(
    taskNames,
    c(
      "resetReport", "plotSensitivity", "plotPKParameters", "plotAbsorption", "plotMassBalance",
      "plotGoF", "meanModelSensitivityAnalysis", "meanModelPKParameters", "simulate"
    )
  )

  expect_equal(mWorkflow$getActiveTasks(), "simulate")

  # So far nothing is printed when the method activate/inactivate is called,
  # Let me know if this should change
  expect_silent(mWorkflow$inactivateTasks("simulate"))
  expect_null(mWorkflow$getActiveTasks())

  expect_silent(mWorkflow$activateTasks("plotGoF"))
  expect_equal(mWorkflow$getActiveTasks(), "plotGoF")
})

test_that("Workflow and tasks print methods print give back character message of what workflow and task do", {
  expect_is(mWorkflow$simulate$print(), "character")

  workflowPrint <- mWorkflow$print()
  expect_is(workflowPrint, "list")

  taskNames <- mWorkflow$getAllTasks()

  for (taskIndex in seq_along(workflowPrint)) {
    expect_true(grepl(
      taskNames[taskIndex],
      names(workflowPrint)[taskIndex]
    ))

    expect_is(workflowPrint[[taskIndex]], "character")
  }
})

# Remove folder created  by test process
unlink(testFolder, recursive = TRUE)
