context("Initialize workflows")
library(ospsuite.reportingengine)

test_that("Workflows can't be initialized without 'simulationSets' or/and 'workflowFolder'", {
  expect_error(Workflow$new())
  expect_error(MeanModelWorkflow$new())
  expect_error(PopulationWorkflow$new())

  expect_error(Workflow$new(simulationSets = list()))
  expect_error(MeanModelWorkflow$new(simulationSets = list()))
  expect_error(PopulationWorkflow$new(simulationSets = list()))

  testFolder <- "testFolder"
  expect_error(Workflow$new(workflowFolder = testFolder))
  unlink(testFolder, recursive = TRUE)
  expect_error(MeanModelWorkflow$new(workflowFolder = testFolder))
  unlink(testFolder, recursive = TRUE)
  expect_error(PopulationWorkflow$new(workflowFolder = testFolder))
  unlink(testFolder, recursive = TRUE)
})

test_that("Workflows initialization creates appropriate folder and logs, and warn user if the folder does exist", {
  # Make sure testFolder is not there
  testFolder <- "testFolder"
  unlink(testFolder, recursive = TRUE)

  simSet <- SimulationSet$new(
    simulationSetName = "myTest",
    simulationFile = getTestDataFilePath("input-data/MiniModel2.pkml")
  )
  popSimSet <- PopulationSimulationSet$new(
    simulationSetName = "myTest",
    simulationFile = getTestDataFilePath("input-data/MiniModel2.pkml"),
    populationFile = "test.csv"
  )
  # Dummy simulation set for the example
  expect_output(mWorkflow <- MeanModelWorkflow$new(
    simulationSets = simSet,
    workflowFolder = testFolder
  ))

  expect_true(testFolder %in% list.files())
  expect_true("log-info.txt" %in% list.files(testFolder))
  expect_false("log-debug.txt" %in% list.files(testFolder))
  expect_false("log-error.txt" %in% list.files(testFolder))

  # Make sure testFolder is not there
  unlink(testFolder, recursive = TRUE)

  # Dummy simulation set for the example
  expect_output(pWorkflow <- PopulationWorkflow$new(
    workflowType = PopulationWorkflowTypes$parallelComparison,
    simulationSets = popSimSet,
    workflowFolder = testFolder
  ))

  expect_true(testFolder %in% list.files())
  expect_true("log-info.txt" %in% list.files(testFolder))
  expect_false("log-debug.txt" %in% list.files(testFolder))
  expect_false("log-error.txt" %in% list.files(testFolder))

  resetLogs()
  unlink(testFolder, recursive = TRUE)
})

test_that("MeanModelWorkflow has finalize method for lifecycle management", {
  testFolder <- "testFolder"
  unlink(testFolder, recursive = TRUE)
  
  simSet <- SimulationSet$new(
    simulationSetName = "myTest",
    simulationFile = getTestDataFilePath("input-data/MiniModel2.pkml")
  )
  
  # Create workflow - this sets log folder to workflowFolder
  expect_output(mWorkflow <- MeanModelWorkflow$new(
    simulationSets = simSet,
    workflowFolder = testFolder
  ))
  
  # Verify that the finalize method exists
  expect_true("finalize" %in% names(mWorkflow))
  
  # Set log folder to the workflow folder to simulate active workflow state
  setLogFolder(testFolder)
  expect_equal(reEnv$log$folder, testFolder)
  
  # Manually call finalize to test its behavior
  # The finalize method should reset the log folder to NULL
  mWorkflow$finalize()
  
  # Verify that the log folder has been reset to NULL
  expect_null(reEnv$log$folder)
  
  # Calling finalize again should be safe (idempotent)
  expect_silent(mWorkflow$finalize())
  expect_null(reEnv$log$folder)
  
  # Cleanup
  rm(mWorkflow)
  gc() # Force garbage collection to trigger finalize automatically
  
  resetLogs()
  unlink(testFolder, recursive = TRUE)
})
