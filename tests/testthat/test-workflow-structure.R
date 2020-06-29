context("Initialize workflows")

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

test_that("Workflows initialization creates correct folder and logs, and warn user if the folder does exist", {
  # Make sure testFolder is not there
  testFolder <- "testFolder"
  unlink(testFolder, recursive = TRUE)

  simSet <- SimulationSet$new(
    simulationSetName = "myTest",
    simulationFile = getSimulationFilePath("test")
  )
  popSimSet <- PopulationSimulationSet$new(
    simulationSetName = "myTest",
    simulationFile = getSimulationFilePath("test"),
    populationFile = "test.csv"
  )

  # Dummy simulation set for the example
  expect_output(mWorkflow <- MeanModelWorkflow$new(
    simulationSets = simSet,
    workflowFolder = testFolder
  ))

  expect_true(testFolder %in% list.files())
  expect_true("log-info.txt" %in% list.files(testFolder))
  expect_true("log-debug.txt" %in% list.files(testFolder))

  # Make sure testFolder is not there
  unlink(testFolder, recursive = TRUE)

  # Dummy simulation set for the example
  expect_output(pWorkflow <- PopulationWorkflow$new(
    simulationSets = popSimSet,
    workflowFolder = testFolder
  ))

  expect_true(testFolder %in% list.files())
  expect_true("log-info.txt" %in% list.files(testFolder))
  expect_true("log-debug.txt" %in% list.files(testFolder))

  unlink(testFolder, recursive = TRUE)
})

unlink("log-debug.txt", recursive = TRUE)
unlink("log-info.txt", recursive = TRUE)
