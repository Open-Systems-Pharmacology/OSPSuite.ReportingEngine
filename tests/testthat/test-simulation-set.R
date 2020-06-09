context("SimulationSet class")

testSimulationFile <- getTestDataFilePath("MiniModel2.pkml")
testPopulationFile <- getTestDataFilePath("Pop500_p1p2p3.csv")
testObservedDataFile <- getTestDataFilePath("SimpleData.nmdat")
testObservedMetaDataFile <- getTestDataFilePath("tpDictionary.csv")

testOutput <- Output$new("Organism|A|Concentration", displayName = "A", displayUnit = "µg/ml")
testOutputWrongPath <- Output$new("Organism|C|Concentration", displayName = "C", displayUnit = "µg/ml")
testOutputWrongUnit <- Output$new("Organism|A|Concentration", displayName = "A", displayUnit = "%")


test_that("SimulationSet requires a pkml file and a set name", {
  expect_error(SimulationSet$new())
  expect_error(SimulationSet$new(simulationSetName = "Test Simulation"))
  expect_error(SimulationSet$new(simulationFile = testSimulationFile))
  expect_error(SimulationSet$new(simulationSetName = "Test Simulation", simulationFile = testPopulationFile))
  expect_silent(SimulationSet$new(simulationSetName = "Test Simulation", simulationFile = testSimulationFile))
})

test_that("PopulationSimulationSet requires a pkml file, a population file and a set name", {
  expect_error(PopulationSimulationSet$new())
  expect_error(PopulationSimulationSet$new(simulationSetName = "Test Simulation"))
  expect_error(PopulationSimulationSet$new(simulationFile = testSimulationFile))
  expect_error(PopulationSimulationSet$new(populationFile = testPopulationFile))
  expect_error(PopulationSimulationSet$new(simulationSetName = "Test Simulation", simulationFile = testSimulationFile))
  expect_error(PopulationSimulationSet$new(simulationSetName = "Test Simulation", populationFile = testPopulationFile))
  expect_error(PopulationSimulationSet$new(simulationFile = testSimulationFile, populationFile = testPopulationFile))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testPopulationFile,
    populationFile = testPopulationFile
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testSimulationFile
  ))
  expect_silent(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile
  ))
})

test_that("SimulationSet tests outputs using simulation from pkml file", {
  expect_silent(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    outputs = testOutput
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    outputs = testOutputWrongPath
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    outputs = testOutputWrongUnit
  ))
})

test_that("PopulationSimulationSet tests outputs using simulation from pkml file", {
  expect_silent(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    outputs = testOutput
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    outputs = testOutputWrongPath
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    outputs = testOutputWrongUnit
  ))
})

# Create wrong temporary dictionaries to test
observedMetaDataWrongHeader <- data.frame(
  matlabID = c("time", "dv"),
  nonmemColumn = c("TIME", "DV")
)
observedMetaDataWrongMapping <- data.frame(
  ID = c("time", "dv"),
  nonmemColumn = c("TIME", "DV2")
)
observedMetaDataWrongLLOQMapping <- data.frame(
  ID = c("time", "dv", "lloq"),
  nonmemColumn = c("TIME", "DV", "LOQ")
)
write.csv(observedMetaDataWrongHeader, file = "observedMetaDataWrongHeader.csv", row.names = FALSE)
write.csv(observedMetaDataWrongMapping, file = "observedMetaDataWrongMapping.csv", row.names = FALSE)
write.csv(observedMetaDataWrongLLOQMapping, file = "observedMetaDataWrongLLOQMapping.csv", row.names = FALSE)

test_that("SimulationSet tests observed data and their metadata", {
  expect_silent(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = testObservedMetaDataFile,
    outputs = testOutput
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    observedDataFile = testObservedDataFile,
    outputs = testOutput
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongHeader.csv",
    outputs = testOutput
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongMapping.csv",
    outputs = testOutput
  ))
  expect_error(SimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongLLOQMapping.csv",
    outputs = testOutput
  ))
})

test_that("SimulationSet tests observed data and their metadata", {
  expect_silent(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = testObservedMetaDataFile,
    outputs = testOutput
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    observedDataFile = testObservedDataFile,
    outputs = testOutput
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongHeader.csv",
    outputs = testOutput
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongMapping.csv",
    outputs = testOutput
  ))
  expect_error(PopulationSimulationSet$new(
    simulationSetName = "Test Simulation",
    simulationFile = testSimulationFile,
    populationFile = testPopulationFile,
    observedDataFile = testObservedDataFile,
    observedMetaDataFile = "observedMetaDataWrongLLOQMapping.csv",
    outputs = testOutput
  ))
})

unlink("observedMetaDataWrongHeader.csv", recursive = TRUE)
unlink("observedMetaDataWrongMapping.csv", recursive = TRUE)
unlink("observedMetaDataWrongLLOQMapping.csv", recursive = TRUE)

unlink("log-debug.txt", recursive = TRUE)
unlink("log-error.txt", recursive = TRUE)
unlink("log-info.txt", recursive = TRUE)
