# Define inputs and format data for tests
# In this test, use a simulation with applications every 12h
simulationFile <- getTestDataFilePath("input-data/Raltegravir 200 mg filmcoated tablet md.pkml")
output <- Output$new(path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)")
simulation <- ospsuite::loadSimulation(simulationFile)
simResults <- ospsuite::runSimulations(simulations = simulation)
simResults <- ospsuite::simulationResultsToTibble(simResults[[1]])
simResults$Time <- ospsuite::toUnit("Time", simResults$Time, "h")

simSetWithoutOffset <- SimulationSet$new(
  simulationFile = simulationFile,
  simulationSetName = "without offset",
  outputs = output,
  timeOffset = 0
)
simSetWithOffset <- SimulationSet$new(
  simulationFile = simulationFile,
  simulationSetName = "with 6h offset",
  outputs = output,
  timeOffset = 6
)

# No Offset: 0h
timeRanges0 <- ospsuite.reportingengine:::getSimulationTimeRanges(
  simulation = simulation,
  path = output$path,
  simulationSet = simSetWithoutOffset
)
# Offset of 6h
timeRanges6 <- ospsuite.reportingengine:::getSimulationTimeRanges(
  simulation = simulation,
  path = output$path,
  simulationSet = simSetWithOffset
)

test_that("Appropriate time ranges are selected based on offset", {
  # Total time range
  expect_equal(timeRanges0$total$values, c(0, 240))
  # Total time range accounting for offset
  expect_equal(timeRanges6$total$values, c(6, 240))
  # First application
  expect_equal(timeRanges0$firstApplication$values, c(0, 12))
  # First application since offset
  expect_equal(timeRanges6$firstApplication$values, c(12, 24))
  # Last application
  expect_equal(timeRanges0$lastApplication$values, c(228, 240))
  # Last application since offset
  expect_equal(timeRanges6$lastApplication$values, c(228, 240))
})

test_that("Time after dose is calculated appropriately", {
  # Total time range without time offset should be equal to initial data
  dataTotal0 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges0$total$values),
    maxTime = max(timeRanges0$total$values)
  )
  expect_equal(simResults$Time, dataTotal0$Time)
  expect_equal(simResults$simulationValues, dataTotal0$simulationValues)

  # First application time range without time offset should be equal to data until 12h
  dataFirst0 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges0$firstApplication$values),
    maxTime = max(timeRanges0$firstApplication$values)
  )
  expect_equal(simResults$Time[simResults$Time <= 12], dataFirst0$Time)
  expect_equal(
    simResults$simulationValues[simResults$Time <= 12],
    dataFirst0$simulationValues
  )

  # Last application time range without time offset should be equal to data since 228h
  # with time as time after dose
  dataLast0 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges0$lastApplication$values),
    maxTime = max(timeRanges0$lastApplication$values)
  )
  expect_equal(simResults$Time[simResults$Time >= 228] - 228, dataLast0$Time)
  expect_equal(
    simResults$simulationValues[simResults$Time >= 228],
    dataLast0$simulationValues
  )

  # Total time range with time offset should be equal to initial data
  # Shifted by the 6h offset
  dataTotal6 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges6$total$values),
    maxTime = max(timeRanges6$total$values)
  )
  expect_equal(simResults$Time[simResults$Time >= 6] - 6, dataTotal6$Time)
  expect_equal(
    simResults$simulationValues[simResults$Time >= 6],
    dataTotal6$simulationValues
  )

  # First application since offset should be equal to initial data after offset
  dataFirst6 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges6$firstApplication$values),
    maxTime = max(timeRanges6$firstApplication$values)
  )
  expect_equal(
    simResults$Time[simResults$Time >= 12 & simResults$Time <= 24] - 12,
    dataFirst6$Time
  )
  expect_equal(
    simResults$simulationValues[simResults$Time >= 12 & simResults$Time <= 24],
    dataFirst6$simulationValues
  )

  # Last application time range without time offset should be equal to data since 228h
  # with time as time after dose
  dataLast6 <- ospsuite.reportingengine:::asTimeAfterDose(
    data = simResults,
    timeOffset = min(timeRanges6$lastApplication$values),
    maxTime = max(timeRanges6$lastApplication$values)
  )
  expect_equal(simResults$Time[simResults$Time >= 228] - 228, dataLast6$Time)
  expect_equal(
    simResults$simulationValues[simResults$Time >= 228],
    dataLast6$simulationValues
  )
})
