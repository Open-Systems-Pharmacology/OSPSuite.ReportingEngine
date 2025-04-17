# GOF MetaData are lists the fields defined below
testMetaDataA <- list(
  Time = list(dimension = "Time", unit = "h"),
  Concentration = list(dimension = "Concentration (mass)", unit = "mg/L"),
  Path = "Test|Path|A",
  legend = "Test Legend A",
  residualsLegend = "Test Residuals Legend A",
  residualScale = ResidualScales$Logarithmic,
  group = "Group 1",
  color = "red",
  fill = "red"
)
testMetaDataB <- list(
  Time = list(dimension = "Time", unit = "h"),
  Concentration = list(dimension = "Concentration (mass)", unit = "mg/L"),
  Path = "Test|Path|B",
  legend = "Test Legend B",
  residualsLegend = "Test Residuals Legend B",
  residualScale = ResidualScales$Logarithmic,
  group = "Group 1",
  color = "blue",
  fill = "blue"
)
# Group C: different residuals scale
testMetaDataC <- list(
  Time = list(dimension = "Time", unit = "h"),
  Concentration = list(dimension = "Concentration (mass)", unit = "mg/L"),
  Path = "Test|Path|C",
  legend = "Test Legend C",
  residualsLegend = "Test Residuals Legend C",
  residualScale = ResidualScales$Linear,
  group = "Group 1",
  color = "green",
  fill = "green"
)
# Group D: different dimension/unit
testMetaDataD <- list(
  Time = list(dimension = "Time", unit = "h"),
  Concentration = list(dimension = "Fraction", unit = ""),
  Path = "Test|Path|D",
  legend = "Test Legend D",
  residualsLegend = "Test Residuals Legend D",
  residualScale = ResidualScales$Logarithmic,
  group = "Group 1",
  color = "yellow",
  fill = "yellow"
)
# Group D: different dimension/unit, scale and group
testMetaDataE <- list(
  Time = list(dimension = "Time", unit = "h"),
  Concentration = list(dimension = "Fraction", unit = ""),
  Path = "Test|Path|E",
  legend = "Test Legend E",
  residualsLegend = "Test Residuals Legend E",
  residualScale = ResidualScales$Logarithmic,
  group = "Group 2",
  color = "green",
  fill = "green"
)

# test getMetaDataFrame(listOfMetaData)
# returns a data frame with correct dimensions

# test getOutputGroups
# as many lists as number of groups
# check groups have consistent residual scales and consistent dimensions/units
correctMetaDataFrame1 <- ospsuite.reportingengine:::getMetaDataFrame(list(testMetaDataA, testMetaDataB))
correctMetaDataFrame2 <- ospsuite.reportingengine:::getMetaDataFrame(list(testMetaDataA, testMetaDataB, testMetaDataE))
scaleMetaDataFrame <- ospsuite.reportingengine:::getMetaDataFrame(list(testMetaDataA, testMetaDataC))
unitMetaDataFrame <- ospsuite.reportingengine:::getMetaDataFrame(list(testMetaDataA, testMetaDataD))

test_that("getMetaDataFrame returns a data.frame with appropriate dimensions for Legend and Labels", {
  expect_equal(correctMetaDataFrame1$dimension, c("Concentration", "Concentration"))
  expect_equal(correctMetaDataFrame2$dimension, c("Concentration", "Concentration", "Fraction"))
  expect_equal(scaleMetaDataFrame$dimension, c("Concentration", "Concentration"))
  expect_equal(unitMetaDataFrame$dimension, c("Concentration", "Fraction"))
})

test_that("getOutputGroups returns as many lists as groups", {
  expect_length(ospsuite.reportingengine:::getOutputGroups(correctMetaDataFrame1), 1)
  expect_length(ospsuite.reportingengine:::getOutputGroups(correctMetaDataFrame2), 2)
})

test_that("getOutputGroups warns appropriately about meta data consistency within groups", {
  expect_no_warning(ospsuite.reportingengine:::getOutputGroups(correctMetaDataFrame1))
  expect_no_warning(ospsuite.reportingengine:::getOutputGroups(correctMetaDataFrame2))
  expect_warning(ospsuite.reportingengine:::getOutputGroups(unitMetaDataFrame))
  expect_warning(ospsuite.reportingengine:::getOutputGroups(scaleMetaDataFrame))
})
