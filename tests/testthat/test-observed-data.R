library(ospsuite.reportingengine)
# Test data frame used as reference
testDataFrame <- data.frame(
  "ID" = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  "Time" = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  "DV" = c(1, 1, 2, 2, 1, 1, 3, 3, 1, 1, 4, 4),
  "Group" = c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B"),
  stringsAsFactors = FALSE
)

testCsvFile <- "testFile.csv"
testCsvSemiFile <- "testFileSemi.csv"
testTxtFile <- "testFile.txt"

# Regular csv file of data.frame
write.csv(testDataFrame,
  file = testCsvFile,
  row.names = FALSE
)
# Semi-column csv file of data.frame
write.table(testDataFrame,
  file = testCsvSemiFile,
  sep = ";",
  row.names = FALSE
)
# Regular txt file of data.frame
write.table(testDataFrame,
  file = testTxtFile,
  row.names = FALSE
)

context("Reading of Observed Data")

test_that("readObservedDataFile can correctly guess separator and read csv and txt format for observed data", {
  csvData <- readObservedDataFile(testCsvFile)
  csvSemiData <- readObservedDataFile(testCsvSemiFile)
  txtData <- readObservedDataFile(testTxtFile)
  expect_equal(testDataFrame, csvData, tolerance = comparisonTolerance())
  expect_equal(testDataFrame, csvSemiData, tolerance = comparisonTolerance())
  expect_equal(testDataFrame, txtData, tolerance = comparisonTolerance())
})

test_that("readObservedDataFile: unexistant file throw error", {
  expect_error(readObservedDataFile("testFile10.csv"))
})

test_that("readObservedDataFile throw an error if columns are inconsistent", {
  expect_error(
    readObservedDataFile(getTestDataFilePath("input-data/error-data.csv"))
  )
})


context("Data selection process")

test_that("'evalDataFilter' gets data.frame variable as 'data' and is consequently independent of input data.frame", {
  testDataFrameA <- testDataFrame
  testDataFrameB <- testDataFrame
  filterExpression <- parse(text = "data")
  expect_equal(testDataFrame, evalDataFilter(testDataFrameA, filterExpression))
  expect_equal(testDataFrame, evalDataFilter(testDataFrameB, filterExpression))
})

test_that("'filterExpression' uses data.frame variable names as actual variable", {
  for (variableName in names(testDataFrame)) {
    filterExpression <- parse(text = variableName)
    filterVariable <- evalDataFilter(testDataFrame, filterExpression)
    expect_equal(testDataFrame[, variableName], filterVariable)
  }
})

test_that("'evalDataFilter' throw an error if variable name does not exist in data.frame", {
  filterExpression <- parse(text = "wrongName")
  expect_error(evalDataFilter(testDataFrame, filterExpression))
})

test_that("Correct expressions work the way they should", {
  filterExpression <- parse(text = "ID == 1")
  expect_equal(
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    evalDataFilter(testDataFrame, filterExpression)
  )
  filterExpression <- parse(text = "Time %in% 1")
  expect_equal(
    c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    evalDataFilter(testDataFrame, filterExpression)
  )
  filterExpression <- parse(text = '!DV %in% 1 & Group %in% "A"')
  expect_equal(
    c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    evalDataFilter(testDataFrame, filterExpression)
  )
})

# get function 'getObservedDataFromOutput' which is not exported
getObservedDataFromOutput <- ospsuite.reportingengine:::getObservedDataFromOutput
test_that("'getObservedDataFromOutput' output empty data.frame when no data or data selection is provided", {
  outputAll <- Output$new(path = "a", dataSelection = DataSelectionKeys$ALL)
  outputNone <- Output$new(path = "a", dataSelection = DataSelectionKeys$NONE)
  outputNoSelect <- Output$new(path = "a", dataSelection = "Time > 5")
  # No data provided
  expect_null(getObservedDataFromOutput(outputAll, data = NULL, dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  expect_null(getObservedDataFromOutput(outputAll, data = data.frame(), dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  expect_null(getObservedDataFromOutput(outputNone, data = NULL, dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  expect_null(getObservedDataFromOutput(outputNone, data = data.frame(), dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  expect_null(getObservedDataFromOutput(outputNoSelect, data = NULL, dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  expect_null(getObservedDataFromOutput(outputNoSelect, data = data.frame(), dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  # No selected data or selection removing all the data
  expect_null(getObservedDataFromOutput(outputNone, data = testDataFrame, dataMapping = NULL, molWeight = NA, timeUnit = NULL))
  # Since in this step, user actually defined a data selection
  # the number of selected rows is tracked in the log debug
  expect_null(getObservedDataFromOutput(outputNoSelect, data = testDataFrame, dataMapping = NULL, molWeight = NA, timeUnit = NULL))
})

# Remove the files created during the tests
unlink("log-debug.txt", recursive = TRUE)
unlink(testCsvFile, recursive = TRUE)
unlink(testCsvSemiFile, recursive = TRUE)
unlink(testTxtFile, recursive = TRUE)

context("extractNameAndUnit")

test_that("It can extract name and unit when no unit is present", {
  # Because the function is not exported, it needs to be called using ospsuite.reportingengine:::
  # Get separateVariableFromUnit into current namespace
  res <- ospsuite.reportingengine:::extractNameAndUnit("Value")
  expect_equal(res$name, "Value")
  expect_equal(res$unit, "")
})

test_that("It can extract name and unit when a unit is present", {
  res <- ospsuite.reportingengine:::extractNameAndUnit("Value [unit]")
  expect_equal(res$name, "Value")
  expect_equal(res$unit, "unit")
})

test_that("It can extract name and unit when there are multiple brackets in the name", {
  res <- ospsuite.reportingengine:::extractNameAndUnit("Value [raw] [unit]")
  expect_equal(res$name, "Value [raw]")
  expect_equal(res$unit, "unit")
})

test_that("It can extract name and unit when there are empty spaces before and after the unit", {
  res <- ospsuite.reportingengine:::extractNameAndUnit("Value [raw] [  unit]  ")
  expect_equal(res$name, "Value [raw]")
  expect_equal(res$unit, "unit")
})

test_that("It does not crash when provided with a string not formatted for the exctraction", {
  res <- ospsuite.reportingengine:::extractNameAndUnit("Value [raw] rest")
  expect_equal(res$name, "Value [raw] rest")
  expect_equal(res$unit, "")
})

test_that("It does not crash when with an empty string", {
  res <- ospsuite.reportingengine:::extractNameAndUnit("")
  expect_equal(res$name, "")
  expect_equal(res$unit, "")
})
