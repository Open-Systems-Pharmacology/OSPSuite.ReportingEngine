# Test data frame used as reference
testDataFrame <- data.frame(
  "ID" = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  "Time" = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  "DV" = c(1, 1, 2, 2, 1, 1, 3, 3, 1, 1, 4, 4),
  "Group" = c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B")
)
testCsvFile <- "testFile.csv"
testTxtFile <- "testFile.txt"

# Regular csv file of data.frame
write.csv(testDataFrame,
  file = testCsvFile,
  row.names = FALSE
)
# Regular txt file of data.frame
write.table(testDataFrame,
  file = testTxtFile,
  row.names = FALSE
)

context("Reading of Observed Data")

test_that("readObservedDataFile can correctly read csv and txt format for observed data", {
  csvData <- readObservedDataFile(testCsvFile)
  txtData <- readObservedDataFile(testTxtFile)
  expect_equal(testDataFrame, csvData)
  expect_equal(testDataFrame, txtData)
})

test_that("readObservedDataFile: unexistant file throw error", {
  expect_condition(readObservedDataFile("testFile10.csv"))
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

# Remove the files created during the tests
unlink(testCsvFile, recursive = TRUE)
unlink(testTxtFile, recursive = TRUE)
