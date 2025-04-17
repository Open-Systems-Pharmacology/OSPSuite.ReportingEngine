library(ospsuite.reportingengine)
# Test data frame used as reference
testDataFrame <- data.frame(
  "ID" = rep(seq(1, 3), each = 4),
  "Time" = rep(seq(1, 4), 3),
  "DV" = c(1, 1, 2, 2, 1, 1, 3, 3, 1, 1, 4, 4),
  "Group" = rep(c("A", "B"), each = 6),
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

# Needs to update expect_equal due to mismatch in attribute "row.names"
# due to new dplyr package method
expectDataframe <- function(x, y) {
  row.names(x) <- seq_len(nrow(x))
  row.names(y) <- seq_len(nrow(y))
  return(testthat::expect_equal(x, y))
}


context("Reading of Observed Data")

test_that("readObservedDataFile can correctly guess separator and read csv and txt format for observed data", {
  csvData <- readObservedDataFile(testCsvFile)
  csvSemiData <- readObservedDataFile(testCsvSemiFile)
  txtData <- readObservedDataFile(testTxtFile)
  expect_equal(testDataFrame, csvData, tolerance = comparisonTolerance())
  expect_equal(testDataFrame, csvSemiData, tolerance = comparisonTolerance())
  expect_equal(testDataFrame, txtData, tolerance = comparisonTolerance())
})

test_that("readObservedDataFile: throws an error for unexistant files", {
  expect_error(readObservedDataFile("testFile10.csv"))
})

test_that("readObservedDataFile throws an error for non UTF-8 files", {
  expect_error(
    readObservedDataFile(getTestDataFilePath("input-data/non-utf8.txt"))
  )
})

test_that("readObservedDataFile throw an error if columns are inconsistent", {
  expect_error(
    readObservedDataFile(getTestDataFilePath("input-data/error-data.csv"))
  )
})


context("Data selection process")

# Get unexported function
translateDataSelection <- ospsuite.reportingengine:::translateDataSelection
test_that("Empty 'dataSelection' is translated as FALSE", {
  expect_false(translateDataSelection(NULL))
  expect_false(translateDataSelection(as.character(NULL)))
  expect_false(translateDataSelection(""))
  expect_false(translateDataSelection(" "))
})
test_that("'translateDataSelection' remove white space appropriately", {
  expect_equal(
    translateDataSelection(c("aa", " ", " bb", "cc ", "")),
    "(aa) & (bb) & (cc)"
  )
  expect_false(translateDataSelection(c(" ", "")))

  expect_equal(
    translateDataSelection(c("a < 5 | b>2", " group %in% 'b' ")),
    "(a < 5 | b>2) & (group %in% 'b')"
  )
})
test_that("'translateDataSelection' understands logicals and DataSelectionKeys", {
  expect_false(translateDataSelection(FALSE))
  expect_false(translateDataSelection(DataSelectionKeys$NONE))

  expect_true(translateDataSelection(TRUE))
  expect_true(eval(parse(text = translateDataSelection(DataSelectionKeys$ALL))))
})




test_that("Selection Keys are well understood", {
  expect_equal(testDataFrame, getSelectedData(testDataFrame, DataSelectionKeys$ALL))
  expect_true(ospsuite.utils::isEmpty(getSelectedData(testDataFrame, DataSelectionKeys$NONE)))

  expect_true(getSelectedRows(testDataFrame, DataSelectionKeys$ALL))
  expect_false(getSelectedRows(testDataFrame, DataSelectionKeys$NONE))
})

test_that("'getSelectedData' and 'getSelectedRows' throw an error if variable name does not exist in data.frame", {
  expect_error(getSelectedData(testDataFrame, "wrongName"))
  expect_error(getSelectedData(testDataFrame, "wrongName"))
})

test_that("Correct expressions work as expected and both methods can be used to select data", {
  testSelection <- "ID == 1"
  selectedRows <- which(testDataFrame$ID == 1)
  expect_equal(selectedRows, getSelectedRows(testDataFrame, testSelection))
  expectDataframe(testDataFrame[selectedRows, ], getSelectedData(testDataFrame, testSelection))
  expectDataframe(
    testDataFrame[getSelectedRows(testDataFrame, testSelection), ],
    getSelectedData(testDataFrame, testSelection)
  )

  testSelection <- "Time %in% 1"
  selectedRows <- which(testDataFrame$Time %in% 1)
  expect_equal(selectedRows, getSelectedRows(testDataFrame, testSelection))
  expectDataframe(testDataFrame[selectedRows, ], getSelectedData(testDataFrame, testSelection))
  expectDataframe(
    testDataFrame[getSelectedRows(testDataFrame, testSelection), ],
    getSelectedData(testDataFrame, testSelection)
  )

  testSelection <- '!DV %in% 1 & Group %in% "A"'
  selectedRows <- which(!(testDataFrame$DV %in% 1) & testDataFrame$Group %in% "A")
  expect_equal(selectedRows, getSelectedRows(testDataFrame, testSelection))
  expectDataframe(testDataFrame[selectedRows, ], getSelectedData(testDataFrame, testSelection))
  expectDataframe(
    testDataFrame[getSelectedRows(testDataFrame, testSelection), ],
    getSelectedData(testDataFrame, testSelection)
  )
})

# get function 'getObservedDataFromOutput' which is not exported
getObservedDataFromOutput <- ospsuite.reportingengine:::getObservedDataFromOutput
test_that("'getObservedDataFromOutput' output empty data.frame when no data or data selection is provided", {
  outputAll <- Output$new(path = "a", dataSelection = DataSelectionKeys$ALL)
  outputNone <- Output$new(path = "a", dataSelection = DataSelectionKeys$NONE)
  outputNoSelect <- Output$new(path = "a", dataSelection = "Time > 5")
  # No data provided
  expect_null(getObservedDataFromOutput(outputAll, data = NULL, dataMapping = NULL, molWeight = NA, structureSet = NULL))
  expect_null(getObservedDataFromOutput(outputAll, data = data.frame(), dataMapping = NULL, molWeight = NA, structureSet = NULL))
  expect_null(getObservedDataFromOutput(outputNone, data = NULL, dataMapping = NULL, molWeight = NA, structureSet = NULL))
  expect_null(getObservedDataFromOutput(outputNone, data = data.frame(), dataMapping = NULL, molWeight = NA, structureSet = NULL))
  expect_null(getObservedDataFromOutput(outputNoSelect, data = NULL, dataMapping = NULL, molWeight = NA, structureSet = NULL))
  expect_null(getObservedDataFromOutput(outputNoSelect, data = data.frame(), dataMapping = NULL, molWeight = NA, structureSet = NULL))
  # No selected data or selection removing all the data
  expect_null(getObservedDataFromOutput(outputNone, data = testDataFrame, dataMapping = NULL, molWeight = NA, structureSet = NULL))
  # Since in this step, user actually defined a data selection
  # the number of selected rows is tracked in the log debug
  expect_null(getObservedDataFromOutput(outputNoSelect, data = testDataFrame, dataMapping = NULL, molWeight = NA, structureSet = NULL))
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
