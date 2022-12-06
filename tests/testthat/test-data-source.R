test_that("DataSource objects require both 'dataFile' and its 'metaDataFile'", {
  expect_error(DataSource$new())
  expect_error(DataSource$new(dataFile = "data.csv"))
  expect_error(DataSource$new(metaDataFile = "meta-data.csv"))
})

test_that("DataSource objects use caption when defined", {
  dataSource <- DataSource$new(
    dataFile = "data.csv", 
    metaDataFile = "meta-data.csv",
    caption = "my data source caption"
  )
  
  expect_equal(dataSource$getCaption(), "Data source: my data source caption")
  
  dataSource <- DataSource$new(
    dataFile = "data.csv", 
    metaDataFile = "meta-data.csv"
  )
  dataSource$caption <- "my second data source caption"
  expect_equal(dataSource$getCaption(), "Data source: my second data source caption")
})

test_that("DataSource objects provide clean path using workflow path when caption is not defined", {
  dataSource <- DataSource$new(
    dataFile = "data.csv", 
    metaDataFile = "meta-data.csv"
  )
  
  expect_equal(dataSource$getCaption(), "Data source: data.csv")
  
  dataSource <- DataSource$new(
    dataFile = "a/b/c/data.csv", 
    metaDataFile = "meta-data.csv"
  )
  
  expect_equal(dataSource$getCaption(), "Data source: a/b/c/data.csv")
  expect_equal(dataSource$getCaption("a"), "Data source: b/c/data.csv")
  expect_equal(dataSource$getCaption("a/b"), "Data source: c/data.csv")
  expect_equal(dataSource$getCaption("a/b/workflow"), "Data source: c/data.csv")
})

# Tests for definitions of units
dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dictFileUnit <- getTestDataFilePath("input-data/tpDictionary.csv")
dictFileNoUnit <- getTestDataFilePath("input-data/tpDictionary-ill-defined-unit.csv")

dataSourceUnit <- DataSource$new(dataFile = dataFile, metaDataFile = dictFileUnit)
dataSourceNoUnit <- DataSource$new(dataFile = dataFile, metaDataFile = dictFileNoUnit)

outputUnit <- Output$new(
  path = "Organism|A|Concentration in container",
  dataSelection = DataSelectionKeys$ALL,
  dataUnit = "µmol/l"
)
outputNoUnit <- Output$new(
  path = "Organism|A|Concentration in container",
  dataSelection = DataSelectionKeys$ALL
)
outputWrongUnit <- Output$new(
  path = "Organism|A|Concentration in container",
  dataSelection = DataSelectionKeys$ALL,
  dataUnit = "mg/l"
)

validateDataSource <- ospsuite.reportingengine:::validateDataSource
test_that("Well defined units work", {
  # No output -> no need to check
  expect_null(validateDataSource(dataSourceUnit, NULL, nullAllowed = TRUE))
  expect_null(validateDataSource(dataSourceNoUnit, NULL, nullAllowed = TRUE))
  # Output without unit but data Source with unit
  expect_null(validateDataSource(dataSourceUnit, c(outputNoUnit), nullAllowed = TRUE))
  # Output with unit but data Source without unit
  expect_null(validateDataSource(dataSourceNoUnit, c(outputUnit), nullAllowed = TRUE))
})

test_that("Errors are thrown for inconsistent/ill defined units", {
  # No unit defined anywhere
  expect_error(validateDataSource(dataSourceNoUnit, c(outputNoUnit), nullAllowed = TRUE))
  # No unit defined in data source -> all outputs require a unit
  expect_error(validateDataSource(dataSourceNoUnit, c(outputUnit, outputNoUnit), nullAllowed = TRUE))
  # Inconsistent units between data source and output
  expect_error(validateDataSource(dataSourceUnit, c(outputWrongUnit), nullAllowed = TRUE))
})
