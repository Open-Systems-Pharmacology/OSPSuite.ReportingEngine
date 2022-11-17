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
