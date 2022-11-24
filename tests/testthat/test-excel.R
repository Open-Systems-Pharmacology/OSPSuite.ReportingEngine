# since the previous test changed the default settings
# reloading the package before this test aims at switching back to default settings
library(ospsuite.reportingengine)

context("Conversion of Excel Template")

# Input files for comparisons
templateBadIds <- getTestDataFilePath("excel/template-bad-ids.xlsx")
templateNoDataSource <- getTestDataFilePath("excel/template-no-source.xlsx")
templateDataSource <- getTestDataFilePath("excel/template-source.xlsx")
templateNoOutput <- getTestDataFilePath("excel/template-no-output.xlsx")
templateOutput <- getTestDataFilePath("excel/template-output.xlsx")
templateBadOutput <- getTestDataFilePath("excel/template-bad-output.xlsx")

test_that("Excel template is not converted when 'Code Identifier' or 'Description' is missing and log the error", {
  expect_error(createWorkflowFromExcelInput(
    excelFile = templateBadIds,
    workflowFile = "template-bad-ids.R"
  ))
  expect_false(file.exists("template-bad-ids.R"))
})

test_that("Excel template defines DataSource objects appropriately", {
  createWorkflowFromExcelInput(
    excelFile = templateNoDataSource,
    workflowFile = "template-no-source.R"
  )
  createWorkflowFromExcelInput(
    excelFile = templateDataSource,
    workflowFile = "template-source.R"
  )
  
  # Check that DataSource objects are not defined in both simulation sets
  expect_true(file.exists("template-no-source.R"))
  noSourceContent <- readLines("template-no-source.R")
  expect_equal(sum(grepl(pattern = "dataSource = NULL", noSourceContent)), 2)
  expect_equal(sum(grepl(pattern = "DataSource\\$new", noSourceContent)), 0)
  
  # Check that DataSource objects are defined in both simulation sets
  expect_true(file.exists("template-source.R"))
  # metaData included using DictionaryType = SHEET and DictionaryLocation = tpDictionary
  expect_true(file.exists("tpDictionary.csv"))
  sourceContent <- readLines("template-source.R")
  expect_equal(sum(grepl(pattern = "dataSource = Source1", sourceContent)), 2)
  expect_equal(sum(grepl(pattern = "DataSource\\$new", sourceContent)), 1)
  
  unlink("template-no-source.R", recursive = TRUE)
  unlink("tpDictionary.csv", recursive = TRUE)
  unlink("template-source.R", recursive = TRUE)
})


test_that("Excel template defines Output objects appropriately", {
  createWorkflowFromExcelInput(
    excelFile = templateNoOutput,
    workflowFile = "template-no-output.R"
  )
  createWorkflowFromExcelInput(
    excelFile = templateOutput,
    workflowFile = "template-output.R"
  )
  
  # Check that Output objects are not defined in both simulation sets
  expect_true(file.exists("template-no-output.R"))
  noOutputContent <- readLines("template-no-output.R")
  expect_equal(sum(grepl(pattern = "outputs = NULL", noOutputContent)), 2)
  expect_equal(sum(grepl(pattern = "Output\\$new", noOutputContent)), 0)
  
  # Check that Output objects are defined in both simulation sets
  expect_true(file.exists("template-output.R"))
  outputContent <- readLines("template-output.R")
  expect_equal(sum(grepl(pattern = "outputs = c\\(Output1, Output2\\)", outputContent)), 1)
  expect_equal(sum(grepl(pattern = "outputs = c\\(Output2\\)", outputContent)), 1)
  expect_equal(sum(grepl(pattern = "Output\\$new", outputContent)), 2)
  
  unlink("template-no-output.R", recursive = TRUE)
  unlink("template-output.R", recursive = TRUE)
})

test_that("Undefined outputs in Simulation Sets are flagged", {
expect_output(
  createWorkflowFromExcelInput(
    excelFile = templateBadOutput,
    workflowFile = "template-bad-output.R"
  ),
  "The following objects were not defined in the 'Outputs' sheet: 'OutputA'"
)
unlink("template-bad-output.R", recursive = TRUE)
})

