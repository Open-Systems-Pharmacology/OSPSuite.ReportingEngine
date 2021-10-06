context("Configuration Plan")

workflowFolder <- "test-configuration-plan"
# Configuration Plan from Minimal Example
minimalFile <- getTestDataFilePath("configuration-plan/minimal.json")
simulationMappings <- read.csv(getTestDataFilePath("configuration-plan/simulation-mappings.csv"),
  stringsAsFactors = FALSE
)
sections <- read.csv(getTestDataFilePath("configuration-plan/sections.csv"),
  stringsAsFactors = FALSE
)
observedDataSets <- read.csv(getTestDataFilePath("configuration-plan/observed-datasets.csv"),
  stringsAsFactors = FALSE
)
suppressWarnings({
  minimalPlan <- loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = minimalFile
  )
})
referenceFolder <- minimalPlan$referenceFolder

test_that("A configuration plan requires correct fields", {
  expect_error(loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = getTestDataFilePath("configuration-plan/wrong-field.json")
  ))
})

test_that("SimulationMappings field works appropriately", {
  expect_equal(
    simulationMappings,
    minimalPlan$simulationMappings
  )

  expect_equal(
    file.path(referenceFolder, "Midazolam/S2/", "S2.pkml"),
    minimalPlan$getSimulationPath(project = "Midazolam", simulation = "S2")
  )
  expect_error(minimalPlan$getSimulationPath(project = "Midazolam", simulation = "S3"))
})

test_that("Sections field works appropriately", {
  expect_equal(
    sections,
    minimalPlan$sections
  )
  expect_equal(
    file.path("test-configuration-plan/002_section_2/003_section_3/", fsep = "/"),
    file.path(minimalPlan$getSectionPath(id = "3"), fsep = "/")
  )
  expect_equal(
    file.path("test-configuration-plan/002_section_2/003_section_3/", fsep = "/"),
    file.path(minimalPlan$getSectionPath(id = 3), fsep = "/")
  )
  expect_error(minimalPlan$getSectionPath(id = 10), fsep = "/")
  expect_equal(
    file.path("test-configuration-plan/003_Chapter 2_1.md", fsep = "/"),
    file.path(minimalPlan$getSectionMarkdown(id = 3), fsep = "/")
  )
})

test_that("observedDataSets field works appropriately", {
  expect_equal(
    observedDataSets,
    minimalPlan$observedDataSets
  )
  expect_equal(
    file.path(referenceFolder, "ObservedData", "Midazolam 600mg SD.csv"),
    minimalPlan$getObservedDataPath(id = "Midazolam 600mg SD")
  )
  expect_error(minimalPlan$getSectionPath(id = "wrong id"))
})

test_that("Duplicated Ids for Sections is correctly handled", {
  expect_error(loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = getTestDataFilePath("configuration-plan/duplicated-sections.json")
  ))
})

test_that("Duplicated Ids for SimulationMappings is correctly handled", {
  expect_error(loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = getTestDataFilePath("configuration-plan/duplicated-mappings.json")
  ))
})

test_that("Duplicated Ids for ObservedDataSets is correctly handled", {
  expect_warning(loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = minimalFile
  ))
  expect_error(loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = getTestDataFilePath("configuration-plan/duplicated-obs-data.json")
  ))
})
