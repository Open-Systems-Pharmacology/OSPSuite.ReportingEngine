# since the previous test changed the default settings
# reloading the package before this test aims at switching back to default settings
library(ospsuite.reportingengine)

context("Observed Data in plot time profile task")

# Input files and structures for comparisons
simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")

dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dataFileUnit <- getTestDataFilePath("input-data/SimpleDataWithUnit.nmdat")
dataFileLLOQ <- getTestDataFilePath("input-data/SimpleDataWithLLOQ.nmdat")

dictFileLLOQ <- getTestDataFilePath("input-data/tpDictionary-lloq.csv")
dictFileUnitInObs <- getTestDataFilePath("input-data/tpDictionary-unit-in-obs.csv")
dictFileErrorUnit <- getTestDataFilePath("input-data/tpDictionary-ill-defined-unit.csv")
dictFileErrorVariable <- getTestDataFilePath("input-data/tpDictionary-ill-defined-variable.csv")

# The tests use expressions centralizing the definitions,
# in order to clarify what each scenario is testing
# Scenarios are defined as
# - ErrorUnit: Unit is not defined for DV
# - ErrorVariable: DV column does not exist in data file
# - UnitInObs: Unit is defined as a column in observed data file
# - LLOQ: Lower limit of quantification is defined observed data file and provided
# - MissingLLOQ: LLOQ column does not exist in data file
scenarios <- c("UnitInObs", "LLOQ", "MissingLLOQ")

#----- Define expected outputs -----
defineExpectedTimeProfileData <- parse(text = paste0(
  "expectedTimeProfileData", scenarios, ' <- getTestDataFilePath("mean-gof/',
  c("All-Obs", "lloq", "All-Obs"), '-timeProfileData.csv")'
))
defineExpectedResidualsData <- parse(text = paste0(
  "expectedResidualsData", scenarios, ' <- getTestDataFilePath("mean-gof/',
  c("All-Obs", "lloq", "All-Obs"), '-residuals.csv")'
))

defineExpectedFigures <- parse(text = paste0(
  "expectedFigures", scenarios, " <- 8"
))

defineExpectedTables <- parse(text = paste0(
  "expectedTables", scenarios, " <- 2"
))

expectedWorkflowStructure <- c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "TimeProfiles"
)

eval(defineExpectedTimeProfileData)
eval(defineExpectedResidualsData)
eval(defineExpectedFigures)
eval(defineExpectedTables)

#----- Define and run workflows for each scenario -----
defineWorkflowFolder <- parse(text = paste0(
  "workflowFolder", scenarios, ' <- "Results-', scenarios, '"'
))

defineSimulationSets <- parse(text = paste0(
  "set", scenarios, " <- SimulationSet$new(",
  'simulationSetName = "A",',
  "simulationFile = simulationFile,",
  # Scenarios for observed data file
  "dataSource = DataSource$new(",
  c(
    "dataFile = dataFileUnit,",
    "dataFile = dataFileLLOQ,",
    "dataFile = dataFile,"
  ),
  # Scenarios for dictionary file
  c(
    "metaDataFile = dictFileUnitInObs",
    "metaDataFile = dictFileLLOQ",
    "metaDataFile = dictFileLLOQ"
  ),
  "),",
  "outputs = Output$new(",
  'path = "Organism|A|Concentration in container",',
  "dataSelection = DataSelectionKeys$ALL,",
  'displayName = "Concentration of A"))'
))

defineAndRunWorkflows <- parse(text = paste0(
  "workflow", scenarios, " <- MeanModelWorkflow$new(",
  "simulationSets = set", scenarios, ", workflowFolder = workflowFolder", scenarios, ");",
  "workflow", scenarios, "$activateTasks(c('simulate', 'plotTimeProfilesAndResiduals'));",
  "workflow", scenarios, "$runWorkflow()"
))

eval(defineWorkflowFolder)
eval(defineSimulationSets)
eval(defineAndRunWorkflows)

#----- Tests -----

test_that("Workflow structure includes appropriate files and folders", {
  testFolderStructureExpression <- parse(text = paste0(
    "expect_setequal(list.files(workflow", scenarios, "$workflowFolder), expectedWorkflowStructure)"
  ))
  eval(testFolderStructureExpression)
})

test_that("Time profile directory includes correct number of figure and table files", {
  testFiguresExpression <- parse(text = paste0(
    "expect_equal(length(list.files(",
    "file.path(workflow", scenarios, '$workflowFolder, "TimeProfiles"),',
    'pattern = ".png")), ',
    "expectedFigures", scenarios, ")"
  ))

  testTablesExpression <- parse(text = paste0(
    "expect_equal(length(list.files(",
    "file.path(workflow", scenarios, '$workflowFolder, "TimeProfiles"),',
    'pattern = ".csv")), ',
    "expectedTables", scenarios, ")"
  ))

  eval(testFiguresExpression)
  eval(testTablesExpression)
})

test_that("Saved time profile data and residuals includes the correct data", {
  for (scenario in scenarios) {
    getTimeProfileResultsFolder <- parse(text = paste0(
      "timeProfileResultsFolder <- file.path(workflow", scenario, '$workflowFolder, "TimeProfiles")'
    ))
    eval(getTimeProfileResultsFolder)
    # Get file corresponding to time profile data
    actualTimeProfileData <- readObservedDataFile(
      intersect(
        list.files(timeProfileResultsFolder, pattern = "csv", full.names = TRUE),
        # Note: Change in nomenclature could change the pattern below
        list.files(timeProfileResultsFolder, pattern = "time_profile", full.names = TRUE)
      )
    )
    testTimeProfileDataExpression <- parse(text = paste0(
      "expect_equal(actualTimeProfileData, ",
      "readObservedDataFile(expectedTimeProfileData", scenario, "),",
      "tolerance = comparisonTolerance())"
    ))
    eval(testTimeProfileDataExpression)
  }

  for (scenario in scenarios) {
    getTimeProfileResultsFolder <- parse(text = paste0(
      "timeProfileResultsFolder <- file.path(workflow", scenario, '$workflowFolder, "TimeProfiles")'
    ))
    eval(getTimeProfileResultsFolder)
    # Get file corresponding to residuals data
    actualResidualsData <- readObservedDataFile(
      intersect(
        list.files(timeProfileResultsFolder, pattern = "csv", full.names = TRUE),
        list.files(timeProfileResultsFolder, pattern = "residuals", full.names = TRUE)
      )
    )
    testResidualsDataExpression <- parse(text = paste0(
      "expect_equal(actualResidualsData, ",
      "readObservedDataFile(expectedResidualsData", scenario, "),",
      "tolerance = comparisonTolerance())"
    ))
    eval(testResidualsDataExpression)
  }
})

test_that("Ill defined variables and units in dictionary are flagged by simulation sets", {
  expect_error(SimulationSet$new(
    simulationSetName = "A",
    simulationFile = simulationFile,
    dataSource = DataSource$new(
      dataFile = dataFile,
      metaDataFile = dictFileErrorUnit
    ),
    outputs = Output$new(
      path = "Organism|A|Concentration in container",
      displayName = "Concentration of A",
      dataSelection = DataSelectionKeys$ALL
    )
  ))

  expect_error(SimulationSet$new(
    simulationSetName = "A",
    simulationFile = simulationFile,
    dataSource = DataSource$new(
      dataFile = dataFile,
      metaDataFile = dictFileErrorVariable
    ),
    outputs = Output$new(
      path = "Organism|A|Concentration in container",
      displayName = "Concentration of A",
      dataSelection = DataSelectionKeys$ALL
    )
  ))
})

#----- Cleaning of test folders -----
clearFolders <- parse(text = paste0(
  "unlink(workflow", scenarios, "$workflowFolder, recursive = TRUE)"
))
eval(clearFolders)
