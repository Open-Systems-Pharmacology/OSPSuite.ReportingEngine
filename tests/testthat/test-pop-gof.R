context("Run population workflows with Time Profiles and Residuals task")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
populationFile <- getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dictFile <- getTestDataFilePath("input-data/tpDictionary.csv")

# The tests use expressions centralizing the definitions,
# in order to clarify what each scenario is testing
# Scenarios are defined as
# - NoObs1: No observed data file, no selected data in output
# - NoObs2: Observed data file, default no selected data in output
# - NoObs3: Observed data file, no selected data in output
# - NoObs4: No observed data file, all selected data in output
# - AllObs: Observed data file, all selected data in output
# - SelectObs: Observed data file, selected data in output
scenarios <- c("NoObs1", "NoObs2", "NoObs3", "NoObs4", "AllObs", "SelectObs")

#----- Define expected outputs -----
defineExpectedTimeProfileData <- parse(text = paste0(
  "expectedTimeProfileData", scenarios, ' <- getTestDataFilePath("pop-gof/',
  c(rep("No-Obs", 4), "All-Obs", "Select-Obs"), '-simulatedData.csv")'
))
defineExpectedResidualsData <- parse(text = paste0(
  "expectedResidualsData", scenarios[5:6], ' <- getTestDataFilePath("pop-gof/',
  c("All-Obs", "Select-Obs"), '-residuals.csv.csv")'
))

defineExpectedFigures <- parse(text = paste0(
  "expectedFigures", scenarios, " <- ", c(rep(2, 4), 8, 8)
))

defineExpectedTables <- parse(text = paste0(
  "expectedTables", scenarios, " <- ", c(rep(1, 4), 2, 2)
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
  "set", scenarios, " <- PopulationSimulationSet$new(",
  'simulationSetName = "A",',
  "simulationFile = simulationFile,",
  "populationFile = populationFile,",
  # Scenarios for observed data file
  c(
    "",
    "observedDataFile = dataFile,",
    "observedDataFile = dataFile,",
    "observedDataFile = NULL,",
    "observedDataFile = dataFile,",
    "observedDataFile = dataFile,"
  ),
  # Scenarios for dictionary file
  c(
    "",
    "observedMetaDataFile = dictFile,",
    "observedMetaDataFile = dictFile,",
    "observedMetaDataFile = NULL,",
    "observedMetaDataFile = dictFile,",
    "observedMetaDataFile = dictFile,"
  ),
  "outputs = Output$new(",
  'path = "Organism|A|Concentration in container",',
  # Scenarios for data selection
  c(
    "",
    "",
    "dataSelection = DataSelectionKeys$NONE,",
    "dataSelection = DataSelectionKeys$ALL,",
    "dataSelection = DataSelectionKeys$ALL,",
    'dataSelection = "TAD > 4",'
  ),
  'displayName = "Concentration of A"))'
))

defineAndRunWorkflows <- parse(text = paste0(
  "workflow", scenarios, " <- PopulationWorkflow$new(",
  "workflowType = PopulationWorkflowTypes$parallelComparison,",
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
        list.files(timeProfileResultsFolder, pattern = "simulated", full.names = TRUE)
      )
    )
    testTimeProfileDataExpression <- parse(text = paste0(
      "expect_equal(actualTimeProfileData, ",
      "readObservedDataFile(expectedTimeProfileData", scenario, "),",
      "tolerance = comparisonTolerance())"
    ))
    eval(testTimeProfileDataExpression)
  }

  for (scenario in scenarios[5:6]) {
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
    eval(testTimeProfileDataExpression)
  }
})

#----- Cleaning of test folders -----
clearFolders <- parse(text = paste0(
  "unlink(workflow", scenarios, "$workflowFolder, recursive = TRUE)"
))
eval(clearFolders)
