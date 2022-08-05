context("Run mean model workflows with Time Profiles and Residuals task")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dictFile <- getTestDataFilePath("input-data/tpDictionary.csv")

# Goodness of Fit without selected observed data
refOutputTimeProfileNoObs <- getTestDataFilePath("mean-gof/No-Obs-timeProfileData.csv")
refOutputTimeProfileAllObs <- getTestDataFilePath("mean-gof/All-Obs-timeProfileData.csv")
refOutputTimeProfileSelectObs <- getTestDataFilePath("mean-gof/Select-Obs-timeProfileData.csv")
refOutputResidualsAllObs <- getTestDataFilePath("mean-gof/All-Obs-residuals.csv")
refOutputResidualsSelectObs <- getTestDataFilePath("mean-gof/Select-Obs-residuals.csv")

refWorkflowStructure <- c(
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "TimeProfiles"
)
timeProfileStructureNoObs <- c(
  "A-timeProfile-Concentration-total.png",
  "A-timeProfileData.csv",
  "A-timeProfileLog-Concentration-total.png"
)
timeProfileStructureObs <- c(
  timeProfileStructureNoObs,
  "A-obsVsPred-Concentration-total.png",
  "A-obsVsPredLog-Concentration-total.png",
  "A-resHisto-total.png",
  "A-resQQPlot-total.png",
  "A-resVsPred-Concentration-total.png",
  "A-resVsTime-total.png",
  "residuals-histogram.png",
  "residuals-qqplot.png",
  "residuals.csv"
)

# No observed data file, no selected data in output
workflowFolderNoObs1 <- "Results-No-Obs-1"
# Observed data file, default no selected data in output
workflowFolderNoObs2 <- "Results-No-Obs-2"
# Observed data file, no selected data in output
workflowFolderNoObs3 <- "Results-No-Obs-3"
# No observed data file, all selected data in output
workflowFolderNoObs4 <- "Results-No-Obs-4"
# Observed data file, all selected data in output
workflowFolderAllObs <- "Results-All-Obs"
# Observed data file, selected data in output
workflowFolderSelectObs <- "Results-Select-Obs"

setNoObs1 <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A"
  )
)
setNoObs2 <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A"
  )
)
setNoObs3 <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$NONE
  )
)
setNoObs4 <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = NULL,
  observedMetaDataFile = NULL,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)
setAllObs <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)
setSelectObs <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = "TAD > 4"
  )
)

# Use expression to define and run each workflow scenario
scenarios <- c("NoObs1", "NoObs2", "NoObs3", "NoObs4", "AllObs", "SelectObs")
defineAndRunWorkflows <- parse(text = paste0(
  "workflow", scenarios, " <- MeanModelWorkflow$new(",
  "simulationSets = set", scenarios, ", workflowFolder = workflowFolder", scenarios, ");",
  "workflow", scenarios, "$activateTasks(c('simulate', 'plotTimeProfilesAndResiduals'));",
  "workflow", scenarios, "$runWorkflow()"
))
eval(defineAndRunWorkflows)

test_that("Workflow structure includes appropriate files and folders", {
  expect_setequal(list.files(workflowNoObs1$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowNoObs2$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowNoObs3$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowNoObs4$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowAllObs$workflowFolder), refWorkflowStructure)
  expect_setequal(list.files(workflowSelectObs$workflowFolder), refWorkflowStructure)
})

test_that("Time profile directory includes correct files and folders", {
  expect_setequal(list.files(file.path(workflowNoObs1$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_setequal(list.files(file.path(workflowNoObs2$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_setequal(list.files(file.path(workflowNoObs3$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_setequal(list.files(file.path(workflowNoObs4$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_setequal(list.files(file.path(workflowAllObs$workflowFolder, "TimeProfiles")), timeProfileStructureObs)
  expect_setequal(list.files(file.path(workflowSelectObs$workflowFolder, "TimeProfiles")), timeProfileStructureObs)
})

test_that("Saved time profile data and residuals includes the correct data", {
  expect_equal(readObservedDataFile(file.path(workflowNoObs1$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileNoObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowNoObs2$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileNoObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowNoObs3$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileNoObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowNoObs4$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileNoObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileAllObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")),
    readObservedDataFile(refOutputTimeProfileSelectObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "residuals.csv")),
    readObservedDataFile(refOutputResidualsAllObs),
    tolerance = comparisonTolerance()
  )

  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "residuals.csv")),
    readObservedDataFile(refOutputResidualsSelectObs),
    tolerance = comparisonTolerance()
  )
})

# Clear test workflow folders
unlink(workflowNoObs1$workflowFolder, recursive = TRUE)
unlink(workflowNoObs2$workflowFolder, recursive = TRUE)
unlink(workflowNoObs3$workflowFolder, recursive = TRUE)
unlink(workflowNoObs4$workflowFolder, recursive = TRUE)
unlink(workflowAllObs$workflowFolder, recursive = TRUE)
unlink(workflowSelectObs$workflowFolder, recursive = TRUE)
