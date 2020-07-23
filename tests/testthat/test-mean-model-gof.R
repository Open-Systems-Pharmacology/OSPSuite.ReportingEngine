context("Run mean model workflows with Goodness of Fit task")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dictFile <- getTestDataFilePath("input-data/tpDictionary.csv")

# Goodness of Fit without selected observed data
refOutputTimeProfileNoObs <- getTestDataFilePath("mean-goodness-of-fit-results/No-Obs-timeProfileData.csv")
refOutputTimeProfileAllObs <- getTestDataFilePath("mean-goodness-of-fit-results/All-Obs-timeProfileData.csv")
refOutputTimeProfileSelectObs <- getTestDataFilePath("mean-goodness-of-fit-results/Select-Obs-timeProfileData.csv")
refOutputResidualsAllObs <- getTestDataFilePath("mean-goodness-of-fit-results/All-Obs-residuals.csv")
refOutputResidualsSelectObs <- getTestDataFilePath("mean-goodness-of-fit-results/Select-Obs-residuals.csv")

refWorkflowStructure <- sort(c(
  "appendix-time-profile.md",
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "TimeProfiles"
))
timeProfileStructureNoObs <- sort(c(
  "A-timeProfile-Concentration (molar)-totalRange.png",
  "A-timeProfileData.csv",
  "A-timeProfileLog-Concentration (molar)-totalRange.png"
))
timeProfileStructureObs <- sort(c(
  timeProfileStructureNoObs,
  "A-obsVsPred-Concentration (molar)-totalRange.png",
  "A-obsVsPredLog-Concentration (molar)-totalRange.png",
  "A-resHisto-totalRange.png",
  "A-resQQPlot-totalRange.png",
  "A-resVsPred-Concentration (molar)-totalRange.png",
  "A-resVsTime-totalRange.png",
  "residuals-histogram.png",
  "residuals-qqplot.png",
  "residuals.csv"
))

workflowFolderNoObs1 <- "Results-No-Obs-1"
workflowFolderNoObs2 <- "Results-No-Obs-2"
workflowFolderNoObs3 <- "Results-No-Obs-3"

workflowFolderAllObs <- "Results-All-Obs"
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

workflowNoObs1 <- MeanModelWorkflow$new(simulationSets = setNoObs1, workflowFolder = workflowFolderNoObs1)
workflowNoObs2 <- MeanModelWorkflow$new(simulationSets = setNoObs2, workflowFolder = workflowFolderNoObs2)
workflowNoObs3 <- MeanModelWorkflow$new(simulationSets = setNoObs3, workflowFolder = workflowFolderNoObs3)
workflowAllObs <- MeanModelWorkflow$new(simulationSets = setAllObs, workflowFolder = workflowFolderAllObs)
workflowSelectObs <- MeanModelWorkflow$new(simulationSets = setSelectObs, workflowFolder = workflowFolderSelectObs)

workflowNoObs1$activateTasks(c("simulate", "plotGoF"))
workflowNoObs2$activateTasks(c("simulate", "plotGoF"))
workflowNoObs3$activateTasks(c("simulate", "plotGoF"))
workflowAllObs$activateTasks(c("simulate", "plotGoF"))
workflowSelectObs$activateTasks(c("simulate", "plotGoF"))

workflowNoObs1$runWorkflow()
workflowNoObs2$runWorkflow()
workflowNoObs3$runWorkflow()
workflowAllObs$runWorkflow()
workflowSelectObs$runWorkflow()

test_that("Workflow structure includes appropriate files and folders", {
  expect_equal(list.files(workflowNoObs1$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowNoObs2$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowNoObs3$workflowFolder), refWorkflowStructure)
  # One value from obs is 0, log(0)=-Inf is removed with a warning written in log-error.txt
  expect_equal(list.files(workflowAllObs$workflowFolder), sort(c("log-error.txt", refWorkflowStructure)))
  expect_equal(list.files(workflowSelectObs$workflowFolder), refWorkflowStructure)
})

test_that("Time profile directory includes correct files and folders", {
  expect_equal(list.files(file.path(workflowNoObs1$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_equal(list.files(file.path(workflowNoObs2$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_equal(list.files(file.path(workflowNoObs3$workflowFolder, "TimeProfiles")), timeProfileStructureNoObs)
  expect_equal(list.files(file.path(workflowAllObs$workflowFolder, "TimeProfiles")), timeProfileStructureObs)
  expect_equal(list.files(file.path(workflowSelectObs$workflowFolder, "TimeProfiles")), timeProfileStructureObs)
})

test_that("Saved time profile data and residuals includes the correct data", {
  expect_equal(readObservedDataFile(file.path(workflowNoObs1$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))
  expect_equal(readObservedDataFile(file.path(workflowNoObs2$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))
  expect_equal(readObservedDataFile(file.path(workflowNoObs3$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))

  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), readObservedDataFile(refOutputTimeProfileAllObs))
  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), readObservedDataFile(refOutputTimeProfileSelectObs))
  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "residuals.csv")), readObservedDataFile(refOutputResidualsAllObs))
  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "residuals.csv")), readObservedDataFile(refOutputResidualsSelectObs))
})

# Clear test workflow folders
unlink(workflowNoObs1$workflowFolder, recursive = TRUE)
unlink(workflowNoObs2$workflowFolder, recursive = TRUE)
unlink(workflowNoObs3$workflowFolder, recursive = TRUE)
unlink(workflowAllObs$workflowFolder, recursive = TRUE)
unlink(workflowSelectObs$workflowFolder, recursive = TRUE)
