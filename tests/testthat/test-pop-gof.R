context("Run population workflows with Time Profiles and Residuals task")

simulationFile <- getTestDataFilePath("input-data/MiniModel2.pkml")
populationFile <- getTestDataFilePath("input-data/Pop500_p1p2p3.csv")
dataFile <- getTestDataFilePath("input-data/SimpleData.nmdat")
dictFile <- getTestDataFilePath("input-data/tpDictionary.csv")

# Goodness of Fit without selected observed data
refOutputTimeProfileNoObs <- getTestDataFilePath("pop-goodness-of-fit-results/No-Obs-simulatedData.csv")
refOutputTimeProfileAllObs <- getTestDataFilePath("pop-goodness-of-fit-results/All-Obs-simulatedData.csv")
refOutputTimeProfileSelectObs <- getTestDataFilePath("pop-goodness-of-fit-results/Select-Obs-simulatedData.csv")
refOutputResidualsAllObs <- getTestDataFilePath("pop-goodness-of-fit-results/All-Obs-residuals.csv")
refOutputResidualsSelectObs <- getTestDataFilePath("pop-goodness-of-fit-results/Select-Obs-residuals.csv")

refWorkflowStructure <- sort(c(
  "appendix-time-profile.md",
  "log-debug.txt", "log-info.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "TimeProfiles"
))
timeProfileStructureNoObs <- sort(c(
  "A-timeProfile-Concentration (molar)-totalRange.png",
  "A-simulatedData.csv",
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
  "A-observedData.csv",
  "residuals-histogram.png",
  "residuals-qqplot.png",
  "residuals.csv"
))

workflowFolderNoObs1 <- "Results-No-Obs-1"
workflowFolderNoObs2 <- "Results-No-Obs-2"
workflowFolderNoObs3 <- "Results-No-Obs-3"

workflowFolderAllObs <- "Results-All-Obs"
workflowFolderSelectObs <- "Results-Select-Obs"

setNoObs1 <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A"
  )
)
setNoObs2 <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A"
  )
)
setNoObs3 <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$NONE
  )
)
setAllObs <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)
setSelectObs <- PopulationSimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  populationFile = populationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFile,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = "TAD > 4"
  )
)

workflowNoObs1 <- PopulationWorkflow$new(workflowType = PopulationWorkflowTypes$parallelComparison, simulationSets = setNoObs1, workflowFolder = workflowFolderNoObs1)
workflowNoObs2 <- PopulationWorkflow$new(workflowType = PopulationWorkflowTypes$parallelComparison, simulationSets = setNoObs2, workflowFolder = workflowFolderNoObs2)
workflowNoObs3 <- PopulationWorkflow$new(workflowType = PopulationWorkflowTypes$parallelComparison, simulationSets = setNoObs3, workflowFolder = workflowFolderNoObs3)
workflowAllObs <- PopulationWorkflow$new(workflowType = PopulationWorkflowTypes$parallelComparison, simulationSets = setAllObs, workflowFolder = workflowFolderAllObs)
workflowSelectObs <- PopulationWorkflow$new(workflowType = PopulationWorkflowTypes$parallelComparison, simulationSets = setSelectObs, workflowFolder = workflowFolderSelectObs)

workflowNoObs1$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowNoObs2$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowNoObs3$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowAllObs$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowSelectObs$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))

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
  expect_equal(readObservedDataFile(file.path(workflowNoObs1$workflowFolder, "TimeProfiles", "A-simulatedData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))
  expect_equal(readObservedDataFile(file.path(workflowNoObs2$workflowFolder, "TimeProfiles", "A-simulatedData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))
  expect_equal(readObservedDataFile(file.path(workflowNoObs3$workflowFolder, "TimeProfiles", "A-simulatedData.csv")), readObservedDataFile(refOutputTimeProfileNoObs))
  
  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "A-simulatedData.csv")), readObservedDataFile(refOutputTimeProfileAllObs))
  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "A-simulatedData.csv")), readObservedDataFile(refOutputTimeProfileSelectObs))
  expect_equal(readObservedDataFile(file.path(workflowAllObs$workflowFolder, "TimeProfiles", "residuals.csv")), readObservedDataFile(refOutputResidualsAllObs))
  expect_equal(readObservedDataFile(file.path(workflowSelectObs$workflowFolder, "TimeProfiles", "residuals.csv")), readObservedDataFile(refOutputResidualsSelectObs))
})

# Clear test workflow folders
unlink(workflowNoObs1$workflowFolder, recursive = TRUE)
unlink(workflowNoObs2$workflowFolder, recursive = TRUE)
unlink(workflowNoObs3$workflowFolder, recursive = TRUE)
unlink(workflowAllObs$workflowFolder, recursive = TRUE)
unlink(workflowSelectObs$workflowFolder, recursive = TRUE)
