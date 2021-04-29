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

refOutputTimeProfile <- getTestDataFilePath("mean-gof/All-Obs-timeProfileData.csv")
refOutputTimeProfileLLOQ <- getTestDataFilePath("mean-gof/lloq-timeProfileData.csv")
refOutputResiduals <- getTestDataFilePath("mean-gof/All-Obs-residuals.csv")

refWorkflowStructure <- sort(c(
  "log-debug.txt", "log-info.txt", "log-error.txt",
  "Report-word.md", "Report.docx", "Report.md",
  "SimulationResults", "TimeProfiles"
))
timeProfileStructure <- sort(c(
  "A-timeProfile-Concentration-total.png",
  "A-timeProfileData.csv",
  "A-timeProfileLog-Concentration-total.png",
  "A-obsVsPred-Concentration-total.png",
  "A-obsVsPredLog-Concentration-total.png",
  "A-resHisto-total.png",
  "A-resQQPlot-total.png",
  "A-resVsPred-Concentration-total.png",
  "A-resVsTime-total.png",
  "residuals-histogram.png",
  "residuals-qqplot.png",
  "residuals.csv"
))

workflowFolderUnit <- "Results-ObsUnit"
workflowFolderLLOQ <- "Results-LLOQ"
workflowFolderMissingLLOQ <- "Results-MissingLLOQ"

test_that("Ill defined variables and units in dictionary are flagged by simulation sets", {
  expect_error(SimulationSet$new(
    simulationSetName = "A",
    simulationFile = simulationFile,
    observedDataFile = dataFile,
    observedMetaDataFile = dictFileErrorUnit,
    outputs = Output$new(
      path = "Organism|A|Concentration in container",
      displayName = "Concentration of A",
      dataSelection = DataSelectionKeys$ALL
    )))
  
  expect_error(SimulationSet$new(
    simulationSetName = "A",
    simulationFile = simulationFile,
    observedDataFile = dataFile,
    observedMetaDataFile = dictFileErrorVariable,
    outputs = Output$new(
      path = "Organism|A|Concentration in container",
      displayName = "Concentration of A",
      dataSelection = DataSelectionKeys$ALL
    )))
  
  expect_warning(SimulationSet$new(
    simulationSetName = "A",
    simulationFile = simulationFile,
    observedDataFile = dataFile,
    observedMetaDataFile = dictFileLLOQ,
    outputs = Output$new(
      path = "Organism|A|Concentration in container",
      displayName = "Concentration of A",
      dataSelection = DataSelectionKeys$ALL
    )))
})


setUnitInObs <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFileUnit,
  observedMetaDataFile = dictFileUnitInObs,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)

setLLOQ <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFileLLOQ,
  observedMetaDataFile = dictFileLLOQ,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)

setMissingLLOQ <- SimulationSet$new(
  simulationSetName = "A",
  simulationFile = simulationFile,
  observedDataFile = dataFile,
  observedMetaDataFile = dictFileLLOQ,
  outputs = Output$new(
    path = "Organism|A|Concentration in container",
    displayName = "Concentration of A",
    dataSelection = DataSelectionKeys$ALL
  )
)

workflowUnitInObs <- MeanModelWorkflow$new(simulationSets = setUnitInObs, workflowFolder = workflowFolderUnit)
workflowLLOQ <- MeanModelWorkflow$new(simulationSets = setLLOQ, workflowFolder = workflowFolderLLOQ)
workflowMissingLLOQ <- MeanModelWorkflow$new(simulationSets = setMissingLLOQ, workflowFolder = workflowFolderMissingLLOQ)

workflowUnitInObs$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowLLOQ$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))
workflowMissingLLOQ$activateTasks(c("simulate", "plotTimeProfilesAndResiduals"))

workflowUnitInObs$runWorkflow()
workflowLLOQ$runWorkflow()
workflowMissingLLOQ$runWorkflow()


test_that("Workflow structure includes appropriate files and folders", {
  expect_equal(list.files(workflowUnitInObs$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowLLOQ$workflowFolder), refWorkflowStructure)
  expect_equal(list.files(workflowMissingLLOQ$workflowFolder), refWorkflowStructure)
})

test_that("Time profile directory includes correct files and folders", {
  expect_equal(list.files(file.path(workflowUnitInObs$workflowFolder, "TimeProfiles")), timeProfileStructure)
  expect_equal(list.files(file.path(workflowLLOQ$workflowFolder, "TimeProfiles")), timeProfileStructure)
  expect_equal(list.files(file.path(workflowMissingLLOQ$workflowFolder, "TimeProfiles")), timeProfileStructure)
})

test_that("Saved time profile data and residuals includes the correct data", {
  expect_equal(readObservedDataFile(file.path(workflowUnitInObs$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), 
               readObservedDataFile(refOutputTimeProfile),
               tolerance = comparisonTolerance()
  )
  expect_equal(readObservedDataFile(file.path(workflowLLOQ$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), 
               readObservedDataFile(refOutputTimeProfileLLOQ),
               tolerance = comparisonTolerance()
  )
  expect_equal(readObservedDataFile(file.path(workflowMissingLLOQ$workflowFolder, "TimeProfiles", "A-timeProfileData.csv")), 
               readObservedDataFile(refOutputTimeProfile),
               tolerance = comparisonTolerance()
  )
  expect_equal(readObservedDataFile(file.path(workflowUnitInObs$workflowFolder, "TimeProfiles", "residuals.csv")), 
               readObservedDataFile(refOutputResiduals),
               tolerance = comparisonTolerance()
  )
  expect_equal(readObservedDataFile(file.path(workflowLLOQ$workflowFolder, "TimeProfiles", "residuals.csv")), 
               readObservedDataFile(refOutputResiduals),
               tolerance = comparisonTolerance()
  )
  expect_equal(readObservedDataFile(file.path(workflowMissingLLOQ$workflowFolder, "TimeProfiles", "residuals.csv")), 
               readObservedDataFile(refOutputResiduals),
               tolerance = comparisonTolerance()
  )
})

# Clear test workflow folders
unlink(workflowUnitInObs$workflowFolder, recursive = TRUE)
unlink(workflowLLOQ$workflowFolder, recursive = TRUE)
unlink(workflowMissingLLOQ$workflowFolder, recursive = TRUE)