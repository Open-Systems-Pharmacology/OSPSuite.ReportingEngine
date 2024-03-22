context("Study Design")

populationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal-Population.csv")
simulationFile <- getTestDataFilePath("input-data/Larson 2013 8-18y meal.pkml")
studyDesignFile <- getTestDataFilePath("input-data/StudyDesign.csv")
studyDesignTempFile <- "TestStudyDesign.csv"
drugMassPath <- "Applications|Larson 400mg|filmcoated tablet (original Merck formulation)|Application_1|ProtocolSchemaItem|DrugMass"

refSimulationSet <- PopulationSimulationSet$new(
  simulationSetName = "ReferenceTest",
  simulationFile = simulationFile,
  populationFile = populationFile
)
studyDesignSimulationSet <- PopulationSimulationSet$new(
  simulationSetName = "StudyDesignTest",
  simulationFile = simulationFile,
  populationFile = populationFile,
  studyDesignFile = studyDesignFile
)

test_that("Loaded population object is same when no study design file is input", {
  referencePopulation <- ospsuite::loadPopulation(populationFile)
  testPopulation <- loadWorkflowPopulation(refSimulationSet)
  expect_equal(testPopulation, referencePopulation)
})

test_that("Target path is added to population object if not existing", {
  referencePopulation <- loadWorkflowPopulation(refSimulationSet)
  testPopulation <- loadWorkflowPopulation(studyDesignSimulationSet)

  expect_false(ospsuite.utils::isIncluded(drugMassPath, referencePopulation$allParameterPaths))
  expect_true(ospsuite.utils::isIncluded(drugMassPath, testPopulation$allParameterPaths))
})

test_that("A study design needs at least one 'SOURCE' and one 'TARGET'", {
  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)

  # No target
  studyDesignData <- data.frame("Organism|Weight" = c("kg", "SOURCE_MIN", 5), check.names = FALSE)
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  expect_error(loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation))
  expect_error(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  unlink(studyDesignTempFile, recursive = TRUE)

  # No source
  studyDesignData <- data.frame("Organism|Weight" = c("kg", "TARGET", 5), check.names = FALSE)
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  expect_error(loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation))
  expect_error(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  unlink(studyDesignTempFile, recursive = TRUE)
})

test_that("Units and paths are checked and converted to base unit when loading a study design", {
  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)

  # Example on Height to be converted from cm to dm (base unit)
  studyDesignData <- data.frame(
    "Organism|Weight" = c("kg", "SOURCE_MIN", 50),
    "Organism|Weight" = c("kg", "SOURCE_MAX", 70),
    "Organism|Height" = c("cm", "TARGET", 170),
    check.names = FALSE
  )
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  studyDesign <- loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation)
  expect_s3_class(studyDesign, "StudyDesign")
  expect_equal(studyDesign$targets[[1]]$name, "Organism|Height")
  expect_equal(studyDesign$targets[[1]]$values, 17)
  expect_null(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  unlink(studyDesignTempFile, recursive = TRUE)

  # Example on source Weight in wrong unit
  studyDesignData <- data.frame(
    "Organism|Weight" = c("l", "SOURCE_MIN", 50),
    "Organism|Weight" = c("kg", "SOURCE_MAX", 70),
    "Organism|Height" = c("cm", "TARGET", 170),
    check.names = FALSE
  )
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  expect_error(loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation))
  expect_error(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  unlink(studyDesignTempFile, recursive = TRUE)

  # Example on taret height in wrong unit
  studyDesignData <- data.frame(
    "Organism|Weight" = c("kg", "SOURCE_MIN", 50),
    "Organism|Weight" = c("kg", "SOURCE_MAX", 70),
    "Organism|Height" = c("kg", "TARGET", 170),
    check.names = FALSE
  )
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  expect_error(loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation))
  expect_error(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  unlink(studyDesignTempFile, recursive = TRUE)

  # Example on covariate such as Gender is handled
  studyDesignData <- data.frame(
    "Gender" = c("", "SOURCE_EQUALS", 2),
    "Organism|Height" = c("cm", "TARGET", 170),
    check.names = FALSE
  )
  write.csv(studyDesignData, file = studyDesignTempFile, row.names = FALSE)
  expect_s3_class(loadStudyDesign(studyDesignTempFile, testPopulation, testSimulation), "StudyDesign")
  expect_null(addStudyParameters(testPopulation, testSimulation, studyDesignTempFile))
  addStudyParameters(testPopulation, testSimulation, studyDesignTempFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  # Base unit gives 17 dm for target
  expect_equal(sum(populationData[, "Gender"] %in% 2), sum(populationData[, "Organism|Height"] %in% 17))
  unlink(studyDesignTempFile, recursive = TRUE)
})

studyDesignMin <- data.frame(source = c("kg", "SOURCE_MIN", ""), target = c("nmol", "TARGET", 5000), check.names = FALSE)
studyDesignMax <- data.frame(source = c("kg", "SOURCE_MAX", ""), target = c("nmol", "TARGET", 5000), check.names = FALSE)
studyDesignEquals <- data.frame(source = c("kg", "SOURCE_EQUALS", ""), target = c("nmol", "TARGET", 5000), check.names = FALSE)
studyDesignElse <- data.frame(source = c("kg", "SOURCE_ELSE", ""), target = c("nmol", "TARGET", 5000), check.names = FALSE)
names(studyDesignMin) <- c("Organism|Weight", drugMassPath)
names(studyDesignMax) <- c("Organism|Weight", drugMassPath)
names(studyDesignEquals) <- c("Organism|Weight", drugMassPath)
names(studyDesignElse) <- c("Organism|Weight", drugMassPath)

studyDesignMinFile <- "studyDesignMin.csv"
studyDesignMaxFile <- "studyDesignMax.csv"
studyDesignEqualsFile <- "studyDesignEquals.csv"
studyDesignElseFile <- "studyDesignElse.csv"

write.csv(studyDesignMin, file = studyDesignMinFile, row.names = FALSE)
write.csv(studyDesignMax, file = studyDesignMaxFile, row.names = FALSE)
write.csv(studyDesignEquals, file = studyDesignEqualsFile, row.names = FALSE)
write.csv(studyDesignElse, file = studyDesignElseFile, row.names = FALSE)

test_that("A study design 'SOURCE' requires a 'MIN', 'MAX', or 'EQUALS' attribute", {
  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)

  expect_s3_class(loadStudyDesign(studyDesignMinFile, testPopulation, testSimulation), "StudyDesign")
  expect_s3_class(loadStudyDesign(studyDesignMaxFile, testPopulation, testSimulation), "StudyDesign")
  expect_s3_class(loadStudyDesign(studyDesignEqualsFile, testPopulation, testSimulation), "StudyDesign")
  expect_error(loadStudyDesign(studyDesignElseFile, testPopulation, testSimulation))

  expect_null(addStudyParameters(testPopulation, testSimulation, studyDesignMinFile))
  expect_null(addStudyParameters(testPopulation, testSimulation, studyDesignMaxFile))
  expect_null(addStudyParameters(testPopulation, testSimulation, studyDesignEqualsFile))
  expect_error(addStudyParameters(testPopulation, testSimulation, studyDesignElseFile))
})

test_that("An empty 'SOURCE' value means no condition constraint", {
  testSimulation <- ospsuite::loadSimulation(simulationFile)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignMinFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), nrow(populationData))

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignMaxFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), nrow(populationData))

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignEqualsFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), nrow(populationData))
})

unlink(studyDesignMinFile, recursive = TRUE)
unlink(studyDesignMaxFile, recursive = TRUE)
unlink(studyDesignEqualsFile, recursive = TRUE)
unlink(studyDesignElseFile, recursive = TRUE)

test_that("Source expression 'MIN' include only values >=", {
  # Target base unit is umol
  studyDesignMin <- data.frame(source = c("kg", "SOURCE_MIN", 50), target = c("nmol", "TARGET", 5000), check.names = FALSE)
  names(studyDesignMin) <- c("Organism|Weight", drugMassPath)
  studyDesignMinFile <- "studyDesignMin.csv"
  write.csv(studyDesignMin, file = studyDesignMinFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignMinFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), 42)
  expect_equal(sum(is.na(populationData[, drugMassPath])), 58)
  expect_gte(min(populationData[populationData[, drugMassPath] %in% 5, "Organism|Weight"]), 50)

  unlink(studyDesignMinFile, recursive = TRUE)
})

test_that("Source expression 'MAX' include only values <", {
  studyDesignMax <- data.frame(source = c("kg", "SOURCE_MAX", 50), target = c("nmol", "TARGET", 5000), check.names = FALSE)
  names(studyDesignMax) <- c("Organism|Weight", drugMassPath)
  studyDesignMaxFile <- "studyDesignMax.csv"
  write.csv(studyDesignMax, file = studyDesignMaxFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignMaxFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), 58)
  expect_equal(sum(is.na(populationData[, drugMassPath])), 42)
  expect_lte(max(populationData[populationData[, drugMassPath] %in% 5, "Organism|Weight"]), 50)

  unlink(studyDesignMaxFile, recursive = TRUE)
})

test_that("Source expression 'EQUALS' include only values >= and attribute correct value", {
  studyDesignEquals <- data.frame(source = c("", "SOURCE_EQUALS", 0.4), target = c("nmol", "TARGET", 5000), check.names = FALSE)
  names(studyDesignEquals) <- c("Organism|Hematocrit", drugMassPath)
  studyDesignEqualsFile <- "studyDesignEquals.csv"
  write.csv(studyDesignEquals, file = studyDesignEqualsFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  testSimulation <- ospsuite::loadSimulation(simulationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignEqualsFile)

  populationData <- ospsuite::populationToDataFrame(testPopulation)
  expect_equal(sum(populationData[, drugMassPath] %in% 5), 49)
  expect_equal(sum(is.na(populationData[, drugMassPath])), 51)
  expect_equal(populationData[populationData[, drugMassPath] %in% 5, "Organism|Hematocrit"], rep(0.4, 49))

  unlink(studyDesignEqualsFile, recursive = TRUE)
})

test_that("Source expressions constraints add up as &", {
  testSimulation <- ospsuite::loadSimulation(simulationFile)

  studyDesign <- data.frame(source1 = c("kg", "SOURCE_MIN", 45), source2 = c("kg", "SOURCE_MAX", 55), target = c("nmol", "TARGET", 5000), check.names = FALSE)
  names(studyDesign) <- c("Organism|Weight", "Organism|Weight", drugMassPath)

  studyDesignNA <- data.frame(source1 = c("kg", "SOURCE_MIN", 10), source2 = c("kg", "SOURCE_MAX", 10), target = c("nmol", "TARGET", 5000), check.names = FALSE)
  names(studyDesignNA) <- c("Organism|Weight", "Organism|Weight", drugMassPath)

  studyDesignFile <- "studyDesign.csv"
  studyDesignNAFile <- "studyDesignNA.csv"
  write.csv(studyDesign, file = studyDesignFile, row.names = FALSE)
  write.csv(studyDesignNA, file = studyDesignNAFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)

  expect_equal(sum(populationData[, drugMassPath] %in% 5), 22)
  expect_equal(sum(is.na(populationData[, drugMassPath])), 78)
  expect_gte(min(populationData[populationData[, drugMassPath] %in% 5, "Organism|Weight"]), 45)
  expect_lte(max(populationData[populationData[, drugMassPath] %in% 5, "Organism|Weight"]), 55)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, testSimulation, studyDesignNAFile)
  populationData <- ospsuite::populationToDataFrame(testPopulation)

  expect_equal(sum(populationData[, drugMassPath] %in% 5), 0)
  expect_equal(sum(is.na(populationData[, drugMassPath])), 100)

  unlink(studyDesignFile, recursive = TRUE)
  unlink(studyDesignNAFile, recursive = TRUE)
})

# Clear logs
unlink("log-error.txt", recursive = TRUE)
unlink("log-debug.txt", recursive = TRUE)
unlink("log-info.txt", recursive = TRUE)
