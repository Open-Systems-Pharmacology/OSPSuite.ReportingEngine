context("Study Design")

populationFile <- "../dev/03_Population_Pediatric_Raltegavir/Larson 2013 8-18y meal-Population.csv"

test_that("Empty study design does not affect a population object", {
  referencePopulation <- ospsuite::loadPopulation(populationFile)
  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, NULL)

  expect_equal(testPopulation, referencePopulation)
})

test_that("A study design needs at least one 'SOURCE' and one 'TARGET'", {
  testPopulation <- ospsuite::loadPopulation(populationFile)

  studyDesignNoTarget <- data.frame("Organism|Weight" = c("kg", "SOURCE_MIN", 5), check.names = FALSE)
  studyDesignNoSource <- data.frame("Organism|Weight" = c("kg", "TARGET", 5), check.names = FALSE)
  studyDesignNoTargetFile <- "studyDesignNoTarget.csv"
  studyDesignNoSourceFile <- "studyDesignNoSource.csv"

  write.csv(studyDesignNoTarget, file = studyDesignNoTargetFile, row.names = FALSE)
  write.csv(studyDesignNoSource, file = studyDesignNoSourceFile, row.names = FALSE)

  expect_error(loadStudyDesign(studyDesignNoTargetFile))
  expect_error(loadStudyDesign(studyDesignNoSourceFile))
  expect_error(addStudyParameters(testPopulation, studyDesignNoTargetFile))
  expect_error(addStudyParameters(testPopulation, studyDesignNoSourceFile))

  unlink(studyDesignNoTargetFile, recursive = TRUE)
  unlink(studyDesignNoSourceFile, recursive = TRUE)
})

studyDesignMin <- data.frame("Organism|Weight" = c("kg", "SOURCE_MIN", ""), "Test" = c("mg", "TARGET", 5), check.names = FALSE)
studyDesignMax <- data.frame("Organism|Weight" = c("kg", "SOURCE_MAX", ""), "Test" = c("mg", "TARGET", 5), check.names = FALSE)
studyDesignEquals <- data.frame("Organism|Weight" = c("kg", "SOURCE_EQUALS", ""), "Test" = c("mg", "TARGET", 5), check.names = FALSE)
studyDesignElse <- data.frame("Organism|Weight" = c("kg", "SOURCE_ELSE", ""), "Test" = c("mg", "TARGET", 5), check.names = FALSE)

studyDesignMinFile <- "studyDesignMin.csv"
studyDesignMaxFile <- "studyDesignMax.csv"
studyDesignEqualsFile <- "studyDesignEquals.csv"
studyDesignElseFile <- "studyDesignElse.csv"

write.csv(studyDesignMin, file = studyDesignMinFile, row.names = FALSE)
write.csv(studyDesignMax, file = studyDesignMaxFile, row.names = FALSE)
write.csv(studyDesignEquals, file = studyDesignEqualsFile, row.names = FALSE)
write.csv(studyDesignElse, file = studyDesignElseFile, row.names = FALSE)

test_that("A study design 'SOURCE' requires a 'MIN', 'MAX', or 'EQUALS' attribute", {
  expect_silent(loadStudyDesign(studyDesignMinFile))
  expect_silent(loadStudyDesign(studyDesignMaxFile))
  expect_silent(loadStudyDesign(studyDesignEqualsFile))
  expect_error(loadStudyDesign(studyDesignElseFile))

  testPopulation <- ospsuite::loadPopulation(populationFile)
  expect_silent(addStudyParameters(testPopulation, studyDesignMinFile))
  testPopulation <- ospsuite::loadPopulation(populationFile)
  expect_silent(addStudyParameters(testPopulation, studyDesignMaxFile))
  testPopulation <- ospsuite::loadPopulation(populationFile)
  expect_silent(addStudyParameters(testPopulation, studyDesignEqualsFile))
  testPopulation <- ospsuite::loadPopulation(populationFile)
  expect_error(addStudyParameters(testPopulation, studyDesignElseFile))
})

test_that("An empty 'SOURCE' value means no condition constraint", {
  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignMinFile)
  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test == 5), nrow(populationData))

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignMaxFile)
  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test == 5), nrow(populationData))

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignEqualsFile)
  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test == 5), nrow(populationData))
})

unlink(studyDesignMinFile, recursive = TRUE)
unlink(studyDesignMaxFile, recursive = TRUE)
unlink(studyDesignEqualsFile, recursive = TRUE)
unlink(studyDesignElseFile, recursive = TRUE)

test_that("Source expression 'MIN' include only values >=", {
  studyDesignMin <- data.frame(
    "Gender" = c("", "SOURCE_MIN", 2),
    "Test" = c("mg", "TARGET", 5), check.names = FALSE
  )
  studyDesignMinFile <- "studyDesignMin.csv"
  write.csv(studyDesignMin, file = studyDesignMinFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignMinFile)

  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test %in% 5), 250)
  expect_equal(sum(populationData$Test %in% NA), 250)
  expect_equal(min(populationData[populationData$Test %in% 5, "Gender"] >= 2), 1)

  unlink(studyDesignMinFile, recursive = TRUE)
})

test_that("Source expression 'MAX' include only values <", {
  studyDesignMax <- data.frame(
    "Gender" = c("", "SOURCE_MAX", 2),
    "Test" = c("mg", "TARGET", 5), check.names = FALSE
  )
  studyDesignMaxFile <- "studyDesignMax.csv"
  write.csv(studyDesignMax, file = studyDesignMaxFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignMaxFile)

  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test %in% 5), 250)
  expect_equal(min(populationData[populationData$Test %in% 5, "Gender"] < 2), 1)

  unlink(studyDesignMaxFile, recursive = TRUE)
})

test_that("Source expression 'EQUALS' include only values >= and attribute correct value", {
  studyDesignEquals <- data.frame(
    "Gender" = c("", "SOURCE_EQUALS", 2),
    "Test" = c("mg", "TARGET", 5), check.names = FALSE
  )
  studyDesignEqualsFile <- "studyDesignEquals.csv"
  write.csv(studyDesignEquals, file = studyDesignEqualsFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignEqualsFile)

  populationData <- ospsuite::populationAsDataFrame(testPopulation)
  expect_equal(sum(populationData$Test %in% 5), 250)
  expect_equal(sum(populationData$Test %in% NA), 250)
  expect_equal(min(populationData[populationData$Test %in% 5, "Gender"] >= 2), 1)

  unlink(studyDesignEqualsFile, recursive = TRUE)
})

test_that("Source expressions constraints add up as &", {
  studyDesign <- data.frame(
    "Gender" = c("", "SOURCE_MIN", 1),
    "Gender" = c("", "SOURCE_MAX", 2),
    "Test" = c("mg", "TARGET", 5), check.names = FALSE
  )
  studyDesignNA <- data.frame(
    "Gender" = c("", "SOURCE_MIN", 2),
    "Gender" = c("", "SOURCE_MAX", 2),
    "Test" = c("mg", "TARGET", 5), check.names = FALSE
  )
  studyDesignFile <- "studyDesign.csv"
  studyDesignNAFile <- "studyDesignNA.csv"
  write.csv(studyDesign, file = studyDesignFile, row.names = FALSE)
  write.csv(studyDesignNA, file = studyDesignNAFile, row.names = FALSE)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignFile)
  populationData <- ospsuite::populationAsDataFrame(testPopulation)

  expect_equal(sum(populationData$Test %in% 5), 250)
  expect_equal(sum(populationData$Test %in% NA), 250)
  expect_equal(min(populationData[populationData$Test %in% 5, "Gender"] >= 1 &
    populationData[populationData$Test %in% 5, "Gender"] < 2), 1)

  testPopulation <- ospsuite::loadPopulation(populationFile)
  addStudyParameters(testPopulation, studyDesignNAFile)
  populationData <- ospsuite::populationAsDataFrame(testPopulation)

  expect_equal(sum(populationData$Test %in% 5), 0)
  expect_equal(sum(populationData$Test %in% NA), 500)

  unlink(studyDesignFile, recursive = TRUE)
  unlink(studyDesignNAFile, recursive = TRUE)
})
