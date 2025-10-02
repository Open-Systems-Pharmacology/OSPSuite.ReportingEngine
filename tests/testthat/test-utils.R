test_that("geomean and sd handle NAs and provide geometric mean and sd", {
  expect_equal(geomean(exp(1:3)), exp(2))
  expect_equal(geomeanMultipliedBySD(exp(1:3)), exp(3))
  expect_equal(geomeanDividedBySD(exp(1:3)), exp(1))
  
  expect_equal(geomean(c(-1, exp(1:3))), exp(2))
  expect_equal(geomeanMultipliedBySD(c(-1, exp(1:3))), exp(3))
  expect_equal(geomeanDividedBySD(c(-1, exp(1:3))), exp(1))
  
  expect_equal(geomean(exp(c(1, NA, 2, 3))), exp(2))
  expect_equal(geomeanMultipliedBySD(exp(c(1, NA, 2, 3))), exp(3))
  expect_equal(geomeanDividedBySD(exp(c(1, NA, 2, 3))), exp(1))
  
})

test_that("GMFE handle NAs and correct values", {
  expect_equal(calculateGMFE(rep(1, 3), rep(1.2, 3)), 1.2)
  expect_equal(calculateGMFE(2*rep(1, 3), 2*rep(1.2, 3)), 1.2)
  expect_equal(calculateGMFE(c(-1, rep(1, 3)), rep(1.2, 4)), 1.2)
  expect_equal(calculateGMFE(c(NA, rep(1, 3)), rep(1.2, 4)), 1.2)
})

test_that("generateResultFileNames number based on cores", {
  expect_equal(
    generateResultFileNames(1, "test-folder", "test-file"),
    "test-folder/test-file-1.csv"
  )
  expect_equal(
    generateResultFileNames(3, "test-folder", "test-file"),
    paste0("test-folder/test-file-", 1:3, ".csv")
  )
})

test_that("parsing variables works as expected", {
  variableToObjectExpression <- ospsuite.reportingengine:::parseVariableToObject(
    objectName = "testObject",
    variableName = "testVariable",
    keepIfNull = FALSE
  )
  expect_is(variableToObjectExpression, "expression")
  expect_equal(
    as.character(variableToObjectExpression),
    "testObject$testVariable <- testVariable"
  )
  variableToObjectExpression <- ospsuite.reportingengine:::parseVariableToObject(
    objectName = "testObject",
    variableName = "testVariable",
    keepIfNull = TRUE
  )
  expect_is(variableToObjectExpression, "expression")
  expect_equal(
    as.character(variableToObjectExpression),
    "testObject$testVariable <- testVariable %||% testObject$testVariable"
  )
})

test_that("getStatisticsFromType get right names from type", {
  arithmeticStats <- getStatisticsFromType(StatisticsTypes$`Arithmetic mean`)
  expect_equal(arithmeticStats$y, "mean")
  expect_equal(arithmeticStats$ymin, "mean-sd")
  expect_equal(arithmeticStats$ymax, "mean+sd")
  expect_equal(arithmeticStats$yCaption, "arithmetic mean")
  expect_equal(arithmeticStats$rangeCaption, "mean \u00b1 SD range")
  
  geometricStats <- getStatisticsFromType(StatisticsTypes$`Geometric mean`)
  expect_equal(geometricStats$y, "geomean")
  expect_equal(geometricStats$ymin, "geomeanDividedBySD")
  expect_equal(geometricStats$ymax, "geomeanMultipliedBySD")
  expect_equal(geometricStats$yCaption, "geometric mean")
  expect_equal(geometricStats$rangeCaption, "mean */ geometric SD range")
  
  percentiles80Stats <- getStatisticsFromType(StatisticsTypes$`10th-90th Percentiles`)
  expect_equal(percentiles80Stats$y, "Percentile50%")
  expect_equal(percentiles80Stats$ymin, "Percentile10%")
  expect_equal(percentiles80Stats$ymax, "Percentile90%")
  expect_equal(percentiles80Stats$yCaption, "median")
  expect_equal(percentiles80Stats$rangeCaption, "[10\u1d57\u02b0-90\u1d57\u02b0] percentiles")
  
  percentiles90Stats <- getStatisticsFromType(StatisticsTypes$`5th-95th Percentiles`)
  expect_equal(percentiles90Stats$y, "Percentile50%")
  expect_equal(percentiles90Stats$ymin, "Percentile5%")
  expect_equal(percentiles90Stats$ymax, "Percentile95%")
  expect_equal(percentiles90Stats$yCaption, "median")
  expect_equal(percentiles90Stats$rangeCaption, "[5\u1d57\u02b0-95\u1d57\u02b0] percentiles")
  
  percentiles95Stats <- getStatisticsFromType(StatisticsTypes$`2.5th-97.5th Percentiles`)
  expect_equal(percentiles95Stats$y, "Percentile50%")
  expect_equal(percentiles95Stats$ymin, "Percentile2.5%")
  expect_equal(percentiles95Stats$ymax, "Percentile97.5%")
  expect_equal(percentiles95Stats$yCaption, "median")
  expect_equal(percentiles95Stats$rangeCaption, "[2.5\u1d57\u02b0-97.5\u1d57\u02b0] percentiles")
})



getStatisticsFromType <- function(statisticsType) {
  validateIsIncluded(statisticsType, StatisticsTypes)
  if (isIncluded(statisticsType, StatisticsTypes$`2.5th-97.5th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile2.5%`,
      ymax = tlf::tlfStatFunctions$`Percentile97.5%`,
      yCaption = "median",
      # The unicode characters below are superscript th
      rangeCaption = "[2.5\u1d57\u02b0-97.5\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`5th-95th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile5%`,
      ymax = tlf::tlfStatFunctions$`Percentile95%`,
      yCaption = "median",
      rangeCaption = "[5\u1d57\u02b0-95\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`10th-90th Percentiles`)) {
    return(list(
      y = tlf::tlfStatFunctions$`Percentile50%`,
      ymin = tlf::tlfStatFunctions$`Percentile10%`,
      ymax = tlf::tlfStatFunctions$`Percentile90%`,
      yCaption = "median",
      rangeCaption = "[10\u1d57\u02b0-90\u1d57\u02b0] percentiles"
    ))
  }
  if (isIncluded(statisticsType, StatisticsTypes$`Arithmetic mean`)) {
    return(list(
      y = tlf::tlfStatFunctions$mean,
      ymin = tlf::tlfStatFunctions$`mean-sd`,
      ymax = tlf::tlfStatFunctions$`mean+sd`,
      yCaption = "arithmetic mean",
      # The unicode character below is +/- symbol
      rangeCaption = "mean \u00b1 SD range"
    ))
  }
  return(list(
    y = "geomean",
    ymin = "geomeanDividedBySD",
    ymax = "geomeanMultipliedBySD",
    yCaption = "geometric mean",
    # The unicode character below is supposed to be */ symbol
    rangeCaption = "mean */ geometric SD range"
  ))
}