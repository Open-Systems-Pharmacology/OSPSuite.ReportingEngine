context("Output class")
# It seems that some instances crash if library(ospsuite) is not loaded
library(ospsuite)

testPath <- "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)"
testPathName <- "Raltegravir Plasma"
testPathUnit <- "nmol/l"
testDataFilter <- "Grouping %in% '10mg_'"
testDataDisplayName <- "My test data"

test_that("Output requires a 'path' as a single character value", {
  expect_error(Output$new())
  expect_error(Output$new(path = NULL))
  expect_error(Output$new(path = data.frame(testPath)))
  expect_error(Output$new(path = c(testPath, testPath)))
  expect_silent(Output$new(path = testPath))
  
  outputPath <- Output$new(path = testPath)
  expect_is(outputPath, "Output")
  expect_equal(outputPath$path, testPath)
})

test_that("Output 'displayName' is checked and set properly", {
  expect_silent(Output$new(path = testPath, displayName = testPathName))
  
  # display name is equal to path when not set
  outputPath <- Output$new(path = testPath)
  expect_equal(outputPath$displayName, outputPath$path)
  
  outputPathName <- Output$new(path = testPath, displayName = testPathName)
  expect_equal(outputPathName$displayName, testPathName)
  
  expect_error(Output$new(path = testPath, dislpayName = data.frame(testPathName)))
  expect_error(Output$new(path = testPath, dislpayName = c(testPathName, testPathName)))
  
})

test_that("Output 'displayUnit' is checked and set properly", {
  expect_silent(Output$new(path = testPath, displayUnit = testPathUnit))
  
  # display name is equal to path when not set
  outputPath <- Output$new(path = testPath)
  expect_null(outputPath$displayUnit)
  
  outputPathUnit <- Output$new(path = testPath, displayUnit = testPathUnit)
  expect_equal(outputPathUnit$displayUnit, testPathUnit)
  
  expect_error(Output$new(path = testPath, dislpayUnit = data.frame(testPathUnit)))
  expect_error(Output$new(path = testPath, dislpayUnit = c(testPathUnit, testPathUnit)))
  
})

test_that("Output 'dataDisplayName' is checked and set properly", {
  expect_silent(Output$new(path = testPath, dataDisplayName = testDataDisplayName))
  
  # display name is equal to path when not set
  outputPath <- Output$new(path = testPath)
  #expect_null(outputPath$dataDisplayName)
  
  outputDataDisplay <- Output$new(path = testPath, dataDisplayName = testDataDisplayName)
  expect_equal(outputDataDisplay$dataDisplayName, testDataDisplayName)
  
  expect_error(Output$new(path = testPath, dataDisplayName = data.frame(testDataDisplayName)))
  expect_error(Output$new(path = testPath, dataDisplayName = c(testDataDisplayName, testDataDisplayName)))
  
})

test_that("Output 'dataFilter' is checked and set properly", {
  expect_silent(Output$new(path = testPath, dataFilter = testDataFilter))
  
  outputPath <- Output$new(path = testPath)
  expect_null(outputPath$dataFilter)
  
  outputFilter <- Output$new(path = testPath, dataFilter = testDataFilter)
  expect_is(outputFilter$dataFilter, "expression")
  expect_equivalent(deparse(outputFilter$dataFilter), deparse(parse(text = testDataFilter)))
  
  # Expressions can be input
  outputFilter <- Output$new(path = testPath, dataFilter = parse(text = testDataFilter))
  expect_is(outputFilter$dataFilter, "expression")
  expect_equivalent(deparse(outputFilter$dataFilter), deparse(parse(text = testDataFilter)))
  
  expect_error(Output$new(path = testPath, dataFilter = data.frame(testDataFilter)))
  expect_error(Output$new(path = testPath, dataFilter = c(testDataFilter, testDataFilter)))
  
})

myTestAUCName <- "AUC_inf"
myTestWrongName <- "Wrong parameter"
myTestCmaxName <- "C_max"
myTestCmaxDisplayName <- "My test Cmax"
myTestCmaxDisplayUnit <- "nmol"

test_that("'PkParameterInfo' works properly", {
  expect_silent(PkParameterInfo$new(pkParameter = myTestAUCName))
  expect_is(PkParameterInfo$new(pkParameter = myTestAUCName), "PkParameterInfo")
  
  expect_error(PkParameterInfo$new())
  expect_error(PkParameterInfo$new(pkParameter = data.frame(myTestAUCName)))
  expect_error(PkParameterInfo$new(pkParameter = c(myTestAUCName, myTestAUCName)))
  # This test check if unexisting PK parameter names are flagged
  expect_error(PkParameterInfo$new(pkParameter = myTestWrongName))
  
  expect_silent(PkParameterInfo$new(pkParameter = myTestCmaxName,
                                    displayName = myTestCmaxDisplayName,
                                    displayUnit = myTestCmaxDisplayUnit))
  
  # display name is equal to path when not set
  myCmax <- PkParameterInfo$new(pkParameter = myTestCmaxName)
  expect_equal(myCmax$pkParameter, myTestCmaxName)
  expect_null(myCmax$displayName)
  expect_null(myCmax$displayUnit)
  
  myCmax <- PkParameterInfo$new(pkParameter = myTestCmaxName,
                                displayName = myTestCmaxDisplayName,
                                displayUnit = myTestCmaxDisplayUnit)
  expect_equal(myCmax$pkParameter, myTestCmaxName)
  expect_equal(myCmax$displayName, myTestCmaxDisplayName)
  expect_equal(myCmax$displayUnit, myTestCmaxDisplayUnit)
  
  expect_error(PkParameterInfo$new(pkParameter = myTestCmaxName, displayName = data.frame(myTestCmaxDisplayName)))
  expect_error(PkParameterInfo$new(pkParameter = myTestCmaxName, displayUnit = data.frame(myTestCmaxDisplayUnit)))
  expect_error(PkParameterInfo$new(pkParameter = myTestCmaxName, displayName = c(myTestCmaxDisplayName, myTestCmaxDisplayName)))
  expect_error(PkParameterInfo$new(pkParameter = myTestCmaxName, displayUnit = c(myTestCmaxDisplayUnit, myTestCmaxDisplayUnit)))
  
  })

test_that("Output 'pkParameters' are checked and set properly", {
  myTestAUC <- PkParameterInfo$new("AUC_inf")
  myTestCmax <- PkParameterInfo$new(myTestCmaxName, 
                                    displayName = myTestCmaxDisplayName,
                                    displayUnit = myTestCmaxDisplayUnit)
  
  outputPath <- Output$new(path = testPath)
  expect_null(outputPath$pkParameters)
  
  expect_silent(Output$new(path = testPath, pkParameters = myTestAUCName))
  expect_silent(Output$new(path = testPath, pkParameters = myTestAUC))
  expect_error(Output$new(path = testPath, pkParameters = data.frame(myTestAUCName)))
  
  outputAUCName <- Output$new(path = testPath, pkParameters = myTestAUCName)
  outputAUC <- Output$new(path = testPath, pkParameters = myTestAUC)
  
  for(pkParameter in outputAUCName$pkParameters){expect_is(pkParameter, "PkParameterInfo")}
  
  for(pkParameter in outputAUC$pkParameters){expect_is(pkParameter, "PkParameterInfo")}
  expect_equivalent(outputAUC$pkParameters, outputAUCName$pkParameters)
  
  outputPK <- Output$new(path = testPath, pkParameters = c(myTestAUC, myTestCmax))
  expect_equal(outputPK$pkParameters[[1]], myTestAUC)
  expect_equal(outputPK$pkParameters[[2]], myTestCmax)
  expect_equal(outputPK$pkParameters[[1]], outputAUC$pkParameters[[1]])
  expect_equal(outputPK$pkParameters[[1]], outputAUCName$pkParameters[[1]])
  
})