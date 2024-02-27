# Get a few setting values for testing
formatNumericsScientific <- getRESettings(reSettingsNames$formatNumericsScientific)$Value
formatNumericsDigits <- getRESettings(reSettingsNames$formatNumericsDigits)$Value
mcRandomSeed <- getRESettings(reSettingsNames$defaultMCRandomSeed)$Value

test_that("Changes of default settings work correctly", {
  setDefaultPlotFormat(format = "pdf")
  expect_equal(getRESettings(reSettingsNames$defaultPlotFormat)$Value$format, "pdf")
  
  setDefaultNumericFormat(digits = 3, scientific = TRUE)
  expect_equal(getRESettings(reSettingsNames$formatNumericsScientific)$Value, TRUE)
  expect_equal(getRESettings(reSettingsNames$formatNumericsDigits)$Value, 3)
  
  setDefaultBins(3)
  expect_equal(getRESettings(reSettingsNames$defaultBins)$Value, 3)
  
  setDefaultStairstep(FALSE)
  expect_equal(getRESettings(reSettingsNames$defaultStairstep)$Value, FALSE)
  
  setDefaultAutoAxisLimitMargin(0.2)
  expect_equal(getRESettings(reSettingsNames$autoAxisLimitMargin)$Value, 0.2)
  
  setDefaultMCRandomSeed(100)
  expect_equal(getRESettings(reSettingsNames$defaultMCRandomSeed)$Value, 100)
  expect_equal(getDefaultMCRandomSeed(), 100)
  
  setDefaultMCRepetitions(100)
  expect_equal(getRESettings(reSettingsNames$defaultMCRepetitions)$Value, 100)
  expect_equal(getDefaultMCRepetitions(), 100)
  
  setDefaultTimeProfileStatistics(statisticsType = StatisticsTypes$`Arithmetic mean`)
  expect_equal(getRESettings(reSettingsNames$defaultTimeProfileStatistics)$Value$y, "mean")
  
})

test_that("Default settings can be saved", {
  saveRESettings("test-settings.RData")
  expect_true(file.exists("test-settings.RData"))
})

test_that("Default settings are reset to RE Default", {
  resetRESettingsToDefault()
  expect_equal(getRESettings(reSettingsNames$formatNumericsScientific)$Value, formatNumericsScientific)
  expect_equal(getRESettings(reSettingsNames$formatNumericsDigits)$Value, formatNumericsDigits)
  expect_equal(getRESettings(reSettingsNames$defaultMCRandomSeed)$Value, mcRandomSeed)
})

test_that("Previous default settings are reloaded", {
  loadRESettings("test-settings.RData")
  expect_equal(getRESettings(reSettingsNames$formatNumericsScientific)$Value, TRUE)
  expect_equal(getRESettings(reSettingsNames$formatNumericsDigits)$Value, 3)
  expect_equal(getRESettings(reSettingsNames$defaultMCRandomSeed)$Value, 100)
})

# Clean up
resetRESettingsToDefault()
unlink("test-settings.RData")
