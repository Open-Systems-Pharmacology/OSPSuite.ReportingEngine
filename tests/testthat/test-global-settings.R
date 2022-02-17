# To access the global settings from unexported reEnv, 
# ospsuite.reportingengine:::reEnv needs to be used

test_that("Functions setting global properties work", {
  # Plot format
  setDefaultPlotFormat(format = "pdf")
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$format, "pdf")
  setDefaultPlotFormat(format = "png")
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$format, "png")
  setDefaultPlotFormat(width = 16, height = 9, units = "cm", dpi = 300)
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$width, 16)
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$height, 9)
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$units, "cm")
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$dpi, 300)
  
  # Numeric format of tables
  expect_error(setDefaultNumericFormat(digits = "a"))
  expect_error(setDefaultNumericFormat(scientific = "a"))
  setDefaultNumericFormat(digits = 2, scientific = TRUE)
  expect_equal(ospsuite.reportingengine:::reEnv$formatNumericsDigits, 2)
  expect_true(ospsuite.reportingengine:::reEnv$formatNumericsScientific)
  
  # stairstep option for plots
  expect_error(setDefaultStairstep("a"))
  setDefaultStairstep(TRUE)
  expect_true(ospsuite.reportingengine:::reEnv$defaultStairstep)
  
  # bins option for plots
  expect_error(setDefaultBins("a"))
  setDefaultBins(13)
  expect_equal(ospsuite.reportingengine:::reEnv$defaultBins, 13)
  
  # auto axis limit margin option for plots
  expect_error(setDefaultAutoAxisLimitMargin("a"))
  setDefaultAutoAxisLimitMargin(0.1)
  expect_equal(ospsuite.reportingengine:::reEnv$autoAxisLimitMargin, 0.1)
})


test_that("Plot dimensions do not use pixels to prevent compatibility issues with {ggplot2}", {
  setDefaultPlotFormat(width = 600, height = 400, units = "px", dpi = 300)
  # Note that size is not tested as it will depend on one's screen properties
  expect_equal(ospsuite.reportingengine:::reEnv$defaultPlotFormat$units, "in")
})

test_that("Template theme defining default plot properties can be loaded without errors", {
  expect_error(setDefaultThemeFromJson("wrong.json"))
  expect_silent(setDefaultThemeFromJson(system.file("extdata", "re-theme.json", package = "ospsuite.reportingengine")))
})


# Remove error logs
unlink("log-debug.txt", recursive = TRUE)
unlink("log-error.txt", recursive = TRUE)
unlink("log-info.txt", recursive = TRUE)