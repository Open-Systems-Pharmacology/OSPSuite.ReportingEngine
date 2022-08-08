# Since the function is not exported, 'ospsuite.reportingengine:::' is required
# To simplify the tests, the names of the non-exported functions are re-assigned as a first step
getSmartZoomLimits <- ospsuite.reportingengine:::getSmartZoomLimits

test_that("'getSmartZoomLimits' works as expected", {
  # If min >= 1 , zoom min = 0.8
  expect_equal(getSmartZoomLimits(10)$min, 0.8)
  expect_equal(getSmartZoomLimits(1.1)$min, 0.8)
  # If min < 1 , zoom min = min between 0.25 and 0.8*min
  expect_equal(getSmartZoomLimits(0.1)$min, 0.08)
  expect_equal(getSmartZoomLimits(0.9)$min, 0.25)
  # If max < 1 , zoom max = 1.25
  expect_equal(getSmartZoomLimits(0.1)$max, 1.25)
  expect_equal(getSmartZoomLimits(0.9)$max, 1.25)
  # If max >= 1 , zoom max = max between 4 and max
  expect_equal(getSmartZoomLimits(10)$max, 12.5)
  expect_equal(getSmartZoomLimits(1.1)$max, 4)
})

test_that("'getSmartZoomLimits' handles NAs", {
  expect_false(is.na(getSmartZoomLimits(c(1, 2, 3, NA))$min))
  expect_false(is.na(getSmartZoomLimits(c(1, 2, 3, NA))$max))
})

test_that("'getSmartZoomLimits' always includes the 2-fold error range for residuals vs observed options", {
  expect_lt(getSmartZoomLimits(10, residualsVsObserved = TRUE)$min, 0.5)
  expect_lt(getSmartZoomLimits(1.1, residualsVsObserved = TRUE)$min, 0.5)
  expect_lt(getSmartZoomLimits(0.1, residualsVsObserved = TRUE)$min, 0.5)
  expect_lt(getSmartZoomLimits(0.9, residualsVsObserved = TRUE)$min, 0.5)

  expect_gt(getSmartZoomLimits(0.1, residualsVsObserved = TRUE)$max, 2)
  expect_gt(getSmartZoomLimits(0.9, residualsVsObserved = TRUE)$max, 2)
  expect_gt(getSmartZoomLimits(10, residualsVsObserved = TRUE)$max, 2)
  expect_gt(getSmartZoomLimits(1.1, residualsVsObserved = TRUE)$max, 2)
})
