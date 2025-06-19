# Since the function is not exported, 'ospsuite.reportingengine:::' is required
# To simplify the tests, the names of the non-exported functions are re-assigned as a first step
getSmartZoomLimits <- ospsuite.reportingengine:::getSmartZoomLimits
getGuestDeltaFromConfigurationPlan <- ospsuite.reportingengine:::getGuestDeltaFromConfigurationPlan
getQualificationDDIRatioMeasure <- ospsuite.reportingengine:::getQualificationDDIRatioMeasure

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

test_that("Guest et al. values are appropriately read", {
  ddiRatioPlan1 <- list(
    PKParameters = c("AUC", "CL", "CMAX"),
    GuestDelta = list(
      list(Value = 1.2, PKParameters = c("AUC", "CL")),
      list(Value = 1.3, PKParameters = c("CMAX"))
    )
  )
  # Guest value defined but not exported in artifacts
  ddiRatioPlan2 <- list(
    PKParameters = c("AUC", "CL"),
    GuestDelta = list(
      list(Value = 1.2, PKParameters = c("AUC", "CL")),
      list(Value = 1.3, PKParameters = c("CMAX"))
    )
  )
  ddiRatioPlan3 <- list(
    PKParameters = c("AUC", "CL", "CMAX"),
    GuestDelta = list(list(Value = 1.2, PKParameters = c("AUC", "CL")))
  )
  ddiRatioPlan4 <- list(
    PKParameters = c("AUC", "CL", "CMAX"),
    GuestDelta = 1.2
  )
  ddiRatioPlan5 <- list(PKParameters = c("AUC", "CL", "CMAX"))

  expect_equal(
    getGuestDeltaFromConfigurationPlan(ddiRatioPlan1),
    list(AUC = 1.2, CL = 1.2, CMAX = 1.3)
  )
  expect_error(
    getGuestDeltaFromConfigurationPlan(ddiRatioPlan2)
  )
  expect_equal(
    getGuestDeltaFromConfigurationPlan(ddiRatioPlan3),
    list(AUC = 1.2, CL = 1.2, CMAX = 1)
  )
  expect_equal(
    getGuestDeltaFromConfigurationPlan(ddiRatioPlan4),
    list(AUC = 1.2, CL = 1.2, CMAX = 1.2)
  )
  expect_equal(
    getGuestDeltaFromConfigurationPlan(ddiRatioPlan5),
    list(AUC = 1, CL = 1, CMAX = 1)
  )
})

# value generated from rlnorm
ddiTestData <- data.frame(
  observedRatio = c(0.69, 0.69, 6.78, 1.54, 3.21, 0.46, 1.31, 4.99, 0.35, 0.85),
  simulatedRatio = c(0.32, 9.86, 1.98, 6.17, 5.54, 1.49, 0.14, 3.70, 0.77, 0.65)
)
ddiTestData$simulatedObservedRatio <- ddiTestData$simulatedRatio / ddiTestData$observedRatio

test_that("Guest et al. measure works as expected", {
  ddiMeasure <- getQualificationDDIRatioMeasure(ddiTestData, pkParameterName = "AUC", delta = 1)
  ddiMeasureNULL <- getQualificationDDIRatioMeasure(ddiTestData, pkParameterName = "AUC", delta = NULL)
  ddiMeasureGuest <- getQualificationDDIRatioMeasure(ddiTestData, pkParameterName = "AUC", delta = 1.3)

  expect_s3_class(ddiMeasure, "data.frame")
  expect_s3_class(ddiMeasureNULL, "data.frame")
  expect_s3_class(ddiMeasureGuest, "data.frame")

  expect_equal(names(ddiMeasure), c("AUC", "Number", "Ratio [%]"))
  expect_equal(names(ddiMeasureGuest), c("AUC", "Number", "Ratio [%]"))

  expect_equal(
    ddiMeasure$AUC,
    c("Points total", "Points within Guest *et al.*", "Points within 2 fold")
  )
  expect_equal(
    ddiMeasureGuest$AUC,
    c("Points total", "Points within Guest *et al.*", "Points within 2 fold")
  )

  expect_equal(ddiMeasure$Number, c(10, 1, 3))
  expect_equal(ddiMeasureGuest$Number, c(10, 3, 3))
  expect_equal(ddiMeasure$`Ratio [%]`, c(NA, 10, 30))
  expect_equal(ddiMeasureGuest$`Ratio [%]`, c(NA, 30, 30))

  # Undefined is same as delta = 1
  expect_equal(ddiMeasure, ddiMeasureNULL)
})

ddiPlan <- getTestDataFilePath("qualification/configuration-plan-ddi-ratio.json")

ospsuite::removeAllUserDefinedPKParameters()
ddiConfig <- ospsuite.reportingengine::loadConfigurationPlan(ddiPlan, "test-ddi")

test_that("getDDIOutputsDataframe generates appropriate data.frame and user-defined PK parameters", {
  ddiOutputs <- ospsuite.reportingengine:::getDDIOutputsDataframe(ddiConfig)
  expect_equal(ddiOutputs$project, rep(c("P1", "P2"), each = 2))
  expect_equal(ddiOutputs$simulation, rep(c("S1", "S2"), each = 2))
  expect_equal(ddiOutputs$pkParameter, rep(c("AUC_tEnd_tStartTime_60", "C_max_tStartTime_60"), 2))
  expect_equal(ddiOutputs$startTime, rep(60, 4))
  expect_equal(ddiOutputs$endTime, rep(NA, 4))
  expect_equal(
    ddiOutputs$outputPath,
    rep(
      c(
        "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
        "Organism|A|Concentration in container"
      ),
      each = 2
    )
  )
  expect_true(all(ddiOutputs$pkParameter %in% ospsuite::allPKParameterNames()))
})

# Use previous naming convention before ospsuite.reportingengine
ospsuite::removeAllUserDefinedPKParameters()
ddiConfig$plots$DDIRatioPlots[[1]]$PKParameters <- c("AUC", "CMAX")
test_that("Previous naming convention use AUC_inf no or infinite time used", {
  # End Time is NULL in the default values
  ddiOutputs <- ospsuite.reportingengine:::getDDIOutputsDataframe(ddiConfig)
  expect_equal(
    ddiOutputs$pkParameter,
    rep(c("AUC_inf_tStartTime_60", "C_max_tStartTime_60"), 2)
  )
  expect_true(all(ddiOutputs$pkParameter %in% ospsuite::allPKParameterNames()))
  # End Time is "Inf"
  ospsuite::removeAllUserDefinedPKParameters()
  for (groupIndex in 1:2) {
    for (ddiIndex in 1:2) {
      ddiConfig$plots$DDIRatioPlots[[1]]$Groups[[groupIndex]]$DDIRatios[[ddiIndex]]$SimulationDDI$EndTime <- "Inf"
      ddiConfig$plots$DDIRatioPlots[[1]]$Groups[[groupIndex]]$DDIRatios[[ddiIndex]]$SimulationControl$EndTime <- "Inf"
    }
  }
  ddiOutputs <- ospsuite.reportingengine:::getDDIOutputsDataframe(ddiConfig)
  expect_equal(
    ddiOutputs$pkParameter,
    rep(c("AUC_inf_tStartTime_60", "C_max_tStartTime_60"), 2)
  )
  expect_true(all(ddiOutputs$pkParameter %in% ospsuite::allPKParameterNames()))
  # End Time is 3h=180min
  ospsuite::removeAllUserDefinedPKParameters()
  for (groupIndex in 1:2) {
    for (ddiIndex in 1:2) {
      ddiConfig$plots$DDIRatioPlots[[1]]$Groups[[groupIndex]]$DDIRatios[[ddiIndex]]$SimulationDDI$EndTime <- 3
      ddiConfig$plots$DDIRatioPlots[[1]]$Groups[[groupIndex]]$DDIRatios[[ddiIndex]]$SimulationControl$EndTime <- 3
    }
  }
  ddiOutputs <- ospsuite.reportingengine:::getDDIOutputsDataframe(ddiConfig)
  expect_equal(
    ddiOutputs$pkParameter,
    rep(c("AUC_tEnd_tStartTime_60_tEndTime_180", "C_max_tStartTime_60_tEndTime_180"), 2)
  )
  expect_true(all(ddiOutputs$pkParameter %in% ospsuite::allPKParameterNames()))
})

ospsuite::removeAllUserDefinedPKParameters()

unlink("test-ddi", recursive = TRUE)