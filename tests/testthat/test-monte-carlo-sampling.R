# Define input data
# Note that population size needs to be big enough 
# for approximation to be close to true known value 
# Tolerance of method is currently set to 5%
popSize <- 1e3
approximationTolerance <- 0.05
referenceParams <- list(meanlog = 0, sdlog = 1)
comparisonParams <- list(meanlog = 1, sdlog = 2)
set.seed(1111)

referenceData <- data.frame(
  simulationSetName = "reference",
  Value = rlnorm(
    n = popSize, 
    meanlog = referenceParams$meanlog, 
    sdlog = referenceParams$sdlog
    )
)

comparisonData <- data.frame(
  simulationSetName = "comparison",
  Value = rlnorm(
    n = popSize, 
    meanlog = comparisonParams$meanlog, 
    sdlog = comparisonParams$sdlog
  )
)

dataMapping <- tlf::BoxWhiskerDataMapping$new(
  x = "simulationSetName",
  y = "Value"
)

# Expected values of ratio geomean and sd
ratioGeoMean <- exp(comparisonParams$meanlog-referenceParams$meanlog)
ratioGeoSD <- exp(sqrt((comparisonParams$sdlog)^2+(referenceParams$sdlog)^2))

test_that("Analytical Solution is close to known solution", {
  # Get analytical solution from simulated distribution
  analyticalSolution <- ospsuite.reportingengine:::getRatioMeasureAnalyticalSolution(
    x = referenceData$Value,
    y = comparisonData$Value
  )
  
  expect_equal(
    analyticalSolution$geoMean,
    ratioGeoMean,
    tolerance = approximationTolerance
    )
  expect_equal(
    analyticalSolution$geoSD,
    ratioGeoSD,
    tolerance = approximationTolerance
  )
})

test_that("Monte Carlo Solution is close to known solution", {
  # Get Monte Carlo solution from simulated distribution
  mcSolution <- ospsuite.reportingengine:::getPKParameterRatioMeasureFromMCSampling(
    comparisonData = comparisonData,
    referenceData = referenceData,
    dataMapping = dataMapping
    )
  
  expect_equal(
    mcSolution$`geo mean`,
    ratioGeoMean,
    tolerance = approximationTolerance
  )
  expect_equal(
    mcSolution$`geo standard deviation`,
    ratioGeoSD,
    tolerance = approximationTolerance
  )
})

test_that("Monte Carlo Solution is repeatable", {
  # Get Monte Carlo solution from simulated distribution
  mcSolution1 <- ospsuite.reportingengine:::getPKParameterRatioMeasureFromMCSampling(
    comparisonData = comparisonData,
    referenceData = referenceData,
    dataMapping = dataMapping,
    settings = list(mcRepetitions = 100, mcRandomSeed = 3333)
  )
  mcSolution2 <- ospsuite.reportingengine:::getPKParameterRatioMeasureFromMCSampling(
    comparisonData = comparisonData,
    referenceData = referenceData,
    dataMapping = dataMapping,
    settings = list(mcRepetitions = 100, mcRandomSeed = 3333)
  )
  
  expect_equal(mcSolution1, mcSolution2)
})
