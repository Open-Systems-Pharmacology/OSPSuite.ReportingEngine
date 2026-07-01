# plotQualificationPopulationTimeProfile

Plot population time profile for qualification workflow

## Usage

``` r
plotQualificationPopulationTimeProfile(
  simulationAnalysis,
  observedDataCollection,
  simulation,
  simulationResults,
  axesProperties,
  configurationPlan,
  plotObject
)
```

## Arguments

- simulationAnalysis:

  Field `Analysis` from `ConfigurationPlan` population time profile plot

- observedDataCollection:

  Field `ObservedDataCollection` from `ConfigurationPlan` population
  time profile plot

- simulation:

  A `Simulation` object from `ospsuite` package that includes required
  information to identify and convert the data requested from
  `configurationPlanCurve` properties

- simulationResults:

  A `SimulationResults` object from `ospsuite` package that includes the
  data requested from `configurationPlanCurve` properties

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- configurationPlan:

  A `ConfigurationPlan` object that includes methods to find observed
  data

- plotObject:

  A `ggplot` object

## Value

Population time profile plot as a `ggplot` object
