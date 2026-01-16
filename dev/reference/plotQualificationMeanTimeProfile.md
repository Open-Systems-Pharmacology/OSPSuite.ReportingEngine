# plotQualificationMeanTimeProfile

Plot mean time profile for qualification workflow

## Usage

``` r
plotQualificationMeanTimeProfile(
  configurationPlanCurves,
  simulation,
  simulationResults,
  axesProperties,
  configurationPlan,
  plotConfiguration
)
```

## Arguments

- configurationPlanCurves:

  `Curves` fields of configuration plan

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

- plotConfiguration:

  A `TimeProfilePlotConfiguration` object

## Value

Mean time profile plot as a `ggplot` object
