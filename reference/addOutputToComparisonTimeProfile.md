# addOutputToComparisonTimeProfile

Add plot layers for an output mapping from comparison time profile plot

## Usage

``` r
addOutputToComparisonTimeProfile(
  outputMapping,
  simulationDuration,
  axesProperties,
  plotObject,
  configurationPlan
)
```

## Arguments

- outputMapping:

  list of mapping elements from `OutputMappings` field in configuration
  plan

- simulationDuration:

  Duration of simulation in X axis unit

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- plotObject:

  ggplot object

- configurationPlan:

  A `ConfigurationPlan` object

## Value

A ggplot object
