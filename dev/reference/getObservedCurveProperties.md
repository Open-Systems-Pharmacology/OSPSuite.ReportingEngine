# getObservedCurveProperties

Get Curve Properties and values of Observed data

## Usage

``` r
getObservedCurveProperties(
  configurationPlanCurve,
  simulation,
  axesProperties,
  configurationPlan
)
```

## Arguments

- configurationPlanCurve:

  `Curves` fields of configuration plan

- simulation:

  A `Simulation` object from `ospsuite` package that includes required
  information to identify and convert the data requested from
  `configurationPlanCurve` properties

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- configurationPlan:

  A `ConfigurationPlan` object that includes methods to find observed
  data

## Value

A named list data and meta data parameters
