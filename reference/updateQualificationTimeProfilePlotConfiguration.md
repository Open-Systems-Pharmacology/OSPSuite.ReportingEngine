# updateQualificationTimeProfilePlotConfiguration

Update TimeProfilePlotConfiguration properties based on meta data and
requested axes

## Usage

``` r
updateQualificationTimeProfilePlotConfiguration(
  simulatedMetaData = NULL,
  observedMetaData = NULL,
  requestedAxes = "Y",
  axesProperties = NULL,
  plotConfiguration
)
```

## Arguments

- simulatedMetaData:

  List of meta data on simulated data obtained from
  `getSimulatedCurveProperties`

- observedMetaData:

  List of meta data on observed data obtained from
  `getObservedCurveProperties`

- requestedAxes:

  Array of requested axes included in `"Y"`, `"Y2"` and `"Y3"`

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- plotConfiguration:

  A `PlotConfiguration` object

## Value

Updated `TimeProfilePlotConfiguration` object
