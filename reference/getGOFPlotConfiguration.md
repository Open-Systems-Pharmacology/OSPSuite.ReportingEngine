# getGOFPlotConfiguration

Define a `PlotConfiguration` object

## Usage

``` r
getGOFPlotConfiguration(
  plotType,
  group,
  data,
  metaData,
  dataMapping = NULL,
  plotConfiguration = NULL
)
```

## Arguments

- plotType:

  Plot type for residuals

- group:

  A data.frame mapping properties to output groups

- data:

  A data.frame

- metaData:

  List of metaData defining dimensions and units in the data.frame

- dataMapping:

  List `DataMapping` object

- plotConfiguration:

  A user-defined `PlotConfiguration` object

## Value

A `PlotConfiguration` object
