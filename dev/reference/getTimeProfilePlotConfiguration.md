# getTimeProfilePlotConfiguration

Define a `TimeProfilePlotConfiguration` object

## Usage

``` r
getTimeProfilePlotConfiguration(
  workflowType,
  group,
  data,
  metaData,
  observedData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL
)
```

## Arguments

- workflowType:

  Workflow type, either `"mean"` or `"population"`

- group:

  A data.frame mapping properties to output groups

- data:

  A data.frame

- metaData:

  List of metaData defining dimensions and units in the data.frame

- dataMapping:

  List mapping x, y and color variables to `data`

- plotConfiguration:

  A user-defined `TimeProfilePlotConfiguration` object

## Value

A `TimeProfilePlotConfiguration` object
