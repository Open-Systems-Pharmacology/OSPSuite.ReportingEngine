# updatePlotConfigurationTimeTicks

Update time ticks based on selected time unit in `PlotConfiguration`
objects

## Usage

``` r
updatePlotConfigurationTimeTicks(
  data,
  metaData,
  dataMapping,
  plotConfiguration
)
```

## Arguments

- data:

  data.frame

- metaData:

  meta data on `data`

- dataMapping:

  `XYGDataMapping` R6 class object from `tlf` library

- plotConfiguration:

  `PlotConfiguration` R6 class object from `tlf` library

## Value

A `PlotConfiguration` object
