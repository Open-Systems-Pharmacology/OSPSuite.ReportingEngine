# plotPopTimeProfileLog

Plot time profile for mean model workflow

## Usage

``` r
plotPopTimeProfileLog(
  simulatedData,
  observedData = NULL,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL
)
```

## Arguments

- simulatedData:

  data.frame of simulated data

- observedData:

  data.frame of observed data

- metaData:

  meta data on `data`

- dataMapping:

  A list mapping `x`, `y` and `group` from datasets

- plotConfiguration:

  `PlotConfiguration` class object from `tlf` library

## Value

ggplot object of time profile for mean model workflow
