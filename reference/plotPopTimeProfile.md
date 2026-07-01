# plotPopTimeProfile

Plot time profile for population model workflow

## Usage

``` r
plotPopTimeProfile(
  simulatedData,
  observedData = NULL,
  dataMapping = NULL,
  metaData = NULL,
  plotConfiguration = NULL
)
```

## Arguments

- simulatedData:

  data.frame of simulated data

- observedData:

  data.frame of observed data

- dataMapping:

  A list to be integrated to `tlf` DataMapping objects

- metaData:

  meta data on `data`

- plotConfiguration:

  `TimeProfilePlotConfiguration` R6 class object from `tlf` library

## Value

ggplot object of time profile for mean model workflow
