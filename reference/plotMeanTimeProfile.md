# plotMeanTimeProfile

Plot time profile for mean model workflow

## Usage

``` r
plotMeanTimeProfile(
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

  A list to be integrated to `tlf` DataMapping objects

- plotConfiguration:

  `PlotConfiguration` class object from `tlf` library

## Value

ggplot object of time profile for mean model workflow
