# plotAbsorptionTimeProfile

Plot absorption time profile

## Usage

``` r
plotAbsorptionTimeProfile(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL
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

ggplot object of time profile for mean model workflow
