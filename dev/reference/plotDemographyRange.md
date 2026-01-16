# plotDemographyRange

Plot demography range plot using visual predictive check style if data
is numeric, or using box whisker plot if data is categorical

## Usage

``` r
plotDemographyRange(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  parameterClass = "numeric"
)
```

## Arguments

- data:

  data.frame

- metaData:

  list of metaData about `data`

- dataMapping:

  A `TimeProfileDataMapping` or `BoxWhiskerDataMapping` object

- plotConfiguration:

  `PlotConfiguration` object

- parameterClass:

  Class of the parameter, either "numeric" or "character"

## Value

ggplot object
