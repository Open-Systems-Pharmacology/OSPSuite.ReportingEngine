# plotDemographyHistogram

Plot histograms for demography parameters

## Usage

``` r
plotDemographyHistogram(
  data,
  metaData,
  dataMapping = NULL,
  plotConfiguration = NULL,
  bins = AggregationConfiguration$bins,
  dodge = TRUE
)
```

## Arguments

- data:

  data.frame

- metaData:

  list of metaData about `data`

- dataMapping:

  `HistogramDataMapping` class object

- plotConfiguration:

  `PlotConfiguration` class object

- bins:

  Number of bins for continuous demography parameters

- dodge:

  For continuous demography parameters, Logical defining if histogram
  bars should dodge for continuous parameters

## Value

ggplot object
