# getResidualsPlotResults

Get plots and their captions for residuals

## Usage

``` r
getResidualsPlotResults(
  timeRange,
  residualsData,
  metaDataFrame,
  structureSet,
  settings = NULL
)
```

## Arguments

- timeRange:

  array of time values defining range of simulated data

- residualsData:

  data.frame of residuals data

- metaDataFrame:

  metaData represented as a data.frame

- structureSet:

  A `SimulationStructure` object

- settings:

  Optional settings for the plots. In particular, includes reference
  data for population time profile.

## Value

List of `plots`, their `captions` and `data` to export
