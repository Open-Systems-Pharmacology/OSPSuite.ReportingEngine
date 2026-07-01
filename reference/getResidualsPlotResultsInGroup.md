# getResidualsPlotResultsInGroup

Get plots and their captions for residuals

## Usage

``` r
getResidualsPlotResultsInGroup(
  data,
  metaData,
  outputId,
  structureSet = NULL,
  settings = NULL
)
```

## Arguments

- data:

  A data.frame of residuals data

- metaData:

  metaData represented as a data.frame

- outputId:

  Output identifier to provide unique id name

- structureSet:

  A `SimulationStructure` object or `NULL` if performing residuals
  across simulations

- settings:

  Optional settings for the plots. In particular, includes reference
  data for population time profile.

## Value

List of `plots`, their `captions` and `data` to export
