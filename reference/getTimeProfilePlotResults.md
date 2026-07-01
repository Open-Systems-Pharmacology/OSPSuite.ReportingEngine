# getTimeProfilePlotResults

Get plots and their captions for mean or population time profiles

## Usage

``` r
getTimeProfilePlotResults(
  workflowType,
  timeRange,
  simulatedData,
  observedData = NULL,
  metaDataFrame,
  timeProfileMapping,
  structureSet,
  settings = NULL
)
```

## Arguments

- workflowType:

  `"mean"` or `"population"` workflow

- timeRange:

  array of time values defining range of simulated data. Note that
  `timeRange` accounts for user defined `timeOffset`

- simulatedData:

  data.frame of simulated data

- observedData:

  data.frame of observed data

- metaDataFrame:

  metaData represented as a data.frame

- timeProfileMapping:

  list of variables to map in the time profile plots

- structureSet:

  A `SimulationStructure` object

- settings:

  Optional settings for the plots. In particular, includes reference
  data for population time profile.

## Value

List of `plots` and their `captions`
