# getObservedErrorValues

Get the observed data error range to display on time profile plots

## Usage

``` r
getObservedErrorValues(
  observedValues,
  observedResults,
  axesProperties,
  molWeight = NA
)
```

## Arguments

- observedValues:

  Numeric values of observed data

- observedResults:

  A named list, including `data` and `metaData`, of observed results.

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- molWeight:

  Molecular weight if unit conversion is required

## Value

A named list, with `ymin` and `ymax`, of the observed data error range
