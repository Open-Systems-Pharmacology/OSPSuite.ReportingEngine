# getTimeProfileObservedDataFromResults

Get the time profile observed data in appropriate unit from the results
from the observed data file

## Usage

``` r
getTimeProfileObservedDataFromResults(
  observedResults,
  molWeight,
  axesProperties,
  observedDataId
)
```

## Arguments

- observedResults:

  List that includes

  - `data`: a data.frame with column 1 = Time, column 2 = Concentration,
    column 3 = Error

  - `metaData`: a list for each column of data that includes their unit

- molWeight:

  Molecular weight of compound

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- observedDataId:

  Id of the observed data for better handling error messages

## Value

List with `time`, `y` and `error` values
