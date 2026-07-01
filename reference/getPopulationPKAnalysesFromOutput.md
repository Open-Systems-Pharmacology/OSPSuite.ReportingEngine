# getPopulationPKAnalysesFromOutput

Get the values of PK parameters specified by an `Output` object from a
data.frame

## Usage

``` r
getPopulationPKAnalysesFromOutput(
  data,
  metaData,
  output,
  pkParameter,
  molWeight = NULL
)
```

## Arguments

- data:

  data.frame of the PK Analyses across Population Simulation sets

- metaData:

  metaData (dimension and unit) of the PK Analyses across Population
  Simulation sets

- output:

  An `Output ` object

- pkParameter:

  `pkParameter` from `Output ` object

- molWeight:

  Molecular weight of compound (if unit conversion needed)

## Value

list of data.frame and its metaData including the values of PK
parameters specified by `pkParameter` and `Output` objects
