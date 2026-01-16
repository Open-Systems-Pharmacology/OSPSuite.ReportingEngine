# getComparisonHistogramResults

Get Comparison Histogram results for Pediatric workflows

## Usage

``` r
getComparisonHistogramResults(
  demographyPaths,
  simulationSetNames,
  data,
  metaData,
  observedData,
  settings = NULL,
  simulationSetDescriptor = "",
  demographyResults = list()
)
```

## Arguments

- demographyPaths:

  Names of demography variables to be displayed

- simulationSetNames:

  Names of simulation sets

- data:

  A data.frame of simulated demography values across the simulationSets

- metaData:

  A list of meta data indicating the display properties of the data

- observedData:

  A data.frame of observed demography values across the simulationSets

- settings:

  A list of plot settings

- simulationSetDescriptor:

  Character describing the population sets within the report

- demographyResults:

  A list of `TaskResults` objects

## Value

A list of `TaskResults` objects
