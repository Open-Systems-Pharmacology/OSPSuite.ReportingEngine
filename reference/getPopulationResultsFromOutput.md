# getPopulationResultsFromOutput

Get simulated population data from an Output object

## Usage

``` r
getPopulationResultsFromOutput(
  simulationPathResults,
  output,
  simulationQuantity,
  molWeight,
  structureSet,
  settings = NULL
)
```

## Arguments

- simulationPathResults:

  list with simulated data included

- output:

  An `Output` object

- simulationQuantity:

  Dimension/quantity for unit conversion of dependent variable

- molWeight:

  Molar weight for unit conversion of dependent variable

- structureSet:

  `SimulationStructure` object

- settings:

  TaskSetting object

## Value

list of data and metaData
