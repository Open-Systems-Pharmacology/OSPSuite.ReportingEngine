# getSimulatedResultsFromOutput

Get simulated data from an Output object

## Usage

``` r
getSimulatedResultsFromOutput(
  simulationPathResults,
  output,
  simulationQuantity,
  molWeight,
  structureSet
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

## Value

list of data and metaData
