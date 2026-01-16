# getMassBalanceData

Get mass balance data for a set of compounds

## Usage

``` r
getMassBalanceData(groupings, compoundNames, simulation, simulationResults)
```

## Arguments

- groupings:

  A list of grouping lists that define naming and inclusion/exclusion
  criteria

- compoundNames:

  A vector of compound names

- simulation:

  A `Simulation` object

- simulationResults:

  A `SimulationResults` object

## Value

A data.frame that includes `Time`, `Amount` and `Legend` as variables
