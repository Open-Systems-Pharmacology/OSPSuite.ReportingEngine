# simulateModelParallel

Simulate models within a list of structure sets in parallel for an
individual.

## Usage

``` r
simulateModelParallel(structureSets, settings = NULL)
```

## Arguments

- structureSets, :

  a list of `SimulationStructure` R6 class objects contain paths of
  files to be used

- settings:

  list of options to be passed to the function

## Value

List of simulation results for each simulation set
