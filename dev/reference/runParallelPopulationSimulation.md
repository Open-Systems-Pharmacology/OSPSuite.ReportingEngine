# runParallelPopulationSimulation

Spawn cores, divide population among cores, run population simulation on
cores, save results as CSV.

## Usage

``` r
runParallelPopulationSimulation(structureSet, numberOfCores, settings)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object contain paths of files to be
  used

- numberOfCores:

  number of cores do be used by the parallel simulation

- settings:

  list of options to be passed on the function

## Value

Simulation results for population
