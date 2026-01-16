# runSensitivity

Determine whether to run SA for individual or population. If for
individual, pass simulation to individualSensitivityAnalysis. If SA is
for population, loop thru population file, extract parameters for each
individual, and pass them to individualSensitivityAnalysis.

## Usage

``` r
runSensitivity(
  structureSet,
  settings,
  individualId = NULL,
  resultsFileName = NULL
)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

- settings:

  list of settings for the sensitivity analysis

- individualId:

  ID of individual in population data file for whom to perform
  sensitivity analysis

- resultsFileName:

  root name of population sensitivity analysis results CSV files

## Value

SA results for individual or population
