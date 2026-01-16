# runParallelSensitivityAnalysis

Spawn cores, divide parameters among cores, run sensitivity analysis on
cores for a single individual, save results as CSV.

## Usage

``` r
runParallelSensitivityAnalysis(
  structureSet,
  settings = settings,
  individualParameters
)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

- settings:

  list of settings for the sensitivity analysis

- individualParameters:

  is an object storing an individual's parameters, obtained from a
  population object's \`getParameterValuesForIndividual()“ function.

## Value

SA results for population
