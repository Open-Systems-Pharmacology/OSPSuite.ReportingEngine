# individualSensitivityAnalysis

Run SA for an individual, possibly after modifying the simulation using
individualParameters. Determine whether to run SA for on single core or
in parallel. If on single core, pass simulation to analyzeSensitivity.
If in parallel, pass simulation to runParallelSensitivityAnalysis.

## Usage

``` r
individualSensitivityAnalysis(structureSet, settings, individualParameters)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

- settings:

  list of settings for the sensitivity analysis

- individualParameters:

  is an object storing an individual's parameters, obtained from a
  population object's getParameterValuesForIndividual() function.

## Value

SA results for an individual
