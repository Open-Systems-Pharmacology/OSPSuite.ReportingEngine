# simulateModelOnCore

Simulate model, either for an individual or for a given population.

## Usage

``` r
simulateModelOnCore(
  simulation,
  population,
  debugLogFileName = file.path(getwd(), defaultFileNames$logDebugFile()),
  nodeName = NULL,
  showProgress = FALSE
)
```

## Arguments

- simulation:

  A `Simulation` object

- population:

  A `Population` object

- debugLogFileName:

  path to file where core debug logs are saved

- nodeName:

  node name for parallel simulations

- showProgress:

  option to print progress of simulation to console
