# loadSimulationWithUpdatedPaths

loadSimulationWithUpdatedPaths

## Usage

``` r
loadSimulationWithUpdatedPaths(
  simulationSet,
  loadFromCache = FALSE,
  addToCache = TRUE
)
```

## Arguments

- simulationSet:

  simulation set containing path to simulation file and pathIDs for
  quantities to be loaded into simulation object

- loadFromCache:

  If `TRUE`, an already loaded pkml file will not be loaded again, but
  the `Simulation` object will be retrieved from cache. If `FALSE`, a
  new `Simulation` object will be created. Default value is `FALSE`.

- addToCache:

  If `TRUE`, the loaded simulation is added to cache. If `FALSE`, the
  returned simulation only exists locally. Default is `TRUE`.

## Value

A `Simulation` object with pathIDs updated from simulationSet
