# getSimulationTimeRanges

Get time ranges for time profile plots according to applications and
user defined settings

## Usage

``` r
getSimulationTimeRanges(simulation, path, simulationSet)
```

## Arguments

- simulation:

  A `Simulation` object

- path:

  Field `path` from `Output` object

- simulationSet:

  A `SimulationSet` or `PopulationSimulationSet` object

## Value

Lists including `values` and `name` of time ranges. Also includes
logical field `keep` to define if a specific application range is kept
in report.
