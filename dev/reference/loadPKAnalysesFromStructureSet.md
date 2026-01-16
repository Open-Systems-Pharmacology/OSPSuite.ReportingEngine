# loadPKAnalysesFromStructureSet

loadPKAnalysesFromStructureSet

## Usage

``` r
loadPKAnalysesFromStructureSet(
  structureSet,
  to = "PKAnalyses",
  useCache = FALSE
)
```

## Arguments

- structureSet:

  A `SimulationStructure` object

- to:

  Format of the loaded output\`

## Value

A `PKAnalyses` object if `to="PKAnalyses"` A `data.frame` if
`to="data.frame"` A `tibble` if `to="tibble"`
