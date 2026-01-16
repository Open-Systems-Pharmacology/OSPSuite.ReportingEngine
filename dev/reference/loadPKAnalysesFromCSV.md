# loadPKAnalysesFromCSV

Load PK analyses from a CSV file Wrap the
[`ospsuite::importPKAnalysesFromCSV()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/importPKAnalysesFromCSV.html)
to provide more useful warning messages.

## Usage

``` r
loadPKAnalysesFromCSV(filePath, simulation, to = "PKAnalyses")
```

## Arguments

- filePath:

  Full path of the file containing the PK-Analyses to load

- simulation:

  A `Simulation` object

- to:

  Format of the loaded output\`

## Value

A `PKAnalyses` object if `to="PKAnalyses"` A `data.frame` if
`to="data.frame"` A `tibble` if `to="tibble"`
