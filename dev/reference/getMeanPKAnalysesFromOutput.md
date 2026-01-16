# getMeanPKAnalysesFromOutput

Get PK analyses from an `Output` object

## Usage

``` r
getMeanPKAnalysesFromOutput(data, output, molWeight = NULL)
```

## Arguments

- data:

  A data.frame of PK Analyses

- output:

  An `Output` object defining `pkParameters`

- molWeight:

  Molecular weight for converting into PK Parameter `displayUnit`

## Value

A data.frame with `Path`, `Parameter`, `Value` and `Unit` to display in
final report
