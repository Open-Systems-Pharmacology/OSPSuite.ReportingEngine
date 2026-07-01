# getOutputsContent

Creates a character vector to be written in a workflow .R script
defining `Output` object.

## Usage

``` r
getOutputsContent(excelFile, outputsTable, simulationOutputs)
```

## Arguments

- excelFile:

  name of the Excel file from which the R script is created

- outputsTable:

  Data.frame read from the Excel sheet "Outputs"

- simulationOutputs:

  Names of Output objects used by simulation sets

## Value

Character vector defining the `Output` object
