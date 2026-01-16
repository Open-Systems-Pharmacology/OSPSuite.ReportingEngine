# getPKParametersContent

Creates a character vector to be written in a workflow .R script
updating the PKParameters objects

## Usage

``` r
getPKParametersContent(pkParametersTable)
```

## Arguments

- pkParametersTable:

  Data.frame read from the Excel sheet "PK Parameters"

## Value

A list of script content, associated with its potential warnings and
errors for updating the PKParameters objects
