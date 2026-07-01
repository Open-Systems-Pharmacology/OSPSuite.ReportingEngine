# getFileLocationFromType

Get the right path text and copy the sheet content as a file if type is
SHEET

## Usage

``` r
getFileLocationFromType(location, type, excelFile)
```

## Arguments

- location:

  Path of the file if type is FILE or sheetname if type is SHEET

- type:

  Location type: either "SHEET" or "FILE"

- excelFile:

  name of the Excel file from which the R script is created

## Value

Character of location to provide
