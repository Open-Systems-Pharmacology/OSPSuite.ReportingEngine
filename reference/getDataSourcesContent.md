# getDataSourcesContent

Creates a character vector to be written in a workflow .R script
defining `DataSource` objects.

## Usage

``` r
getDataSourcesContent(excelFile, dataSourcesTable, simulationSources)
```

## Arguments

- excelFile:

  name of the Excel file from which the R script is created

- dataSourcesTable:

  Data.frame read from the Excel sheet "DataSources"

- simulationSources:

  Names of DataSource objects used by simulation sets

## Value

Character vector defining the `DataSource` objects
