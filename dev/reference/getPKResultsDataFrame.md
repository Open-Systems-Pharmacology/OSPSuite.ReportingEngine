# getPKResultsDataFrame

Read PK parameter results into a dataframe and set
QuantityPath,Parameter and Unit columns as factors

## Usage

``` r
getPKResultsDataFrame(structureSet)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

## Value

pkResultsDataFrame, a dataframe storing the contents of the CSV file
with path pkParameterResultsFilePath
