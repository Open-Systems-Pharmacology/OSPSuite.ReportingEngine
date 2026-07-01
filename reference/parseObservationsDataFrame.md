# parseObservationsDataFrame

Function to read the variable names and units in the first two columns
of an observations data frame used for qualification. First column
stores timepoints. Second column stores measurements of a quantity
corresponding to timepoints in first column.

## Usage

``` r
parseObservationsDataFrame(observationsDataFrame)
```

## Arguments

- observationsDataFrame:

  from CSV

## Value

A named list with `time` and `output` fields. Each field contains a list
that is output by `extractNameAndUnit` with fields that store the
variable name and the unit.
