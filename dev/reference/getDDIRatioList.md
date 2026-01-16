# getDDIRatioList

Read the entries from a DDI observations data.frame that correspond to a
particular PK parameter into a named list

## Usage

``` r
getDDIRatioList(observedDataFrameRow, ddiPKRatioColumnName)
```

## Arguments

- observedDataFrameRow:

  data.frame of DDI observations

- ddiPKRatioColumnName:

  Name of column in data.frame `observedDataFrameRow` containing the
  value of the PK parameter observation to be read

## Value

A named list containing entries in `observedDataFrameRow`corresponding
to the PK parameter in the data.frame column `ddiPKRatioColumnName`
