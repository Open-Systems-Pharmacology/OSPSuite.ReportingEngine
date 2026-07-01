# getDictionaryVariable

Get the variable name from dictionary

## Usage

``` r
getDictionaryVariable(
  dictionary,
  variableID,
  idColumn = dictionaryParameters$ID,
  datasetColumn = dictionaryParameters$datasetColumn
)
```

## Arguments

- dictionary:

  A data.frame from dictionary

- variableID:

  An identifier

- idColumn:

  The column name used for identification

- datasetColumn:

  The column name used mapping the id to variable

## Value

A variable name from dictionary
