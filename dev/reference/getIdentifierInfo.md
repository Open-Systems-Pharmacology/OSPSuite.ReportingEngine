# getIdentifierInfo

Get and format the information from a data.frame matching a certain
`simulationIndex` column and `codeId` line

## Usage

``` r
getIdentifierInfo(workflowTable, simulationIndex, codeId)
```

## Arguments

- workflowTable:

  Data.frame read from one of the available Excel sheets

- simulationIndex:

  Column to read after removing "Code Identifier" and "Description"

- codeId:

  Line to read in the data.frame corresponding to a specific value of
  "Code Identifier"

## Value

Information from a data.frame matching a certain `simulationIndex`
column and `codeId` line
