# getSimulationSetContent

Creates a character vector to be written in a workflow .R script
defining `SimulationSet` objects.

## Usage

``` r
getSimulationSetContent(excelFile, simulationTable, workflowMode)
```

## Arguments

- excelFile:

  name of the Excel file from which the R script is created

- simulationTable:

  Data.frame read from the Excel sheet "SimulationSets

- workflowMode:

  Either `PopulationWorkflow` or `MeanModelWorkflow`

## Value

Character vector defining the `SimulationSet` objects
