# StandardSimulationTasks

Names of simulation tasks performed by both `MeanModelWorkflow` and
`PopulationWorkflow` objects

## Usage

``` r
StandardSimulationTasks
```

## Format

An object of class `list` of length 3.

## See also

Other enum helpers:
[`AllAvailableTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/AllAvailableTasks.md),
[`ApplicationRanges`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ApplicationRanges.md),
[`DataSelectionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/DataSelectionKeys.md),
[`PopulationWorkflowTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/PopulationWorkflowTypes.md),
[`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ResidualScales.md),
[`StandardPlotTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/StandardPlotTasks.md),
[`StatisticsTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/StatisticsTypes.md),
[`reSettingsNames`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/reSettingsNames.md)

## Examples

``` r

# Lists all available standard simulation task names available in both mean and population workflows
StandardSimulationTasks
#> $simulate
#> [1] "simulate"
#> 
#> $calculatePKParameters
#> [1] "calculatePKParameters"
#> 
#> $calculateSensitivity
#> [1] "calculateSensitivity"
#> 
```
