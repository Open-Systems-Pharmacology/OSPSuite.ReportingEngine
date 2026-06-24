# StandardPlotTasks

Names of plot tasks performed by both `MeanModelWorkflow` and
`PopulationWorkflow` objects

## Usage

``` r
StandardPlotTasks
```

## Format

An object of class `list` of length 3.

## See also

Other enum helpers:
[`AllAvailableTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/AllAvailableTasks.md),
[`ApplicationRanges`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ApplicationRanges.md),
[`DataSelectionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/DataSelectionKeys.md),
[`PopulationWorkflowTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationWorkflowTypes.md),
[`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ResidualScales.md),
[`StandardSimulationTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StandardSimulationTasks.md),
[`StatisticsTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StatisticsTypes.md),
[`reSettingsNames`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/reSettingsNames.md)

## Examples

``` r

# Lists all available standard plot task names available in both mean and population workflows
StandardPlotTasks
#> $plotTimeProfilesAndResiduals
#> [1] "plotTimeProfilesAndResiduals"
#> 
#> $plotPKParameters
#> [1] "plotPKParameters"
#> 
#> $plotSensitivity
#> [1] "plotSensitivity"
#> 
```
