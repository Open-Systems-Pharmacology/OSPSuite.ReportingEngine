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
[`AllAvailableTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/AllAvailableTasks.md),
[`ApplicationRanges`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ApplicationRanges.md),
[`DataSelectionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/DataSelectionKeys.md),
[`PopulationWorkflowTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/PopulationWorkflowTypes.md),
[`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ResidualScales.md),
[`StandardSimulationTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/StandardSimulationTasks.md),
[`StatisticsTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/StatisticsTypes.md),
[`reSettingsNames`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/reSettingsNames.md)

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
