# ApplicationRanges

Keys of reported ranges when simulation includes multiple applications

## Usage

``` r
ApplicationRanges
```

## Format

An object of class `list` of length 3.

## See also

Other enum helpers:
[`AllAvailableTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/AllAvailableTasks.md),
[`DataSelectionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/DataSelectionKeys.md),
[`PopulationWorkflowTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationWorkflowTypes.md),
[`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ResidualScales.md),
[`StandardPlotTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StandardPlotTasks.md),
[`StandardSimulationTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StandardSimulationTasks.md),
[`StatisticsTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StatisticsTypes.md),
[`reSettingsNames`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/reSettingsNames.md)

## Examples

``` r
# Lists available Application Ranges
ApplicationRanges
#> $total
#> [1] "total"
#> 
#> $firstApplication
#> [1] "firstApplication"
#> 
#> $lastApplication
#> [1] "lastApplication"
#> 
```
