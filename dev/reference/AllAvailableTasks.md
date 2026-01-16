# AllAvailableTasks

Names of all existing tasks that can be performed by `MeanModelWorkflow`
or `PopulationWorkflow` objects

## Usage

``` r
AllAvailableTasks
```

## Format

An object of class `list` of length 9.

## See also

Other enum helpers:
[`ApplicationRanges`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ApplicationRanges.md),
[`DataSelectionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/DataSelectionKeys.md),
[`PopulationWorkflowTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationWorkflowTypes.md),
[`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ResidualScales.md),
[`StandardPlotTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StandardPlotTasks.md),
[`StandardSimulationTasks`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StandardSimulationTasks.md),
[`StatisticsTypes`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/StatisticsTypes.md),
[`reSettingsNames`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/reSettingsNames.md)

## Examples

``` r
# Lists all available task names available in mean or population workflows
AllAvailableTasks
#> $simulate
#> [1] "simulate"
#> 
#> $calculatePKParameters
#> [1] "calculatePKParameters"
#> 
#> $calculateSensitivity
#> [1] "calculateSensitivity"
#> 
#> $plotTimeProfilesAndResiduals
#> [1] "plotTimeProfilesAndResiduals"
#> 
#> $plotPKParameters
#> [1] "plotPKParameters"
#> 
#> $plotSensitivity
#> [1] "plotSensitivity"
#> 
#> $plotDemography
#> [1] "plotDemography"
#> 
#> $plotAbsorption
#> [1] "plotAbsorption"
#> 
#> $plotMassBalance
#> [1] "plotMassBalance"
#> 
```
