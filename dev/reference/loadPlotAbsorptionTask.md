# loadPlotAbsorptionTask

Define `plotAbsorption` task and its settings

## Usage

``` r
loadPlotAbsorptionTask(workflow, active = FALSE, settings = NULL)
```

## Arguments

- workflow:

  `MeanModelWorkflow` object or derived class

- active:

  logical defining if `Task` will be run by workflow. Default value is
  `FALSE`

- settings:

  specific settings for `plotAbsorption` task

## Value

A `PlotTask` object

## See also

Other workflow tasks:
[`GofPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/GofPlotTask.md),
[`PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md),
[`PopulationPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationPlotTask.md),
[`PopulationSensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationSensitivityAnalysisTask.md),
[`QualificationTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationTask.md),
[`SensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SensitivityAnalysisTask.md),
[`Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md),
[`addUserDefinedTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addUserDefinedTask.md),
[`loadCalculatePKParametersTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculatePKParametersTask.md),
[`loadCalculateSensitivityTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculateSensitivityTask.md),
[`loadGOFMergedTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadGOFMergedTask.md),
[`loadPlotDDIRatioTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotDDIRatioTask.md),
[`loadPlotDemographyTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotDemographyTask.md),
[`loadPlotMassBalanceTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotMassBalanceTask.md),
[`loadPlotPKParametersTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotPKParametersTask.md),
[`loadPlotPKRatioTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotPKRatioTask.md),
[`loadPlotSensitivityTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotSensitivityTask.md),
[`loadPlotTimeProfilesAndResidualsTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotTimeProfilesAndResidualsTask.md),
[`loadQualificationComparisonTimeProfileTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationComparisonTimeProfileTask.md),
[`loadQualificationTimeProfilesTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationTimeProfilesTask.md),
[`loadSimulateTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadSimulateTask.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# A workflow object needs to be created first
myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)

# (Re)load a default plotAbsorption task for workflow
myWorkflow$plotAbsorption <- loadPlotAbsorptionTask(
  workflow = myWorkflow,
  active = TRUE
)

# Load a user-defined simulation task for workflow
myWorkflow$userDefinedTasks[["plotAbsorption"]] <- loadPlotAbsorptionTask(
  workflow = myWorkflow,
  active = TRUE
)
} # }
```
