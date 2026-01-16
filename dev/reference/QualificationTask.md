# QualificationTask

R6 class for QualificationTask settings

## See also

Other workflow tasks:
[`GofPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/GofPlotTask.md),
[`PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md),
[`PopulationPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationPlotTask.md),
[`PopulationSensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationSensitivityAnalysisTask.md),
[`SensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SensitivityAnalysisTask.md),
[`Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md),
[`addUserDefinedTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addUserDefinedTask.md),
[`loadCalculatePKParametersTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculatePKParametersTask.md),
[`loadCalculateSensitivityTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculateSensitivityTask.md),
[`loadGOFMergedTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadGOFMergedTask.md),
[`loadPlotAbsorptionTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotAbsorptionTask.md),
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

## Super classes

[`ospsuite.reportingengine::Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md)
-\>
[`ospsuite.reportingengine::PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md)
-\> `QualificationTask`

## Methods

### Public methods

- [`QualificationTask$saveResults()`](#method-QualificationTask-saveResults)

- [`QualificationTask$runTask()`](#method-QualificationTask-runTask)

- [`QualificationTask$clone()`](#method-QualificationTask-clone)

Inherited methods

- [`ospsuite.reportingengine::Task$activate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-activate)
- [`ospsuite.reportingengine::Task$getAbsolutePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getAbsolutePath)
- [`ospsuite.reportingengine::Task$getInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getInputs)
- [`ospsuite.reportingengine::Task$getRelativePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getRelativePath)
- [`ospsuite.reportingengine::Task$inactivate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-inactivate)
- [`ospsuite.reportingengine::Task$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-print)
- [`ospsuite.reportingengine::Task$validateInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateInput)
- [`ospsuite.reportingengine::Task$validateStructureSetInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateStructureSetInput)
- [`ospsuite.reportingengine::PlotTask$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.html#method-initialize)

------------------------------------------------------------------------

### Method `saveResults()`

Save results from task run.

#### Usage

    QualificationTask$saveResults(taskResults, configurationPlan)

#### Arguments

- `taskResults`:

  list of `TaskResults` objects

- `configurationPlan`:

  A `ConfigurationPlan` object

------------------------------------------------------------------------

### Method `runTask()`

Run task and save its output

#### Usage

    QualificationTask$runTask(configurationPlan)

#### Arguments

- `configurationPlan`:

  A `ConfigurationPlan` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    QualificationTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
