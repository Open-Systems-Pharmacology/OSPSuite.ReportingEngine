# PopulationPlotTask

R6 class for PopulationPlotTask settings

## See also

Other workflow tasks:
[`GofPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/GofPlotTask.md),
[`PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md),
[`PopulationSensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationSensitivityAnalysisTask.md),
[`QualificationTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationTask.md),
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
-\> `PopulationPlotTask`

## Public fields

- `workflowType`:

  Type of population workflow

- `xParameters`:

  list of parameter paths to be plotted along x-axis

- `yParameters`:

  list of parameter paths to be plotted along y-axis

## Methods

### Public methods

- [`PopulationPlotTask$new()`](#method-PopulationPlotTask-new)

- [`PopulationPlotTask$saveResults()`](#method-PopulationPlotTask-saveResults)

- [`PopulationPlotTask$runTask()`](#method-PopulationPlotTask-runTask)

- [`PopulationPlotTask$clone()`](#method-PopulationPlotTask-clone)

Inherited methods

- [`ospsuite.reportingengine::Task$activate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-activate)
- [`ospsuite.reportingengine::Task$getAbsolutePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getAbsolutePath)
- [`ospsuite.reportingengine::Task$getInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getInputs)
- [`ospsuite.reportingengine::Task$getRelativePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getRelativePath)
- [`ospsuite.reportingengine::Task$inactivate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-inactivate)
- [`ospsuite.reportingengine::Task$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-print)
- [`ospsuite.reportingengine::Task$validateInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateInput)
- [`ospsuite.reportingengine::Task$validateStructureSetInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateStructureSetInput)

------------------------------------------------------------------------

### Method `new()`

Create a `PopulationPlotTask` object

#### Usage

    PopulationPlotTask$new(
      workflowType = PopulationWorkflowTypes$parallelComparison,
      xParameters = NULL,
      yParameters = NULL,
      ...
    )

#### Arguments

- `workflowType`:

  Type of population workflow. Use enum `PopulationWorkflowTypes` to get
  list of workflow types.

- `xParameters`:

  list of parameter paths to be plotted along x-axis

- `yParameters`:

  list of parameter paths to be plotted along y-axis

- `...`:

  input parameters inherited from `PlotTask` R6 class

#### Returns

A new `PopulationPlotTask` object

------------------------------------------------------------------------

### Method `saveResults()`

Save the task results

#### Usage

    PopulationPlotTask$saveResults(taskResults)

#### Arguments

- `taskResults`:

  list of `TaskResults` objects

------------------------------------------------------------------------

### Method `runTask()`

Run task and save its output

#### Usage

    PopulationPlotTask$runTask(structureSets)

#### Arguments

- `structureSets`:

  list of `SimulationStructure` R6 class

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PopulationPlotTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
