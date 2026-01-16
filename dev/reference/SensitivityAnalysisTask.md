# SensitivityAnalysisTask

R6 class for SensitivityAnalysisTask settings

## See also

Other workflow tasks:
[`GofPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/GofPlotTask.md),
[`PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md),
[`PopulationPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationPlotTask.md),
[`PopulationSensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationSensitivityAnalysisTask.md),
[`QualificationTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationTask.md),
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

## Super class

[`ospsuite.reportingengine::Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md)
-\> `SensitivityAnalysisTask`

## Public fields

- `getTaskResults`:

  function called by task that computes and format figure results

- `settings`:

  instance of SensitivityAnalysisSettings class

- `nameTaskResults`:

  name of function that returns task results

## Methods

### Public methods

- [`SensitivityAnalysisTask$new()`](#method-SensitivityAnalysisTask-new)

- [`SensitivityAnalysisTask$saveResults()`](#method-SensitivityAnalysisTask-saveResults)

- [`SensitivityAnalysisTask$runTask()`](#method-SensitivityAnalysisTask-runTask)

- [`SensitivityAnalysisTask$clone()`](#method-SensitivityAnalysisTask-clone)

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

Create a `SensitivityAnalysisTask` object

#### Usage

    SensitivityAnalysisTask$new(
      getTaskResults = NULL,
      settings = NULL,
      nameTaskResults = "none",
      ...
    )

#### Arguments

- `getTaskResults`:

  function called by task that computes and format figure results

- `settings`:

  `SensitivityAnalysisSettings` object

- `nameTaskResults`:

  name of function that returns task results

- `...`:

  parameters inherited from R6 class `Task` object

#### Returns

A new `SensitivityAnalysisTask` object

------------------------------------------------------------------------

### Method `saveResults()`

Save the task results related to a `structureSet`.

#### Usage

    SensitivityAnalysisTask$saveResults(structureSet, taskResults)

#### Arguments

- `structureSet`:

  A `SimulationStructure` object defining the properties of a simulation
  set

- `taskResults`:

  list of results from task run.

------------------------------------------------------------------------

### Method `runTask()`

Run task and save its output results

#### Usage

    SensitivityAnalysisTask$runTask(structureSets)

#### Arguments

- `structureSets`:

  list of `SimulationStructure` objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SensitivityAnalysisTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
