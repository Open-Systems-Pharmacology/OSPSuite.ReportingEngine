# GofPlotTask

R6 class for GofPlotTask settings

## See also

Other workflow tasks:
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
-\> `GofPlotTask`

## Methods

### Public methods

- [`GofPlotTask$saveResults()`](#method-GofPlotTask-saveResults)

- [`GofPlotTask$saveResidualsResults()`](#method-GofPlotTask-saveResidualsResults)

- [`GofPlotTask$runTask()`](#method-GofPlotTask-runTask)

- [`GofPlotTask$getResidualsResults()`](#method-GofPlotTask-getResidualsResults)

- [`GofPlotTask$clone()`](#method-GofPlotTask-clone)

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

Save the task results related to a `structureSet`.

#### Usage

    GofPlotTask$saveResults(structureSet, taskResults)

#### Arguments

- `structureSet`:

  A `SimulationStructure` object defining the properties of a simulation
  set

- `taskResults`:

  list of `TaskResults` objects

------------------------------------------------------------------------

### Method `saveResidualsResults()`

Save the task results related to residuals across all simulations

#### Usage

    GofPlotTask$saveResidualsResults(taskResults)

#### Arguments

- `taskResults`:

  list of `TaskResults` objects

------------------------------------------------------------------------

### Method `runTask()`

Run task and save its output results

#### Usage

    GofPlotTask$runTask(structureSets)

#### Arguments

- `structureSets`:

  list of `SimulationStructure` objects

------------------------------------------------------------------------

### Method `getResidualsResults()`

Get plot results for residuals across all simulations

#### Usage

    GofPlotTask$getResidualsResults(structureSets, data, metaData, settings)

#### Arguments

- `structureSets`:

  A list of `SimulationStructure` objects defining the properties of a
  simulation set

- `data`:

  data.frame

- `metaData`:

  A data.frame containing relevant information about the residuals

- `settings`:

  List of settings such as `PlotConfiguration` R6 class objects for each
  goodness of fit plot

#### Returns

A list of `TaskResults` objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GofPlotTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
