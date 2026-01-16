# CalculatePKParametersTask

R6 class for defining how pk parameters are calculated and save

## Super classes

[`ospsuite.reportingengine::Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md)
-\>
[`ospsuite.reportingengine::SimulationTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SimulationTask.md)
-\> `CalculatePKParametersTask`

## Public fields

- `ratioComparison`:

  logical defining if a ratio comparison is required

## Methods

### Public methods

- [`CalculatePKParametersTask$saveResults()`](#method-CalculatePKParametersTask-saveResults)

- [`CalculatePKParametersTask$runTask()`](#method-CalculatePKParametersTask-runTask)

- [`CalculatePKParametersTask$clone()`](#method-CalculatePKParametersTask-clone)

Inherited methods

- [`ospsuite.reportingengine::Task$activate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-activate)
- [`ospsuite.reportingengine::Task$getAbsolutePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getAbsolutePath)
- [`ospsuite.reportingengine::Task$getInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getInputs)
- [`ospsuite.reportingengine::Task$getRelativePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-getRelativePath)
- [`ospsuite.reportingengine::Task$inactivate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-inactivate)
- [`ospsuite.reportingengine::Task$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-print)
- [`ospsuite.reportingengine::Task$validateInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateInput)
- [`ospsuite.reportingengine::Task$validateStructureSetInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.html#method-validateStructureSetInput)
- [`ospsuite.reportingengine::SimulationTask$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SimulationTask.html#method-initialize)

------------------------------------------------------------------------

### Method `saveResults()`

Save the task results related to a `structureSet`.

#### Usage

    CalculatePKParametersTask$saveResults(structureSet, taskResults)

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

    CalculatePKParametersTask$runTask(structureSets)

#### Arguments

- `structureSets`:

  list of `SimulationStructure` objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CalculatePKParametersTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
