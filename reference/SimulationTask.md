# SimulationTask

R6 class for SimulationTask settings

## Super class

[`ospsuite.reportingengine::Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.md)
-\> `SimulationTask`

## Public fields

- `getTaskResults`:

  function called by task that computes and format figure results

- `settings`:

  instance of SimulationSettings class

- `nameTaskResults`:

  name of function that returns task results

## Methods

### Public methods

- [`SimulationTask$new()`](#method-SimulationTask-new)

- [`SimulationTask$saveResults()`](#method-SimulationTask-saveResults)

- [`SimulationTask$runTask()`](#method-SimulationTask-runTask)

- [`SimulationTask$clone()`](#method-SimulationTask-clone)

Inherited methods

- [`ospsuite.reportingengine::Task$activate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-activate)
- [`ospsuite.reportingengine::Task$getAbsolutePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-getAbsolutePath)
- [`ospsuite.reportingengine::Task$getInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-getInputs)
- [`ospsuite.reportingengine::Task$getRelativePath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-getRelativePath)
- [`ospsuite.reportingengine::Task$inactivate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-inactivate)
- [`ospsuite.reportingengine::Task$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-print)
- [`ospsuite.reportingengine::Task$validateInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-validateInput)
- [`ospsuite.reportingengine::Task$validateStructureSetInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Task.html#method-validateStructureSetInput)

------------------------------------------------------------------------

### Method `new()`

Create a `SimulationTask` object

#### Usage

    SimulationTask$new(
      getTaskResults = NULL,
      settings = NULL,
      nameTaskResults = "none",
      ...
    )

#### Arguments

- `getTaskResults`:

  function called by task that computes and format figure results

- `settings`:

  instance of SimulationSettings class

- `nameTaskResults`:

  name of function that returns task results

- `...`:

  parameters inherited from R6 class `Task` object

#### Returns

A new `SimulationTask` object

------------------------------------------------------------------------

### Method `saveResults()`

Save results from task run.

#### Usage

    SimulationTask$saveResults(set, taskResults)

#### Arguments

- `set`:

  R6 class `SimulationStructure`

- `taskResults`:

  list of results from task run.

------------------------------------------------------------------------

### Method `runTask()`

Run task and save its output

#### Usage

    SimulationTask$runTask(structureSets)

#### Arguments

- `structureSets`:

  list of `SimulationStructure` R6 class

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
