# Workflow

R6 class representing Reporting Engine generic Workflow

## Public fields

- `simulationStructures`:

  `SimulationStructure` R6 class object managing the structure of the
  workflow output

- `workflowFolder`:

  path of the folder create by the Workflow

- `taskNames`:

  Enum of task names

- `reportFileName`:

  name of the Rmd report file

- `reportTitle`:

  report title page

- `createWordReport`:

  logical of option for creating Markdown-Report only but not a
  Word-Report.

- `wordConversionTemplate`:

  optional docx template for rendering a tuned Word-Report document

- `userDefinedTasks`:

  List of user-defined tasks (to update with loadUserDefinedTask)

- `numberSections`:

  logical defining if the report sections should be numbered

## Active bindings

- `reportFolder`:

  Directory in which workflow report is saved

- `reportFilePath`:

  Path of workflow report

## Methods

### Public methods

- [`Workflow$new()`](#method-Workflow-new)

- [`Workflow$getAllTasks()`](#method-Workflow-getAllTasks)

- [`Workflow$getAllPlotTasks()`](#method-Workflow-getAllPlotTasks)

- [`Workflow$getActiveTasks()`](#method-Workflow-getActiveTasks)

- [`Workflow$getInactiveTasks()`](#method-Workflow-getInactiveTasks)

- [`Workflow$activateTasks()`](#method-Workflow-activateTasks)

- [`Workflow$inactivateTasks()`](#method-Workflow-inactivateTasks)

- [`Workflow$printReportingEngineInfo()`](#method-Workflow-printReportingEngineInfo)

- [`Workflow$getWatermark()`](#method-Workflow-getWatermark)

- [`Workflow$setWatermark()`](#method-Workflow-setWatermark)

- [`Workflow$setParameterDisplayPaths()`](#method-Workflow-setParameterDisplayPaths)

- [`Workflow$getParameterDisplayPaths()`](#method-Workflow-getParameterDisplayPaths)

- [`Workflow$setSimulationDescriptor()`](#method-Workflow-setSimulationDescriptor)

- [`Workflow$getSimulationDescriptor()`](#method-Workflow-getSimulationDescriptor)

- [`Workflow$print()`](#method-Workflow-print)

- [`Workflow$clone()`](#method-Workflow-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Workflow` object.

#### Usage

    Workflow$new(
      simulationSets,
      workflowFolder,
      createWordReport = TRUE,
      wordConversionTemplate = NULL,
      watermark = NULL,
      simulationSetDescriptor = NULL,
      numberSections = TRUE,
      reportTitle = NULL,
      theme = NULL
    )

#### Arguments

- `simulationSets`:

  list of `SimulationSet` R6 class objects

- `workflowFolder`:

  path of the output folder created or used by the Workflow.

- `createWordReport`:

  logical of option for creating Markdown-Report only but not a
  Word-Report.

- `wordConversionTemplate`:

  optional docx template for rendering a tuned Word-Report document

- `watermark`:

  displayed watermark in figures background

- `simulationSetDescriptor`:

  character Descriptor of simulation sets indicated in reports

- `numberSections`:

  logical defining if the report sections should be numbered

- `reportTitle`:

  report title internally added as a cover page If `reportTitle` is an
  existing file, it will be merged to the report as cover page.

- `theme`:

  A `Theme` object from `{tlf}` package

#### Returns

A new `Workflow` object

------------------------------------------------------------------------

### Method `getAllTasks()`

Get a vector with all the names of the tasks within the `Workflow`

#### Usage

    Workflow$getAllTasks()

#### Returns

Vector of `Task` names

------------------------------------------------------------------------

### Method `getAllPlotTasks()`

Get a vector with all the names of the plot tasks within the `Workflow`

#### Usage

    Workflow$getAllPlotTasks()

#### Returns

Vector of `PlotTask` names

------------------------------------------------------------------------

### Method `getActiveTasks()`

Get a vector with all the names of active tasks within the `Workflow`

#### Usage

    Workflow$getActiveTasks()

#### Returns

Vector of active `Task` names

------------------------------------------------------------------------

### Method `getInactiveTasks()`

Get a vector with all the names of inactive tasks within the `Workflow`

#### Usage

    Workflow$getInactiveTasks()

#### Returns

Vector of inactive `Task` names

------------------------------------------------------------------------

### Method `activateTasks()`

Activates a series of `Tasks` from current `Workflow`

#### Usage

    Workflow$activateTasks(tasks = self$getAllTasks())

#### Arguments

- `tasks`:

  names of the workflow tasks to activate. Default activates all tasks
  of the workflow using workflow method `workflow$getAllTasks()`

#### Returns

Vector of inactive `Task` names

------------------------------------------------------------------------

### Method `inactivateTasks()`

Inactivates a series of `Tasks` from current `Workflow`

#### Usage

    Workflow$inactivateTasks(tasks = self$getAllTasks())

#### Arguments

- `tasks`:

  names of the workflow tasks to inactivate. Default inactivates all
  tasks of the workflow using workflow method `workflow$getAllTasks()`

#### Returns

Vector of inactive `Task` names

------------------------------------------------------------------------

### Method `printReportingEngineInfo()`

Print reporting engine information obtained from initializing a
`Workflow`

#### Usage

    Workflow$printReportingEngineInfo()

------------------------------------------------------------------------

### Method `getWatermark()`

Get the current watermark to be reported on figures background

#### Usage

    Workflow$getWatermark()

------------------------------------------------------------------------

### Method `setWatermark()`

Set the watermark to be reported on figure background. The default value
`NULL` leads to check if the computer has a validated environment. If
the environment is validated, no watermark is reported on the
background. If the environment is NOT validated,
`workflowWatermarkMessage` is reported on the background. Any
user-defined text will overwrite this default feature and be reported on
the figure background.

#### Usage

    Workflow$setWatermark(watermark)

#### Arguments

- `watermark`:

  text to be reported on figures background. Default value is `NULL`,
  which leads to check if the computer has a validated environment. If
  the environment is validated, no watermark is reported on the
  background. If the environment is NOT validated,
  `workflowWatermarkMessage` is reported on the background. Any
  user-defined text will overwrite this default feature and be reported
  on the figure background.

------------------------------------------------------------------------

### Method `setParameterDisplayPaths()`

Set mapping between parameters and their display paths in workflow to
replace standard display of parameter paths.

#### Usage

    Workflow$setParameterDisplayPaths(parameterDisplayPaths)

#### Arguments

- `parameterDisplayPaths`:

  data.frame mapping Parameters with their display paths Variables of
  the data.frame should include `parameter` and `displayPath`.

------------------------------------------------------------------------

### Method `getParameterDisplayPaths()`

Get mapping between parameters and their display paths in workflow to
replace standard display of parameter paths.

#### Usage

    Workflow$getParameterDisplayPaths()

#### Returns

A data.frame with `parameter` and `displayPath` variables.

------------------------------------------------------------------------

### Method [`setSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/setSimulationDescriptor.md)

Set descriptor of simulation sets indicated in reports

#### Usage

    Workflow$setSimulationDescriptor(text)

#### Arguments

- `text`:

  character describing what simulation sets refer to

------------------------------------------------------------------------

### Method [`getSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getSimulationDescriptor.md)

Get descriptor of simulation sets indicated in reports

#### Usage

    Workflow$getSimulationDescriptor()

#### Returns

character Descriptor of simulation sets

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print workflow list of tasks

#### Usage

    Workflow$print()

#### Returns

Task list information

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Workflow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
