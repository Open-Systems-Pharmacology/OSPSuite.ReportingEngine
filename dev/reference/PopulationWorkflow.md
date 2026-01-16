# PopulationWorkflow

R6 class for Reporting Engine Population Workflow

## See also

Other workflows:
[`MeanModelWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/MeanModelWorkflow.md),
[`QualificationWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationWorkflow.md)

## Super class

[`ospsuite.reportingengine::Workflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.md)
-\> `PopulationWorkflow`

## Public fields

- `workflowType`:

  Type of population workflow

- `simulate`:

  `SimulationTask` object for time profile simulations

- `calculatePKParameters`:

  `CalculatePKParametersTask` object for PK parameters calculation

- `calculateSensitivity`:

  `SensitivityAnalysisTask` object for sensitivity analysis

- `plotDemography`:

  R6 class `Task` for demography plots

- `plotTimeProfilesAndResiduals`:

  `PlotTask` object for goodness of fit plots

- `plotPKParameters`:

  R6 class `Task` for PK parameters plot

- `plotSensitivity`:

  R6 class `Task` for sensitivity plot

## Methods

### Public methods

- [`PopulationWorkflow$new()`](#method-PopulationWorkflow-new)

- [`PopulationWorkflow$runWorkflow()`](#method-PopulationWorkflow-runWorkflow)

- [`PopulationWorkflow$clone()`](#method-PopulationWorkflow-clone)

Inherited methods

- [`ospsuite.reportingengine::Workflow$activateTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-activateTasks)
- [`ospsuite.reportingengine::Workflow$getActiveTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getActiveTasks)
- [`ospsuite.reportingengine::Workflow$getAllPlotTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getAllPlotTasks)
- [`ospsuite.reportingengine::Workflow$getAllTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getAllTasks)
- [`ospsuite.reportingengine::Workflow$getInactiveTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getInactiveTasks)
- [`ospsuite.reportingengine::Workflow$getParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getParameterDisplayPaths)
- [`ospsuite.reportingengine::Workflow$getSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getSimulationDescriptor)
- [`ospsuite.reportingengine::Workflow$getWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-getWatermark)
- [`ospsuite.reportingengine::Workflow$inactivateTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-inactivateTasks)
- [`ospsuite.reportingengine::Workflow$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-print)
- [`ospsuite.reportingengine::Workflow$printReportingEngineInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-printReportingEngineInfo)
- [`ospsuite.reportingengine::Workflow$setParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-setParameterDisplayPaths)
- [`ospsuite.reportingengine::Workflow$setSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-setSimulationDescriptor)
- [`ospsuite.reportingengine::Workflow$setWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.html#method-setWatermark)

------------------------------------------------------------------------

### Method `new()`

Create a new `PopulationWorkflow` object.

#### Usage

    PopulationWorkflow$new(
      workflowType,
      simulationSets,
      workflowFolder,
      createWordReport = TRUE,
      watermark = NULL,
      simulationSetDescriptor = NULL,
      numberSections = TRUE,
      reportTitle = NULL,
      theme = NULL
    )

#### Arguments

- `workflowType`:

  Type of population workflow. Use enum `PopulationWorkflowTypes` to get
  list of workflow types.

- `simulationSets`:

  list of `SimulationSet` R6 class objects

- `workflowFolder`:

  path of the output folder created or used by the Workflow.

- `createWordReport`:

  logical of option for creating Markdown-Report only but not a
  Word-Report.

- `watermark`:

  displayed watermark in every plot background

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

A new `PopulationWorkflow` object

------------------------------------------------------------------------

### Method `runWorkflow()`

Run population workflow tasks for all simulation sets if tasks are
activated The order of tasks is as follows:

1.  Run simulations

2.  Perform PK and sensitivity analyses

3.  Perform plot tasks  
    a. time profiles and residual plots  
    b. demography plots  
    c. PK and sensitivity analyses tables and plots  

4.  Render report

#### Usage

    PopulationWorkflow$runWorkflow()

#### Returns

All results and plots as a structured output in the workflow folder

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PopulationWorkflow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
