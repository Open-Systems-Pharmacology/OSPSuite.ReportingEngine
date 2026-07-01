# MeanModelWorkflow

R6 class for Reporting Engine Mean Model Workflow

## See also

Other workflows:
[`PopulationWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/PopulationWorkflow.md),
[`QualificationWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/QualificationWorkflow.md)

## Super class

[`ospsuite.reportingengine::Workflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.md)
-\> `MeanModelWorkflow`

## Public fields

- `simulate`:

  `SimulationTask` object for time profile simulations

- `calculatePKParameters`:

  `CalculatePKParametersTask` object for PK parameters calculation

- `calculateSensitivity`:

  `SensitivityAnalysisTask` object for sensitivity analysis

- `plotTimeProfilesAndResiduals`:

  `PlotTask` object for goodness of fit plots

- `plotMassBalance`:

  `PlotTask` object for mass balance plot

- `plotAbsorption`:

  `PlotTask` object for absorption plot

- `plotPKParameters`:

  `PlotTask` object for PK parameters plot

- `plotSensitivity`:

  `PlotTask` object for sensitivity plot

## Methods

### Public methods

- [`MeanModelWorkflow$new()`](#method-MeanModelWorkflow-new)

- [`MeanModelWorkflow$runWorkflow()`](#method-MeanModelWorkflow-runWorkflow)

- [`MeanModelWorkflow$clone()`](#method-MeanModelWorkflow-clone)

Inherited methods

- [`ospsuite.reportingengine::Workflow$activateTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-activateTasks)
- [`ospsuite.reportingengine::Workflow$getActiveTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getActiveTasks)
- [`ospsuite.reportingengine::Workflow$getAllPlotTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getAllPlotTasks)
- [`ospsuite.reportingengine::Workflow$getAllTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getAllTasks)
- [`ospsuite.reportingengine::Workflow$getInactiveTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getInactiveTasks)
- [`ospsuite.reportingengine::Workflow$getParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getParameterDisplayPaths)
- [`ospsuite.reportingengine::Workflow$getSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getSimulationDescriptor)
- [`ospsuite.reportingengine::Workflow$getWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-getWatermark)
- [`ospsuite.reportingengine::Workflow$inactivateTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-inactivateTasks)
- [`ospsuite.reportingengine::Workflow$print()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-print)
- [`ospsuite.reportingengine::Workflow$printReportingEngineInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-printReportingEngineInfo)
- [`ospsuite.reportingengine::Workflow$setParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-setParameterDisplayPaths)
- [`ospsuite.reportingengine::Workflow$setSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-setSimulationDescriptor)
- [`ospsuite.reportingengine::Workflow$setWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/Workflow.html#method-setWatermark)

------------------------------------------------------------------------

### Method `new()`

Create a new `MeanModelWorkflow` object.

#### Usage

    MeanModelWorkflow$new(...)

#### Arguments

- `...`:

  input parameters inherited from R6 class object `Workflow`.

#### Returns

A new `MeanModelWorkflow` object

------------------------------------------------------------------------

### Method `runWorkflow()`

Run mean model workflow tasks for all simulation sets if tasks are
activated The order of tasks is as follows:

1.  Run simulations

2.  Perform PK and sensitivity analyses

3.  Perform plot tasks  
    a. time profiles and residual plots  
    b. absorption plots  
    c. mass balance plots  
    d. PK and sensitivity analyses tables and plots

4.  Render report

#### Usage

    MeanModelWorkflow$runWorkflow()

#### Returns

All results and plots as a structured output in the workflow folder

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeanModelWorkflow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
