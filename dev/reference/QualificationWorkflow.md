# QualificationWorkflow

R6 class for Reporting Engine Qualification Workflow

## See also

Other workflows:
[`MeanModelWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/MeanModelWorkflow.md),
[`PopulationWorkflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationWorkflow.md)

## Super class

[`ospsuite.reportingengine::Workflow`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Workflow.md)
-\> `QualificationWorkflow`

## Public fields

- `configurationPlan`:

  `ConfigurationPlan` object

- `simulate`:

  `SimulationTask` object for time profile simulations

- `calculatePKParameters`:

  `CalculatePKParametersTask` object for PK parameters calculation

- `plotTimeProfiles`:

  `PlotTask` object for time profile plots

- `plotComparisonTimeProfile`:

  `PlotTask` object for comparison of time profiles plots

- `plotGOFMerged`:

  `PlotTask` object for goodness of fit plots

- `plotPKRatio`:

  `PlotTask` object for PK ratio plot

- `plotDDIRatio`:

  `PlotTask` object for DDI ratio plot

## Methods

### Public methods

- [`QualificationWorkflow$new()`](#method-QualificationWorkflow-new)

- [`QualificationWorkflow$runWorkflow()`](#method-QualificationWorkflow-runWorkflow)

- [`QualificationWorkflow$updateConfigurationPlan()`](#method-QualificationWorkflow-updateConfigurationPlan)

- [`QualificationWorkflow$clone()`](#method-QualificationWorkflow-clone)

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

Create a new `QualificationWorkflow` object.

#### Usage

    QualificationWorkflow$new(configurationPlan, ...)

#### Arguments

- `configurationPlan`:

  A `ConfigurationPlan` object

- `...`:

  input parameters inherited from R6 class object `Workflow`.

#### Returns

A new `QualificationWorkflow` object

------------------------------------------------------------------------

### Method `runWorkflow()`

Run qualification workflow tasks for all simulation sets if tasks are
activated The order of tasks is as follows:

1.  Run simulations

2.  Perform PK analyses

3.  Perform plot tasks  
    a. time profiles and residual plots  
    b. comparison time profiles plots  
    c. PK ratio tables and plots  
    d. DDI ratio tables and plots

4.  Render report

#### Usage

    QualificationWorkflow$runWorkflow()

#### Returns

All results and plots as a structured output in the workflow folder

------------------------------------------------------------------------

### Method `updateConfigurationPlan()`

Update the content of the workflow `configurationPlan`. Caution,
updating the `configurationPlan` using this method won't update the
workflow simulations and their results. Use the method only to bypass
reloading a full workflow if only plot aesthetics or section content is
changed.

#### Usage

    QualificationWorkflow$updateConfigurationPlan(configurationPlanFile)

#### Arguments

- `configurationPlanFile`:

  path to the json file corresponding to the Configuration Plan of a
  Qualification workflow

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    QualificationWorkflow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
