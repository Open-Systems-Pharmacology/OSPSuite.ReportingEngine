# activateWorkflowTasks

activates a series of `Tasks` from a `Workflow`

## Usage

``` r
activateWorkflowTasks(workflow, tasks = workflow$getAllTasks())
```

## Arguments

- workflow:

  A `Workflow` object

- tasks:

  names of the tasks to activate Default activates all tasks of the
  workflow using workflow method `workflow$getAllTasks()`

## See also

Other workflow helpers:
[`addStudyParameters()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addStudyParameters.md),
[`addXParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addXParametersForDemographyPlot.md),
[`addXParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addXParametersForPKParametersPlot.md),
[`addYParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addYParametersForDemographyPlot.md),
[`addYParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addYParametersForPKParametersPlot.md),
[`checkTaskInputsExist()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/checkTaskInputsExist.md),
[`createWorkflowFromExcelInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/createWorkflowFromExcelInput.md),
[`getMeanSensitivityAnalysisResultsFileNames()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getMeanSensitivityAnalysisResultsFileNames.md),
[`getOutputPathsInSimulationSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getOutputPathsInSimulationSet.md),
[`getPKAnalysisResultsFileNames()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPKAnalysisResultsFileNames.md),
[`getPKParameterGroupsInOutput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPKParameterGroupsInOutput.md),
[`getPKParametersInOutput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPKParametersInOutput.md),
[`getPKParametersInSimulationSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPKParametersInSimulationSet.md),
[`getPopulationPKData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPopulationPKData.md),
[`getPopulationPKMetaData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPopulationPKMetaData.md),
[`getPopulationSensitivityAnalysisResultsFileNames()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getPopulationSensitivityAnalysisResultsFileNames.md),
[`getSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getSimulationDescriptor.md),
[`getSimulationParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getSimulationParameterDisplayPaths.md),
[`getSimulationResultFileNames()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getSimulationResultFileNames.md),
[`getTaskInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getTaskInputs.md),
[`getWorkflowParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getWorkflowParameterDisplayPaths.md),
[`getWorkflowReferencePopulationName()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getWorkflowReferencePopulationName.md),
[`getXParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getXParametersForDemographyPlot.md),
[`getXParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getXParametersForPKParametersPlot.md),
[`getYParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getYParametersForDemographyPlot.md),
[`getYParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/getYParametersForPKParametersPlot.md),
[`inactivateWorkflowTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/inactivateWorkflowTasks.md),
[`setSimulationDescriptor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setSimulationDescriptor.md),
[`setWorkflowParameterDisplayPaths()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setWorkflowParameterDisplayPaths.md),
[`setWorkflowParameterDisplayPathsFromFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setWorkflowParameterDisplayPathsFromFile.md),
[`setXParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setXParametersForDemographyPlot.md),
[`setXParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setXParametersForPKParametersPlot.md),
[`setYParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setYParametersForDemographyPlot.md),
[`setYParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/setYParametersForPKParametersPlot.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# A workflow object needs to be created first
myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)

# Use enum helper to get task names
activateWorkflowTasks(
  workflow = myWorkflow,
  tasks = StandardSimulationTasks$simulate
)

# Default will activate every task of workflow
activateWorkflowTasks(workflow = myWorkflow)
} # }
```
