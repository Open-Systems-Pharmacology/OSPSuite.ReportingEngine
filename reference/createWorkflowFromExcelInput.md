# createWorkflowFromExcelInput

Creates an R script for running mean model or population workflows from
an Excel file Note that the `{readxl}` package is required to use this
function

## Usage

``` r
createWorkflowFromExcelInput(
  excelFile,
  workflowFile = "workflow.R",
  removeComments = FALSE
)
```

## Arguments

- excelFile:

  name of the Excel file from which the R script is created

- workflowFile:

  name of the R script file created from the Excel file

- removeComments:

  logical to remove comments and information in `workflowFile`

## Value

An R script of name `workflowFile` to be run

## See also

Other workflow helpers:
[`activateWorkflowTasks()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/activateWorkflowTasks.md),
[`addStudyParameters()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addStudyParameters.md),
[`addXParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addXParametersForDemographyPlot.md),
[`addXParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addXParametersForPKParametersPlot.md),
[`addYParametersForDemographyPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addYParametersForDemographyPlot.md),
[`addYParametersForPKParametersPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/addYParametersForPKParametersPlot.md),
[`checkTaskInputsExist()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/checkTaskInputsExist.md),
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
