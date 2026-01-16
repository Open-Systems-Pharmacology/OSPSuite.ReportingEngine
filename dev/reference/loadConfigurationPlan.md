# loadConfigurationPlan

Load a `ConfigurationPlan` object from json file

## Usage

``` r
loadConfigurationPlan(configurationPlanFile, workflowFolder)
```

## Arguments

- configurationPlanFile:

  path to the json file corresponding to the Configuration Plan of a
  Qualification workflow

- workflowFolder:

  path of the output folder created or used by the Workflow.

## Value

A `ConfigurationPlan` object including the content of json file

## See also

Other qualification workflow:
[`ConfigurationPlan`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/ConfigurationPlan.md),
[`QualificationVersionInfo`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationVersionInfo.md),
[`adjustTitlePage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/adjustTitlePage.md),
[`loadQualificationWorkflow()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationWorkflow.md),
[`startQualificationRunner()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/startQualificationRunner.md)
