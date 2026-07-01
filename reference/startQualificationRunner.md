# startQualificationRunner

Starts the qualification runner and creates inputs for the reporting
engine

## Usage

``` r
startQualificationRunner(
  qualificationRunnerFolder,
  qualificationPlanFile,
  outputFolder,
  pkSimPortableFolder = NULL,
  configurationPlanName = NULL,
  overwrite = TRUE,
  logFile = NULL,
  logLevel = NULL,
  displayVersion = FALSE
)
```

## Arguments

- qualificationRunnerFolder:

  Folder where QualificationRunner.exe is located

- qualificationPlanFile:

  full path of the input qualification plan

- outputFolder:

  Name of output folder created by the qualification runner

- pkSimPortableFolder:

  Folder where PK-Sim is located. If not specified, installation path
  will be read from the registry (available only in case of full
  **non-portable** installation). This option is **MANDATORY** for the
  portable version of PK-Sim.

- configurationPlanName:

  Name of the configuration plan to be generated. Default is
  `"report-configuration-plan"`

- overwrite:

  Logical defining if the contents of the output folder will be deleted,
  even if it not empty. Default is false.

- logFile:

  Full path of log file where log output will be written. A log file
  will not be created if this value is not provided.

- logLevel:

  Log verbosity (Debug, Information, Warning, Error). Default is
  Information.

- displayVersion:

  Logical defining if version information is displayed

## See also

Other qualification workflow:
[`ConfigurationPlan`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ConfigurationPlan.md),
[`QualificationVersionInfo`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/QualificationVersionInfo.md),
[`adjustTitlePage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/adjustTitlePage.md),
[`loadConfigurationPlan()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/loadConfigurationPlan.md),
[`loadQualificationWorkflow()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/loadQualificationWorkflow.md)
