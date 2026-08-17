# Qualification Workflow

``` r

require(ospsuite.reportingengine)
#> Loading required package: ospsuite.reportingengine
#> Loading required package: tlf
#> Loading required package: ospsuite
#> 
#> Attaching package: 'ospsuite'
#> The following object is masked from 'package:tlf':
#> 
#>     plotTimeProfile
#> Warning: replacing previous import 'ospsuite::plotTimeProfile' by
#> 'tlf::plotTimeProfile' when loading 'ospsuite.reportingengine'
```

## Objectives

The [qualification
framework](https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/qualification)
enables an automated validation of various scenarios (use-cases)
supported by the OSP platform. This technical framework is used, for
example, to release, in full confidence, a new version of the OSP Suite
by verifying automatically that an ever-growing list of scenarios is
performing as expected. Qualification workflows as performed by the
package `ospsuite.reportingengine` aims at building Qualification
Reports in Markdown and Word format. This article aims at introducing
how to work with `ospsuite.reportingengine` to perform such
qualification workflows.

## Qualification runner

The **QualificationRunner** is the software in charge of initializing
and managing the qualification workflow by creating and organizing the
inputs required for the reporting engine.

The function `startQualificationRunner`, inspired from its Matlab
version, launches the **QualificationRunner** on a project. The function
requires the inputs below:

- `qualificationRunnerFolder`: the directory where the file
  **QualificationRunner.exe** is located
- `qualificationPlanFile`: the path of the input qualification plan
  (json file summarizing the project qualification)
- `outputFolder`: the path of the output directory created by the
  qualification runner

``` r

startQualificationRunner(qualificationRunnerFolder, qualificationPlanFile, outputFolder)
```

Note that, in the R version of `startQualificationRunner`, the
simulations and PK analyses are not run from the **QualificationRunner**
but from the workflow itself. This update aims at preventing the runs of
all the simulations available in the qualification. Only the simulations
results and PK analyses required by the configuration plan are performed
to optimize the building time of a qualification report.

## Configuration Plan

The configuration plan is included in a json file, created by the
qualification runner, that defines the configuration of the final
report. The function `loadConfigurationPlan` reads such a file and
creates a `ConfigurationPlan` object that all the knowledge required to
build the final report. It is internally called upon creation of a
`QualificationWorkflow` and requires the inputs below:

- `configurationPlanFile`: path of the Configuration Plan json file used
  by the Qualification workflow
- `workflowFolder`: path of the output directory created and/or used by
  the Workflow.

## Qualification workflow

Qualification workflows are built from `Qualificationworkflow` objects
which derive from `Workflow` objects. As such, they are also based on
the concept of tasks but show a difference in the way they are created.

### How to create a qualification workflow

The function `loadQualificationWorkflow` is advised to create a
`QualificationWorkflow` object from a configuration plan. It requires
the same inputs as `loadConfigurationPlan` (`workflowFolder` and
`configurationPlanFile`).

``` r

workflow <- loadQualificationWorkflow(workflowFolder, configurationPlanFile)
```

### Qualification workflow features

#### Configuration Plan

All `QualificationWorkflow` objects include a `configurationPlan` field.
This field is a `ConfigurationPlan` object that provides a smart
interface for building the report.

In particular, all the fields below are reorganized in a data.frame
format clarifying the design of the configuration plan.

- `simulationMappings`
- `observedDataSets`
- `sections`

Additionally, the `configurationPlan` field includes many `get` methods
to search for specific paths of files managed by the configuration plan
(`$getSimulationPath(project, simulation)`, `$getObservedDataPath(id)`,
etc)

#### Tasks available for Qualification workflow

Two simulation tasks are available and active by default in the
qualification workflows:

- `simulate`: simulate time profile results requested in the report
  plots
- `calculatePKParameters`: calculate the PK Parameters requested in the
  PK and DDI ratio plots

Their results are stored within the `workflowFolder` output directory,
in *SimulationResults* and *PKAnalysisResults* directories respectively.

Five plot tasks are available in the qualification workflows, they are
only active if defined by the configuration plan:

- `plotTimeProfiles`: plot time profiles of the simulated outputs and/or
  observed data.
- `plotGOFMerged`: plot goodness of fit results of grouped simulated
  outputs vs observed data.
- `plotComparisonTimeProfile`: plot comparison of time profiles from the
  simulated outputs and/or observed data.
- `plotPKRatio`: plot PK ratios of simulated vs observed PK parameters
  as a function of covariate (usually Age).
- `plotDDIRatio`: plot Drug-Drug Interaction ratios of simulated vs
  observed PK parameters.

#### Settings and artifacts

Most of the settings and artifacts are read from the configuration plan
file and translated into task settings. Settings for simulations include
`showProgress` to show the progress of the `simulate` task and
`allowedCores` to perform the simulations in parallel.

Plot tasks can include multiple results for the report, usually called
artifacts. If the field **Artifacts** is not mentioned in a plot of the
configuration plan, all its corresponding artifacts results will be
added to the report.

##### predictedVsObserved and residualsOverTime

For `plotGOFMerged` and `plotDDIRatio` tasks, the plots represent
observed vs predicted data. It is consequently possible to include them
as predicted vs observed and/or residuals vs time/predicted by including
the options **predictedVsObserved**, **residualsOverPredicted** and
**residualsOverTime** in the field **PlotTypes** of the configuration
plan.

##### Geometric Mean Fold Error (GMFE)

For `plotGOFMerged`, `plotPKRatio` and `plotDDIRatio` tasks, it is
possible to include a **GMFE** artifact in the configuration plan. This
artifact corresponds to the addition of a table showing the Geometric
Mean Fold Error (*GMFE*) of the *residuals* calculated for specific plot
using the formula $`GMFE = 10^{\Sigma log_{10}(residuals)}`$.

##### Measure

For `plotPKRatio` and `plotDDIRatio` tasks, it is possible to include a
**Measure** artifact in the configuration plan. This artifact
corresponds to the addition of a table showing:

- For `plotPKRatio`, count and percent of ratios lying within 1.5- and
  2-fold error ranges.
- For `plotDDIRatio`, count and percent of ratios lying within 2-fold
  and Guest *et al.* error ranges.

##### Table

For `plotPKRatio` task, it is possible to include a **Table** artifact
in the configuration plan. This artifact corresponds to the addition of
a table showing comprehensive observed and simulated PK data of the PK
Ratio analysis.

##### Settings

Since plot tasks can include numerical results, `digits` and
`scientific` are fields available in each task `settings` that will
define the format of their numeric values.

### How to update and re-run a qualification workflow

If simulations need to be re-run, it is advised to reload the workflow
using `loadQualificationWorkflow`.

In case the update does not require to re-run simulations (e.g. updating
the section content, modifying plot aesthetics or removing a plot), the
`QualificationWorkflow` method
`$updateConfigurationPlan(configurationPlanFile)` can be used to avoid
the process of reloading and checking all the simulations of the
workflow.

Then, you can also `inactivate` the tasks corresponding to the re-run of
the simulations before re-running the workflow.

``` r

# Reload Configuration Plan
workflow$updateConfigurationPlan(
  configurationPlanFile = newConfigurationPlanFile
)

# Inactivate simulations already performed
workflow$simulate$inactivate()
workflow$calculatePKParameters$inactivate()

# Re-run and build updated report
workflow$runWorkflow()
```

### Template

A template R script for qualification workflows is available through the
following link
[qualification-workflow-template.R](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/articles/templates/qualification-workflow-template.R).

The template aims at creating a function wrapping the usage of the
qualification workflow including the call to the qualification runner
and named `createQualificationReport`.

``` r

#' @title createQualificationReport
#' @description Run a qualification workflow to create a qualification report.
#' @param qualificationRunnerFolder Folder where QualificationRunner.exe is located
#' @param pkSimPortableFolder Folder where PK-Sim is located.
#' If not specified, installation path will be read from the registry (available only in case of full **non-portable** installation).
#' This option is **MANDATORY** for the portable version of PK-Sim.
#' @param moBiPortableFolder Folder where MoBi is located.
#' If not specified, installation path will be read from the registry (available only in case of full **non-portable** installation).
#' This option is **MANDATORY** for the portable version of MoBi.
#' Only required for qualification plans including MoBi projects.
#' @param createWordReport Logical defining if a `docx` version of the report should also be created.
#' Note that `pandoc` installation is required for this feature
#' [https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/wiki/Installing-pandoc]
#' @param maxSimulationsPerCore An integer that sets the maximum number of simulations per core
#' @param versionInfo A `QualificationVersionInfo` object to update title page with Qualification Version Information
#' @param wordConversionTemplate File name of docx template document passed to Pandoc for the conversion of the md report into docx
#' Default template is available using `system.file("extdata", "reference.docx", package = "ospsuite.reportingengine")`
#' @examples
#' # Create a Qualification Report without any option and running v9.1.1 of Qualification Runner
#' createQualificationReport("C:/Software/QualificationRunner9.1.1")
#' 
#' # Create a Qualification Report and turn off the creation of a doc version
#' createQualificationReport("C:/Software/QualificationRunner9.1.1", createWordReport = FALSE)
#' 
#' # Create a Qualification Report and set the number of simulations to be run per core
#' createQualificationReport("C:/Software/QualificationRunner9.1.1", maxSimulationsPerCore = 8)
#' 
#' # Create a Qualification Report and update Qualification Version Information on title page
#' versionInfo <- QualificationVersionInfo$new("1.1", "2.2","3.3")
#' createQualificationReport("C:/Software/QualificationRunner9.1.1", versionInfo = versionInfo)
#' 
createQualificationReport <- function(qualificationRunnerFolder,
                                      pkSimPortableFolder = NULL,
                                      moBiPortableFolder = NULL,
                                      createWordReport = TRUE,
                                      maxSimulationsPerCore = NULL,
                                      versionInfo = NULL,
                                      wordConversionTemplate = NULL) {
  library(ospsuite.reportingengine)
  # Reset settings such as plot theme or format of numeric in tables
  # to Reporting Engine default values
  resetRESettingsToDefault()

  #-------- STEP 1: Define workflow settings --------#
  #' replace `workingDirectory` and `qualificationPlanName` with your paths
  #'
  #' The directories are assumed the following structure
  #' `workingDirectory`
  #'   - `input`
  #'     - `qualificationPlanName`
  #'   - `re_input`
  #'     - `configurationPlanFile`
  #'   - `re_output`
  #'     - `report`
  #'
  #' In case your folder structure is different from assumed above,
  #' the paths below must be adjusted as well:
  #' - `qualificationPlanFile`: path of qualification plan file run by Qualification Runner
  #' - `reInputFolder`: input path for the Reporting engine
  #' - `reOutputFolder`: outputs of the Reporting Engine will be created here
  #' - `reportName`:  path of final report
  #'
  #' **Template parameters to be replaced below**

  #' `workingDirectory`: current directory is used as default working directory
  workingDirectory <- getwd()

  qualificationPlanName <- "qualification_plan.json"
  qualificationPlanFile <- file.path(workingDirectory, "input", qualificationPlanName)

  #' The default outputs of qualification runner should be generated under `<workingDirectory>/re_input`
  reInputFolder <- file.path(workingDirectory, "re_input")
  #' The default outputs or RE should be generated under `<workingDirectory>/re_output`
  reOutputFolder <- file.path(workingDirectory, "re_output")

  #' Configuration Plan created from the Qualification Plan by the Qualification Runner
  configurationPlanName <- "report-configuration-plan"
  configurationPlanFile <- file.path(reInputFolder, paste0(configurationPlanName, ".json"))

  #' Option to record the time require to run the workflow.
  #' The timer will calculate calculation time from internal `Sys.time()` function
  recordWorkflowTime <- TRUE

  #' Set watermark that will appear in all generated plots
  #' Default is no watermark. `Label` objects from `tlf` package can be used to specify watermark font
  watermark <- ""

  #' If not set, report created will be named `report.md` and located in the worflow folder namely `reOutputFolder`
  reportFolder <- file.path(workingDirectory, "report")
  reportPath <- file.path(reportFolder, "report.md")
  
  #----- Optional parameters for the Qualification Runner -----#
  #' If not null, `logFile` is passed internally via the `-l` option
  logFile <- NULL
  #' If not null, `logLevel` is passed internally via the `--logLevel` option
  logLevel <- NULL
  #' If `overwrite` is set to true, eventual results from the previous run of the QualiRunner/RE will be removed first
  overwrite <- TRUE

  #-------- STEP 2: Qualification Runner  --------#
  #' Start timer to track time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    tic <- as.numeric(Sys.time())
  }

  #' Start Qualification Runner to generate inputs for the reporting engine
  startQualificationRunner(
    qualificationRunnerFolder = qualificationRunnerFolder,
    qualificationPlanFile = qualificationPlanFile,
    outputFolder = reInputFolder,
    pkSimPortableFolder = pkSimPortableFolder,
    moBiPortableFolder = moBiPortableFolder,
    configurationPlanName = configurationPlanName,
    overwrite = overwrite,
    logFile = logFile,
    logLevel = logLevel
  )

  #' Print timer tracked time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    toc <- as.numeric(Sys.time())
    print(paste0("Qualification Runner Duration: ", round((toc - tic) / 60, 1), " minutes"))
  }

  #-------- STEP 3: Run Qualification Workflow  --------#
  # If version info is provided update title page
  titlePageFile <- file.path(reInputFolder, "Intro/titlepage.md") 
  addTitlePage <- all(
    !is.null(versionInfo),
    file.exists(titlePageFile)
  )
  if(addTitlePage){
    adjustTitlePage(titlePageFile, qualificationVersionInfo = versionInfo)
  }
  
  #' Load `QualificationWorkflow` object from configuration plan
  workflow <- loadQualificationWorkflow(
    workflowFolder = reOutputFolder,
    configurationPlanFile = configurationPlanFile
  )

  #' Set the name of the final report
  workflow$reportFilePath <- reportPath
  workflow$createWordReport <- createWordReport
  workflow$wordConversionTemplate <- wordConversionTemplate

  #' Set watermark. If set, it will appear in all generated plots
  workflow$setWatermark(watermark)

  #' Set the maximum number of simulations per core if defined
  if(!is.null(maxSimulationsPerCore)){
    workflow$simulate$settings$maxSimulationsPerCore <- maxSimulationsPerCore
  }
  
  #' @note Activate/Inactivate tasks of qualification workflow prior running
  #' workflow$inactivateTasks("simulate")
  #' workflow$inactivateTasks("calculatePKParameters")
  #' workflow$inactivateTasks("plotTimeProfiles")
  #' workflow$inactivateTasks("plotComparisonTimeProfile")
  #' workflow$inactivateTasks("plotGOFMerged")
  #' workflow$inactivateTasks("plotPKRatio")
  #' workflow$inactivateTasks("plotDDIRatio")
  
  #' Run the `QualificationWorkflow`
  workflow$runWorkflow()

  #' Print timer tracked time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    toc <- as.numeric(Sys.time())
    print(paste0("Qualification Workflow Total Duration: ", round((toc - tic) / 60, 1), " minutes"))
  }
  return(invisible())
}
```

## Creation of a word report

By default, the qualification workflow field `createWordReport` is set
to TRUE. This option requests [**Pandoc**](https://pandoc.org/) to
convert the report to a word version (with extension *docx*).

Note that [**Pandoc**
installation](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/wiki/Installing-pandoc)
is required for using this feature.

The conversion to word uses a reference word document that defines each
style of report (e.g. *Title*, *Heading 1*, *Normal*, etc.). Users can
input their own reference word document using the option
`wordConversionTemplate` as previously illustrated in the template. The
default reference word document can be found through the link
[reference.docx](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/articles/templates/reference.docx).

## Referencing

To create references in the final report, html tags are used by the
qualification workflow as illustrated below. Note that spaces should not
be used as reference ids.

``` html
<a id="my-reference-id"></a>
```

The markdown link to the reference above can then be written as:

``` md
[display text](#my-reference-id)
```

### Dynamic references

References for sections, figures and tables created by the qualification
workflow are generated dynamically.

For section references, the field **Reference** is used in priority by
the configuration plan to generate the reference id. In case, such
reference is not defined, the qualification workflow uses the **Id** of
the section. If no **Reference** nor **Id** is defined, the
qualification workflow generate a reference id named
`"undefined-section-<section index>"`.

Tables and figures are numbered internally within first level sections
(section whose title start with one markdown character `"#"` and are
also referenced).

The algorithm for numbering the figures and tables will look for the
keywords **`Figure:`** and **`Table:`** as first element of a line
(italic/bold characters are ignored by the algorithm if there is no
space between the keyword and the `"*"` or `"_"` characters).

Their references follow the nomenclature below:

- For Figures:
  `<a id="figure-<section number>-<internal figure count>"></a>`
- For Table:
  `<a id="table-<section number>-<internal table count>"></a>`

For instance, the second table encountered within section 3
(e.g. section in 3.1.4) would be referenced: `<a id="table-3-2"></a>`

### User-defined references

Users can define their own static references to include in the report.
For instance, user can cite articles in the report using exponent
notations as illustrated in the example below:

``` md
text related to article [<sup>1</sup>](#citations)

some text in between

<a id="citations"></a>

### Citations

<sup>1</sup> Cited article
```

which would render as below in the report:

text related to article [¹](#citations)

  
some text in between  

#### Citations

¹ Cited article
