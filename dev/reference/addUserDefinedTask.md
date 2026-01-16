# addUserDefinedTask

Add a user-defined function as a task accessible in the `Workflow` field
`$userDefinedTasks`

## Usage

``` r
addUserDefinedTask(
  workflow,
  taskFunction,
  taskName = "userDefinedTask",
  active = TRUE,
  settings = NULL
)
```

## Arguments

- workflow:

  `Workflow` object or derived class

- taskFunction:

  Function to be called by `Task` object. For `MeanModelWorkflow`, input
  arguments of `taskFunction` should include: `simulationSet`, and
  `settings`. For `PopulationWorkflow`, input arguments of
  `taskFunction` should include: `simulationSets`, `xParameters`,
  `yParameters` and `settings`

- taskName:

  character name of task to initialize reporting of task

- active:

  logical to set if task is run by workflow

- settings:

  list of input arguments that can be used by `settings` input argument
  of `taskFunction`

## Value

Updated `Workflow` object

## See also

Other workflow tasks:
[`GofPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/GofPlotTask.md),
[`PlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PlotTask.md),
[`PopulationPlotTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationPlotTask.md),
[`PopulationSensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/PopulationSensitivityAnalysisTask.md),
[`QualificationTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationTask.md),
[`SensitivityAnalysisTask`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SensitivityAnalysisTask.md),
[`Task`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/Task.md),
[`loadCalculatePKParametersTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculatePKParametersTask.md),
[`loadCalculateSensitivityTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadCalculateSensitivityTask.md),
[`loadGOFMergedTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadGOFMergedTask.md),
[`loadPlotAbsorptionTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotAbsorptionTask.md),
[`loadPlotDDIRatioTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotDDIRatioTask.md),
[`loadPlotDemographyTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotDemographyTask.md),
[`loadPlotMassBalanceTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotMassBalanceTask.md),
[`loadPlotPKParametersTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotPKParametersTask.md),
[`loadPlotPKRatioTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotPKRatioTask.md),
[`loadPlotSensitivityTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotSensitivityTask.md),
[`loadPlotTimeProfilesAndResidualsTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadPlotTimeProfilesAndResidualsTask.md),
[`loadQualificationComparisonTimeProfileTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationComparisonTimeProfileTask.md),
[`loadQualificationTimeProfilesTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationTimeProfilesTask.md),
[`loadSimulateTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadSimulateTask.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# A workflow object needs to be created first
myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)

# Create a function to output results for workflow
# Mean model workflows functions usually expect SimulationStructure objects as inputs
userDefinedFunction <- function(structureSet, settings = NULL) {
  # Insert some code to calculate results and tune their output
  userResults <- list()
  userPlot <- ggplot()
  userTable <- data.frame()
  userText <- "Comments about the results of thMy user results"

  userResults[[1]] <- saveTaskResults(
    id = "user-result-1",
    plot = userPlot,
    plotCaption = "Title of Figure 1",
    includePlot = TRUE,
    table = userTable,
    tableCaption = "Title of Table 1",
    includeTable = TRUE,
    textChunk = userText,
    includeTextChunk = TRUE
  )

  # Tasks will run through the list of userResults
  # And add their outputs to the report
  return(userResults)
}

# Add a user-defined task to workflow
addUserDefinedTask(
  workflow = myWorkflow,
  taskFunction = userDefinedFunction,
  taskName = "userDefinedTask",
  active = TRUE
)

# Checks structure of task using
# myWorkflow$userDefinedTasks[[index]]$
} # }
```
