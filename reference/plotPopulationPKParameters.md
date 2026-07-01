# plotPopulationPKParameters

Plot PK parameters box plots and tables

## Usage

``` r
plotPopulationPKParameters(
  structureSets,
  settings = NULL,
  workflowType = PopulationWorkflowTypes$parallelComparison,
  xParameters = getDefaultPkParametersXParameters(workflowType),
  yParameters = NULL
)
```

## Arguments

- structureSets:

  `SimulationStructure` R6 class object

- settings:

  list of settings for the output table/plot

- workflowType:

  workflowType Type of population workflow. Use enum
  `PopulationWorkflowTypes` to get list of workflow types.

- xParameters:

  list of parameters to be plotted along x axis

- yParameters:

  list of parameters to be plotted along y axis

## Value

list of plots and tables with summary of PK parameters
