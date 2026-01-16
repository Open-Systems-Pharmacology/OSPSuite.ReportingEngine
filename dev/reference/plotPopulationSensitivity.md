# plotPopulationSensitivity

Retrieve list of plots of population sensitivity analyses across all
populations

## Usage

``` r
plotPopulationSensitivity(
  structureSets,
  settings,
  workflowType = PopulationWorkflowTypes$parallelComparison,
  xParameters = NULL,
  yParameters = NULL
)
```

## Arguments

- structureSets:

  list of `SimulationStructure` objects

- settings:

  list of settings for the population sensitivity plot

- workflowType:

  Element from `PopulationWorkflowTypes`

- xParameters:

  selected parameters to be plotted in x axis

- yParameters:

  selected parameters to be plotted in y axis

## Value

a structured list of plots for each possible combination of pathID
output-pkParameter that is found in sensitivity results index file
