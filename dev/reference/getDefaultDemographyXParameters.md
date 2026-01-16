# getDefaultDemographyXParameters

Get names of default demography parameters in x axis of demography
plots.

## Usage

``` r
getDefaultDemographyXParameters(workflowType)
```

## Arguments

- workflowType:

  Name of workflow type. Use enum `PopulationWorkflowTypes` to get a
  list of available workflow types.

## Value

names of default demography parameters

## Examples

``` r
getDefaultDemographyXParameters(PopulationWorkflowTypes$pediatric)
#> [1] "Organism|Age"
```
