# getDefaultPKParametersXParameters

Get names of default parameters in x axis of pk parameters plots.

## Usage

``` r
getDefaultPKParametersXParameters(workflowType)

getDefaultPkParametersXParameters(workflowType)
```

## Arguments

- workflowType:

  Name of workflow type. Use enum `PopulationWorkflowTypes` to get a
  list of available workflow types.

## Value

names of default parameters

## Examples

``` r
getDefaultPKParametersXParameters(PopulationWorkflowTypes$pediatric)
#> $Age
#> [1] "Organism|Age"
#> 
#> $Height
#> [1] "Organism|Height"
#> 
#> $Weight
#> [1] "Organism|Weight"
#> 
#> $BMI
#> [1] "Organism|BMI"
#> 
#> $Gender
#> [1] "Gender"
#> 
```
