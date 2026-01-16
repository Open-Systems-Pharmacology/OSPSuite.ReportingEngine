# displayConfigurationPlanPlotInfo

Display information of a configuration plan `Plot` field

## Usage

``` r
displayConfigurationPlanPlotInfo(configurationPlanField)
```

## Arguments

- configurationPlanField:

  A list extracted from a configuration plan field

## Value

Array of name properties and their value

## Examples

``` r
# Example of list with properties
plotField <- list(
  PlotNumber = 5,
  Simulation = "Name of Simulation",
  Project = "Name of Project",
  Options = list(width = 10, height = 10, units = "cm")
)

# Log messages are usually displayed through `cat()`, `error()` or `warning()`
cat(displayConfigurationPlanPlotInfo(plotField))
#> 
#> Configuration Plan Information:
#> PlotNumber: 5
#> Simulation: Name of Simulation
#> Project: Name of Project
#> Options: list(...)
```
