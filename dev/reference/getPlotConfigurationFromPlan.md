# getPlotConfigurationFromPlan

Get the appropriate `PlotConfiguration` object with scaled dimensions
for exporting it

## Usage

``` r
getPlotConfigurationFromPlan(
  plotProperties,
  plotType = NULL,
  legendPosition = NULL
)
```

## Arguments

- plotProperties:

  Plot properties from configuration plan

- plotType:

  Name of plot type to call the appropriate `PlotConfiguration` object.
  E.g. for pk ratio plots, use "PKRatio" to create a
  `PKRatioPlotConfiguration` object

- legendPosition:

  Legend position in order to add scale factor in the final plot
  dimensions that accounts for possible shrinking of the plot panel due
  to the addition of the legend

## Value

A `PlotConfiguration` object
