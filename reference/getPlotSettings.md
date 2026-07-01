# getPlotSettings

Read plot settings from configuration plan.

## Usage

``` r
getPlotSettings(plotSettingsFromConfigurationPlot)
```

## Arguments

- plotSettingsFromConfigurationPlot:

  is a field from the `configurationPlan$plots` list

## Value

`plotSettings`, a list of settings for each of the X and Y axis. Each
list contains the unit, dimensions, and scaling type for each axes and
option to plot grid lines.
