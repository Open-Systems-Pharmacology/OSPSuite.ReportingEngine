# getAxesSettings

Read axes settings for plots.

## Usage

``` r
getAxesSettings(axesSettingsFromConfigurationPlot)
```

## Arguments

- axesSettingsFromConfigurationPlot:

  is a field from the `configurationPlan$plots` list

## Value

`axesSettings`, a list of settings for each of the X and Y axis. Each
list contains the unit, dimensions, and scaling type for each axes and
option to plot grid lines.
