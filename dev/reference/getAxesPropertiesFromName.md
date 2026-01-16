# getAxesPropertiesFromName

Get axes properties from global field `AxesSettings` of configuration
plan if defined, otherwise use default values.

## Usage

``` r
getAxesPropertiesFromName(configurationPlan, plotName)
```

## Arguments

- configurationPlan:

  A `ConfigurationPlan` object

- plotName:

  Field name of the plot in the configuration plan `AxesSettings`

## Value

A list of properties for axes identified for `x`, `y` and `y2` axes. The
identified properties are directly compatible with `tlf` package
nomenclature
