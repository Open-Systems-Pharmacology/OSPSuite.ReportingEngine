# getGOFDataForMapping

Get data of goodness of fit from field `GOFMergedPlots` of configuration
plan

## Usage

``` r
getGOFDataForMapping(outputMapping, configurationPlan, axesUnits)
```

## Arguments

- outputMapping:

  list of mapping elements from `OutputMappings` field in configuration
  plan

- configurationPlan:

  A `ConfigurationPlan` object

- axesUnits:

  list of axes properties obtained from `getGOFAxesUnits`

## Value

A data.frame as obtained by `getResiduals` whose values are in base unit
