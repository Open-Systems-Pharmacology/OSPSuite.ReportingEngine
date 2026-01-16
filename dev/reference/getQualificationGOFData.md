# getQualificationGOFData

Get data of goodness of fit from field `GOFMergedPlots` of configuration
plan

## Usage

``` r
getQualificationGOFData(gofPlan, configurationPlan, axesUnits)
```

## Arguments

- gofPlan:

  List providing the mapping of observed and simulated data

- configurationPlan:

  A `ConfigurationPlan` object

- axesUnits:

  list of axes properties obtained from `getGOFAxesUnits`

## Value

list with `data` and `metaData`
