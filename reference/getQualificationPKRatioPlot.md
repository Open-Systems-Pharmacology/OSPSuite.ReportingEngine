# getQualificationPKRatioPlot

Get plot of pk ratio from field `PKRatioPlots` of configuration plan

## Usage

``` r
getQualificationPKRatioPlot(
  pkParameterName,
  data,
  metaData,
  axesProperties,
  plotProperties
)
```

## Arguments

- pkParameterName:

  Name of PK Parameter as defined by users

- data:

  data.frame with PK Ratios

- metaData:

  metaData with units and dimension for labeling the table header

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- plotProperties:

  list of plot properties defined in field `Plot` of PKRatio
  configuration plan

## Value

A ggplot object
