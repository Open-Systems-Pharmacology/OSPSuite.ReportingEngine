# getQualificationGOFPlot

Get goodness of fit plot

## Usage

``` r
getQualificationGOFPlot(
  plotType,
  data,
  metaData,
  axesProperties,
  plotProperties
)
```

## Arguments

- plotType:

  Name of PK Parameter as defined by users

- data:

  data.frame with PK Ratios

- metaData:

  metaData with units and dimension for labeling the table header

- axesProperties:

  list of axes properties obtained from `getAxesProperties`

- plotProperties:

  list of plot properties defined in field `Plot` of GOFMerged
  configuration plan

## Value

A ggplot object
