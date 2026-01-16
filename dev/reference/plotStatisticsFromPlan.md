# plotStatisticsFromPlan

Add summary statistics to a time profile plot from configuration plan

## Usage

``` r
plotStatisticsFromPlan(
  time,
  outputValues,
  statisticId,
  outputName,
  color,
  linetype,
  plotObject
)
```

## Arguments

- time:

  Time values on which output values are aggregated

- outputValues:

  Output values to be aggregated

- statisticId:

  Statistic Id as defined in `ConfigurationPlan` used for data
  aggregation

- outputName:

  Display name of output

- color:

  Color of the line or ribbon

- linetype:

  Linetype of the line

- plotObject:

  A `ggplot` object with previous statistics displayed

## Value

A `ggplot` object updated with new displayed statistic
