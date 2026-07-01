# getStatisticsFromType

Get statistics

## Usage

``` r
getStatisticsFromType(statisticsType)
```

## Arguments

- statisticsType:

  Statistics summarizing time profile simulated data as defined by
  helper enum `StatisticsType`

## Value

A list including `y`, `ymin` and `ymax` summary statistics as well as
their `caption`

## Examples

``` r
if (FALSE) { # \dontrun{
getStatisticsFromType(StatisticsTypes$`Arithmetic mean`)
} # }
```
