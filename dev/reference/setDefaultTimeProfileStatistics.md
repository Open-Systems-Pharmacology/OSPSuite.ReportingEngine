# setDefaultTimeProfileStatistics

Set default statistics used in population time profiles and residuals
plots

## Usage

``` r
setDefaultTimeProfileStatistics(
  statisticsType = NULL,
  y = NULL,
  ymin = NULL,
  ymax = NULL,
  yCaption = NULL,
  rangeCaption = NULL
)
```

## Arguments

- statisticsType:

  Name of statistics type as defined in enum `StatisticsTypes`

- y:

  Function or function name for middle values statistics

- ymin:

  Function or function name for min values statistics

- ymax:

  Function or function name for max values statistics

- yCaption:

  Legend caption for middle values statistics

- rangeCaption:

  Legend caption for range values statistics

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the default statistics as geometric mean
setDefaultTimeProfileStatistics(statisticsType = StatisticsTypes$`Geometric mean`)

# Set the default legend caption displayed for range
setDefaultTimeProfileStatistics(rangeCaption = "90% population range")
} # }
```
