# getDemographyAggregatedData

getDemographyAggregatedData

## Usage

``` r
getDemographyAggregatedData(
  data,
  xParameterName,
  yParameterName,
  groupName = NULL,
  bins = NULL,
  stairstep = TRUE
)
```

## Arguments

- data:

  A data.frame

- xParameterName:

  Name of parameter in `data` used for aggregation in x axis of plot

- yParameterName:

  Name of parameter in `data` aggregated in y axis of plot

- groupName:

  Name of parameter in `data` aggregated for grouping

- bins:

  Either a numeric vector defining bin edges or a numeric value defining
  the number of bins.

- stairstep:

  A logical value defining if aggregation uses continuous or stairstep
  plot

## Value

A data.frame of aggregated data
