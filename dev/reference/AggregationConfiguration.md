# AggregationConfiguration

To be deprecated: Aggregation default properties (which functions and
their captions).

## Usage

``` r
AggregationConfiguration
```

## Format

An object of class `list` of length 4.

## Fields

- `functions`:

  list of `middle`, `ymin` and `ymax` functions for aggregation

- `names`:

  list of legend captions for `middle` and `range` from aggregation

- `bins`:

  default number of bins in plots

- `binUsingQuantiles`:

  logical to choose a binning based on the quantiles rather than on a
  constant interval width
