# getLineBreakWidth

Calculate the maximum number of characters before breaking lines. This
aims at preventing as much as possible legends shrinking the plot and
legends not fully displayed

## Usage

``` r
getLineBreakWidth(element = "legend", plotConfiguration)
```

## Arguments

- element:

  The name of element to which the line break should be added. If
  applied to the legend, use `"legend"`. If applied to a plot label use
  e.g. `"ylabel"` or `"title"`. If applied to tick labels use `"yticks"`
  or `"yticklabels"`.

- plotConfiguration:

  A `PlotConfiguration` object from the `tlf` package

## Value

An integer as max character width before using line breaks
