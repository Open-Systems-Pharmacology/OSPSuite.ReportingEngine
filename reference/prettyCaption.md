# prettyCaption

Get prettied captions with line breaks to prevent cropping of long
captions

## Usage

``` r
prettyCaption(captions, plotObject, element = "legend")
```

## Arguments

- captions:

  Array of character strings to render

- plotObject:

  A `ggplot` object

- element:

  The name of element to which the line break should be added. If
  applied to the legend, use `"legend"`. If applied to a plot label use
  e.g. `"ylabel"` or `"title"`. If applied to tick labels use `"yticks"`
  or `"yticklabels"`.

## Value

A character vector of wrapped strings with line breaks at sensible
places.
