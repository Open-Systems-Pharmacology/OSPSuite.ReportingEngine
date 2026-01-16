# getLegendAesOverride

In time profiles, legends are merged into one unique legend The
displayed legend is stored in the `plotObject` within the color guide
field `override.aes`. This function simply gets the list from that field
for updating the current legend

## Usage

``` r
getLegendAesOverride(plotObject)
```

## Arguments

- plotObject:

  A ggplot object

## Value

A list of aesthetic values
