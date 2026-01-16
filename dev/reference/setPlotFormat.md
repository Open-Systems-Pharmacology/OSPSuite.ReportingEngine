# setPlotFormat

Set default plot format to be exported by reporting engine

## Usage

``` r
setPlotFormat(
  format = NULL,
  width = NULL,
  height = NULL,
  units = NULL,
  dpi = NULL
)
```

## Arguments

- format:

  file format of the exported plots. E.g. "png" or "pdf"

- width:

  plot width in `unit`

- height:

  plot height in `unit`

- units:

  units of `width` and `height` included in "in", "cm", "mm", or "px"

- dpi:

  Plot resolution in dots per inch. Caution, font sizes depend on
  resolution.
