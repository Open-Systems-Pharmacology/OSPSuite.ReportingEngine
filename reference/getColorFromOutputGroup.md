# getColorFromOutputGroup

Get the appropriate colors from an output group

## Usage

``` r
getColorFromOutputGroup(
  group,
  data,
  dataMapping,
  legendVariable = "legend",
  colorVariable = "color"
)
```

## Arguments

- group:

  A data.frame mapping properties to output groups

- data:

  A data.frame

- dataMapping:

  A `DataMapping` object

- legendVariable:

  Name of legend variable in`group`

- colorVariable:

  Name of color variable in`group`

## Value

A sorted array of color values
