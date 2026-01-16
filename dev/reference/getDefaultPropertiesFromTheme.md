# getDefaultPropertyFromTheme

Get default property value from current reEnv theme

## Usage

``` r
getDefaultPropertiesFromTheme(
  plotName,
  propertyType = "points",
  propertyNames = as.character(tlf::AestheticProperties)
)
```

## Arguments

- plotName:

  Name of the plot in Theme (e.g. `"plotTimeProfile"`)

- propertyType:

  One of `"points"`, `"lines`, `"ribbons"` or `"errorbars"`

- propertyNames:

  Names of the aesthetic property (e.g. `"color"`)

## Value

Property value
