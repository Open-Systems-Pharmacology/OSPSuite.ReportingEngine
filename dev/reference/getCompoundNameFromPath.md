# getCompoundNameFromPath

Get the compound name from a configuration plan quantity path

## Usage

``` r
getCompoundNameFromPath(path)
```

## Arguments

- path:

  A quantity path from the configuration plan For instance,
  "S2\|Organism\|PeripheralVenousBlood\|Midazolam\|Plasma (Peripheral
  Venous Blood)" or "Midazolam 600mg SD\|ObservedData\|Peripheral Venous
  Blood\|Plasma\|Rifampin\|Conc"

## Value

A string corresponding to the compound name of a configuration plan
quantity path

## Examples

``` r
if (FALSE) { # \dontrun{
getCompoundNameFromPath("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma")
# > "Midazolam"
getCompoundNameFromPath("Midazolam SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc")
# > "Rifampin"
} # }
```
