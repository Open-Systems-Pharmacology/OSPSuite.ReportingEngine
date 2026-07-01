# isObservedData

Check if a configuration plan quantity path corresponds to observed data

## Usage

``` r
isObservedData(path)
```

## Arguments

- path:

  A quantity path from the configuration plan For instance,
  "S2\|Organism\|PeripheralVenousBlood\|Midazolam\|Plasma (Peripheral
  Venous Blood)" or "Midazolam 600mg SD\|ObservedData\|Peripheral Venous
  Blood\|Plasma\|Rifampin\|Conc"

## Value

A logical checking if path corresponds to observed data

## Examples

``` r
if (FALSE) { # \dontrun{
isObservedData("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)")
# > FALSE
isObservedData("Midazolam 600mg SD|ObservedData|Peripheral Venous Blood|Plasma|Rifampin|Conc")
# > TRUE
} # }
```
