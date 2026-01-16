# getObservedDataIdFromPath

Get an observed dataset id from a configuration plan quantity path

## Usage

``` r
getObservedDataIdFromPath(path)
```

## Arguments

- path:

  A quantity path from the configuration plan For instance,
  "S2\|Organism\|PeripheralVenousBlood\|Midazolam\|Plasma (Peripheral
  Venous Blood)" or "Midazolam 600mg SD\|ObservedData\|Peripheral Venous
  Blood\|Plasma\|Rifampin\|Conc"

## Value

A string corresponding to the `id` of a configuration plan observed
dataset

## Examples

``` r
if (FALSE) { # \dontrun{
getObservedDataIdFromPath("S2|Organism|PeripheralVenousBlood|Midazolam|Plasma")
# > NULL
getObservedDataIdFromPath("Midazolam 600mg SD|ObservedData|Plasma|Rifampin|Conc")
# > "Midazolam 600mg SD"
} # }
```
