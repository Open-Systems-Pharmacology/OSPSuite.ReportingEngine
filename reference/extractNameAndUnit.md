# extractNameAndUnit

Returns a named list with two entries (name, unit) corresponding to the
name and unit extracted out of the `text` provided as parameter

## Usage

``` r
extractNameAndUnit(text)
```

## Arguments

- text:

  Character from which name and unit are extracted

## Value

A named list, with fields `name` and `unit`.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- extractNameAndUnit("Value [mg]")
res$name
# > "Value"
res$unit
# > "mg"

res <- extractNameAndUnit("Value")
res$name
# > "Value"
res$unit
# > ""
} # }
```
