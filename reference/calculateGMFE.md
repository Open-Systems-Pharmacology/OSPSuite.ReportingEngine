# calculateGMFE

Calculate Geometric Mean Fold Error between `x` and `y`. Strictly
positive pairs of values are kept in the calculation

## Usage

``` r
calculateGMFE(x, y)
```

## Arguments

- x:

  x values to compare

- y:

  y values to compare

## Value

GMFE

## Examples

``` r
# GMFE
calculateGMFE(c(1, 2, 2.1), c(1.3, 1.9, 3.0))
#> [1] 1.250376
```
