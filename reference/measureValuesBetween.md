# measureValuesBetween

Measure the values of `x` between `left` and `right` bounds according to
`method`.

## Usage

``` r
measureValuesBetween(x, left, right, method = "count", strict = FALSE)
```

## Arguments

- x:

  Numeric values to assess

- left:

  Numeric value(s) used as lower bound

- right:

  Numeric value(s) used as upper bound

- method:

  One of the following methods `"count"`, `"ratio"`, and `"percent"`.

- strict:

  Logical value defining if `x` is strictly between `left` and `right`.
  Default value is `FALSE`.

## Value

Measure of `x` values between `left` and `right` bounds

## Examples

``` r
measureValuesBetween(1:12, 7, 9)
#> [1] 3
measureValuesBetween(1:12, 7, 9, method = "percent")
#> [1] 25

x <- rnorm(1e2)
measureValuesBetween(x, -1, 1)
#> [1] 67
measureValuesBetween(x, -1, 1, method = "ratio")
#> [1] 0.67

measureValuesBetween(x, cos(x) + 1, cos(x) - 1)
#> [1] 0
```
