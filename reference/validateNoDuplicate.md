# validateNoDuplicate

Leverage
[`ospsuite.utils::validateHasOnlyDistinctValues()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/hasOnlyDistinctValues.html)
to validate that a vector has only distinct values and display a useful
message.

## Usage

``` r
validateNoDuplicate(
  values,
  variableName = NULL,
  na.rm = TRUE,
  nullAllowed = FALSE
)
```

## Arguments

- values:

  An array to validate

- variableName:

  Name of variable that can be used to display a useful message

- na.rm:

  logical indicating if `NA` values should be removed before the check

- nullAllowed:

  logical indicating if `NULL` values should be allowed
