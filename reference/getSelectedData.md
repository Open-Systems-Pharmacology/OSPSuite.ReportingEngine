# getSelectedData

Get selected data The function leverage
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) to
select the data

## Usage

``` r
getSelectedData(data, dataSelection)
```

## Arguments

- data:

  A data.frame

- dataSelection:

  Character string or expression evaluated to select data The enum
  helper `DataSelectionKeys` provides keys for selected all or none of
  the data

## Value

A data.frame of selected data

## See also

DataSelectionKeys

## Examples

``` r
data <- data.frame(
  x = seq(0, 9),
  y = seq(10, 19),
  mdv = c(1, 1, rep(0, 8)),
  groups = rep(c("A", "B"), 5)
)

# Select all the data
getSelectedData(data, DataSelectionKeys$ALL)
#>    x  y mdv groups
#> 1  0 10   1      A
#> 2  1 11   1      B
#> 3  2 12   0      A
#> 4  3 13   0      B
#> 5  4 14   0      A
#> 6  5 15   0      B
#> 7  6 16   0      A
#> 8  7 17   0      B
#> 9  8 18   0      A
#> 10 9 19   0      B

# Select no data
getSelectedData(data, DataSelectionKeys$NONE)
#> [1] x      y      mdv    groups
#> <0 rows> (or 0-length row.names)

# Select data from group A
getSelectedData(data, "groups %in% 'A'")
#>   x  y mdv groups
#> 1 0 10   1      A
#> 2 2 12   0      A
#> 3 4 14   0      A
#> 4 6 16   0      A
#> 5 8 18   0      A

# Remove missing dependent variable (mdv)
getSelectedData(data, "mdv == 0")
#>   x  y mdv groups
#> 1 2 12   0      A
#> 2 3 13   0      B
#> 3 4 14   0      A
#> 4 5 15   0      B
#> 5 6 16   0      A
#> 6 7 17   0      B
#> 7 8 18   0      A
#> 8 9 19   0      B
```
