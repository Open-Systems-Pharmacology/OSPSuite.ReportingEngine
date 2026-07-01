# getSelectedRows

Get selected rows from data and its selection The function leverage
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) to
select the rows

## Usage

``` r
getSelectedRows(data, dataSelection)
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

# Select all the rows
getSelectedRows(data, DataSelectionKeys$ALL)
#> [1] TRUE

# Select no row
getSelectedRows(data, DataSelectionKeys$NONE)
#> [1] FALSE

# Select rows from group A
getSelectedData(data, "groups %in% 'A'")
#>   x  y mdv groups
#> 1 0 10   1      A
#> 2 2 12   0      A
#> 3 4 14   0      A
#> 4 6 16   0      A
#> 5 8 18   0      A

# Get rows of missing dependent variable (mdv)
getSelectedRows(data, "mdv == 0")
#> [1]  3  4  5  6  7  8  9 10
```
