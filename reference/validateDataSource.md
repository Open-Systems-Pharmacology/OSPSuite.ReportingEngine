# Check the consistency between observed data and its dictionary. Units for `dv `and `time` need to be defined at least once in either the observed dataset, its dictionary or outputs In case of multiple definitions, warnings will thrown and the following priorities will be applied:

1.  Use units from outputs

2.  Use units from observed dataset

3.  Use units from dictionary

Check the consistency between observed data and its dictionary. Units
for `dv `and `time` need to be defined at least once in either the
observed dataset, its dictionary or outputs In case of multiple
definitions, warnings will thrown and the following priorities will be
applied:

1.  Use units from outputs

2.  Use units from observed dataset

3.  Use units from dictionary

## Usage

``` r
validateDataSource(dataSource, outputs, nullAllowed = TRUE)
```

## Arguments

- dataSource:

  A `DataSource` object

- outputs:

  list or array of `Output` objects

- nullAllowed:

  Logical defining if `NULL` input is allowed
