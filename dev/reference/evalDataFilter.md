# evalDataFilter

Evaluate a data filter by converting the variable names of the
data.frame into names of variables to be evaluated in the filter
expression.

## Usage

``` r
evalDataFilter(data, filterExpression)
```

## Arguments

- data:

  data.frame containing observed data

- filterExpression:

  expression character string filter to be applied

## Value

vector of logicals corresponding to the evaluation of the filter
