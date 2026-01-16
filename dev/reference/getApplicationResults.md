# getApplicationResults

Get a data.frame of application results corresponding to total drug mass
as a function of time.

## Usage

``` r
getApplicationResults(applications)
```

## Arguments

- applications:

  list of `Application` objects queried by the method
  `simulation$allApplicationsFor()`

## Value

A data.frame that includes `time`, `drugMass` and `totalDrugMass` as
variables
