# validateHasRunOnAllCores

Validate if all cores executed an mpi.remote.exec command successfully.

## Usage

``` r
validateHasRunOnAllCores(coreResults, inputName, inputType, runType = "load")
```

## Arguments

- coreResults:

  list of logical results returned by each core after an mpi.remote.exec
  command is complete

- inputName:

  Name of the input to be loaded

- inputType:

  Type of input to be loaded

- runType:

  Type of run executed on `{Rmpi}` cores
