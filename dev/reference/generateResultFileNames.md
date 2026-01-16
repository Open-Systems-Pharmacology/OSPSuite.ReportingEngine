# generateResultFileNames

\#Generate a list containing names of CSV result files that will be
output by each core in parallel computation

## Usage

``` r
generateResultFileNames(
  numberOfCores,
  folderName,
  fileName,
  separator = "-",
  extension = ".csv"
)
```

## Arguments

- numberOfCores:

  to be used in parallel computation

- folderName:

  where result files will be saved

- fileName:

  prefix of result file names

- separator:

  used between file name prefix and index

- extension:

  for result file type. default is CSV

## Value

A list of filenames to be output by each core
