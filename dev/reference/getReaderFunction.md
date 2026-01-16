# getReaderFunction

Get most appropriate reader function by guessing file separator File
separator is guessed by checking number of fields/columns along lines

## Usage

``` r
getReaderFunction(fileName, nlines = 2)
```

## Arguments

- fileName:

  Name of file to be read

- nlines:

  Number of lines to look at for checking consistency within file. Note
  that, unlike `read.csv`, `count.fields` does not have options for
  handling encoding or escape characters. Thus, using `nlines` as low as
  possible reduces the chances of inconsistent column widths caused by
  such characters.

## Value

Reader function such as `read.csv`
