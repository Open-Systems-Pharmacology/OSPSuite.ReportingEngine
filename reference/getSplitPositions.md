# getSplitPositions

Algorithm that gets positions where splitting a character string for
sensible line breaks

## Usage

``` r
getSplitPositions(possibleSplits, splitWidth, numberOfSplits)
```

## Arguments

- possibleSplits:

  Positions where a space or a dash was found

- splitWidth:

  Maximum number of characters desired per lines

- numberOfSplits:

  Maximum number of line breaks to use

## Value

Position where to insert a line break character
