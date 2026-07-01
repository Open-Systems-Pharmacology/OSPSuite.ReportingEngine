# addLineBreakToCaption

Add line breaks to get prettier captions

## Usage

``` r
addLineBreakToCaption(
  captions,
  maxLines = reEnv$maxLinesPerLegendCaption,
  width = reEnv$maxWidthPerLegendCaption
)
```

## Arguments

- captions:

  Array of character strings to render

- maxLines:

  Maximum number of lines directly setting the maximum number of line
  breaks allowed.

- width:

  Maximum number of characters per line desired. Due to `maxLines`, the
  returned width can be wider than `width`.

## Value

A character vector of wrapped strings with line breaks at sensible
places.

## Examples

``` r
# Use cat to display result of line break character
cat(addLineBreakToCaption("this-is-a-long-sentence-with-dashes", maxLines = 2, width = 25))
#> this-is-a-long-
#> sentence-with-dashes

cat(addLineBreakToCaption("this is a sentence with spaces", maxLines = 2, width = 25))
#> this is a sentence 
#> with spaces

cat(addLineBreakToCaption(
  "this_is_a_long_sentence_without_preferential_splits",
  maxLines = 2, width = 25
))
#> this_is_a_long_sentence_w
#> ithout_preferential_splits

cat(addLineBreakToCaption("this too short to split", maxLines = 3, width = 40))
#> this too short to split

cat(addLineBreakToCaption("this forces the sentence to use one line", maxLines = 1, width = 5))
#> this forces the sentence to use one line
```
