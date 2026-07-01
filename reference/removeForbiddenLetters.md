# removeForbiddenLetters

Trim path and extension of a file

## Usage

``` r
removeForbiddenLetters(
  text,
  forbiddenLetters = "[[:punct:][:blank:]]",
  replacement = "_"
)
```

## Arguments

- text:

  character string to be evaluated

- forbiddenLetters:

  characters to be removed if in the `text`. Default value of
  `forbiddenLetters` is `"[[:punct:]]"` meaning that all punctuation
  characters are forbidden.

- replacement:

  character replacing the `forbiddenLetters`. Default value of
  `forbiddenLetters` is "\_".

## Value

`text` character string with forbidden letters replaced

## Examples

``` r
if (FALSE) { # \dontrun{
removeForbiddenLetters(text)
} # }
```
