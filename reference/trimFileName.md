# trimFileName

Trim path and extension of a file

## Usage

``` r
trimFileName(path, extension = NULL, sep = "/")
```

## Arguments

- path:

  character string containing the name of the path or file to trim

- extension:

  character string containing the extension file

- sep:

  character string separating path elements. "/" is default value.

## Value

fileName character string of the trimmed filed name

## Examples

``` r
if (FALSE) { # \dontrun{
pathName <- "folder/subfolder/testFile.txt"
trimFileName(pathName, extension = "txt")
} # }
```
