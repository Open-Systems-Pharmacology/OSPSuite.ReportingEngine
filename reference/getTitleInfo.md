# getTitleInfo

Get section titles information from report content

## Usage

``` r
getTitleInfo(fileContent, titlePattern = "#", titleLevels = 6)
```

## Arguments

- fileContent:

  Content of a markdown or text file read as an array of character
  strings

- titlePattern:

  character pattern referencing titles in first element of line

- titleLevels:

  levels of titles in the report

## Value

List of title information including `line`, `content`, `reference`,
`count`, `level`
