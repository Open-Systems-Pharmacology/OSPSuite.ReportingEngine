# updateArtifactNumbers

Update artifact (figure or table) captions and references in report

## Usage

``` r
updateArtifactNumbers(
  fileContent,
  pattern,
  replacement,
  anchorId,
  captionBelow = FALSE
)
```

## Arguments

- fileContent:

  Content of a markdown or text file read as an array of character
  strings

- pattern:

  character pattern referencing figures in first element of line

- replacement:

  character replacing pattern in updated caption name

- anchorId:

  character pattern referencing anchor tags

- captionBelow:

  logical defining if caption is below artifact

## Value

Array of character strings
