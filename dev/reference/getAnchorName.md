# getAnchorName

Get the name/identifier an anchor tag

## Usage

``` r
getAnchorName(tag)
```

## Arguments

- tag:

  Character string

## Value

A character Name/identifier of the anchor tag

## See also

Other reporting:
[`addFigureChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addFigureChunk.md),
[`addTableChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTableChunk.md),
[`addTextChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTextChunk.md),
[`anchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/anchor.md),
[`copyReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/copyReport.md),
[`getFigurePathsFromReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getFigurePathsFromReport.md),
[`hasAnchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/hasAnchor.md),
[`mergeMarkdownFiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/mergeMarkdownFiles.md),
[`renderReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderReport.md),
[`renderWordReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderWordReport.md),
[`resetReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/resetReport.md),
[`trimFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/trimFile.md)

## Examples

``` r
getAnchorName('<a id="section-1"></a>')
#> [1] "section-1"

# Works also for tag name instead of id
getAnchorName('<a name="section-1"></a>')
#> [1] "section-1"
```
