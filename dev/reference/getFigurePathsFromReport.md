# getFigurePathsFromReport

Get file paths from a report figure links

## Usage

``` r
getFigurePathsFromReport(fileName)
```

## Arguments

- fileName:

  name of .md file to

## Value

array of file paths corresponding to figures linked in reports

## See also

Other reporting:
[`addFigureChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addFigureChunk.md),
[`addTableChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTableChunk.md),
[`addTextChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTextChunk.md),
[`anchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/anchor.md),
[`copyReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/copyReport.md),
[`getAnchorName()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getAnchorName.md),
[`hasAnchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/hasAnchor.md),
[`mergeMarkdownFiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/mergeMarkdownFiles.md),
[`renderReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderReport.md),
[`renderWordReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderWordReport.md),
[`resetReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/resetReport.md),
[`trimFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/trimFile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check the figure paths of your report named "report.md"
getFigurePathsFromReport("report.md")
} # }
```
