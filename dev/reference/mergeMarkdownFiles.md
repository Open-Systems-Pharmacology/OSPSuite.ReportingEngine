# mergeMarkdownFiles

Merge markdown files into one unique file

## Usage

``` r
mergeMarkdownFiles(inputFiles, outputFile, keepInputFiles = FALSE)
```

## Arguments

- inputFiles:

  names of .md files to merge

- outputFile:

  name of merged .md file

- keepInputFiles:

  logical option to prevent the input files to be deleted after merging
  them

## See also

Other reporting:
[`addFigureChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addFigureChunk.md),
[`addTableChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTableChunk.md),
[`addTextChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTextChunk.md),
[`anchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/anchor.md),
[`copyReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/copyReport.md),
[`getAnchorName()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getAnchorName.md),
[`getFigurePathsFromReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getFigurePathsFromReport.md),
[`hasAnchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/hasAnchor.md),
[`renderReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderReport.md),
[`renderWordReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderWordReport.md),
[`resetReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/resetReport.md),
[`trimFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/trimFile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
resetReport("chapter-1.md")
addTextChunk(fileName = "chapter-1.md", text = "Chapter 1")
resetReport("chapter-2.md")
addTextChunk(fileName = "chapter-2.md", text = "Chapter 2")
mergeMarkdownFiles(
  inputFiles = c("chapter-1.md", "chapter-2.md"),
  outputFile = "chapters-1and2.md"
)
} # }
```
