# addTableChunk

Add a table in a .md document

## Usage

``` r
addTableChunk(
  fileName,
  tableFileRelativePath,
  tableFileRootDirectory,
  digits = NULL,
  scientific = NULL,
  na = "-"
)
```

## Arguments

- fileName:

  name of .md file

- tableFileRelativePath:

  path to table relative to working directory

- tableFileRootDirectory:

  working directory

- digits:

  number of decimal digits in displayed numbers

- scientific:

  logical defining if displayed numbers use scientific writing

- na:

  character string replacing `NA` values in table

## See also

Other reporting:
[`addFigureChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addFigureChunk.md),
[`addTextChunk()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/addTextChunk.md),
[`anchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/anchor.md),
[`copyReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/copyReport.md),
[`getAnchorName()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getAnchorName.md),
[`getFigurePathsFromReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/getFigurePathsFromReport.md),
[`hasAnchor()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/hasAnchor.md),
[`mergeMarkdownFiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/mergeMarkdownFiles.md),
[`renderReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderReport.md),
[`renderWordReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderWordReport.md),
[`resetReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/resetReport.md),
[`trimFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/trimFile.md)
