# renderWordReport

Render docx report with number sections and table of content

## Usage

``` r
renderWordReport(
  fileName,
  intro = NULL,
  createWordReport = FALSE,
  wordConversionTemplate = NULL
)
```

## Arguments

- fileName:

  name of .md file to render

- intro:

  name of .md file that include introduction (before toc)

- createWordReport:

  option for creating Markdown-Report only but not a Word-Report

- wordConversionTemplate:

  optional docx template for rendering a tuned Word-Report document

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
[`mergeMarkdownFiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/mergeMarkdownFiles.md),
[`renderReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/renderReport.md),
[`resetReport()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/resetReport.md),
[`trimFile()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/trimFile.md)
