# getIntroFromReportTitle

Get introduction file used as cover page of report

## Usage

``` r
getIntroFromReportTitle(reportTitle = NULL, intro = "temp-report-title.md")
```

## Arguments

- reportTitle:

  Report title page If `reportTitle` is an existing file, its content
  will be used as cover page. If `reportTitle` is one character string,
  it is assumed as a title. Thus the markdown title tag is internally
  added to `reportTitle`. If `reportTitle` is multiple character
  strings, it is assumed as the cover page content. and used *as is* in
  the cover page.

- intro:

  temporary introduction file deleted when merged to final report
  Parameter is named intro to stay consistent with Qualification
  Workflow nomenclature

## Value

`intro`, path of temporary introduction file if created
