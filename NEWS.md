# ospsuite.reportingengine 1.1.0

## New features

* `createWorkflowFromExcelInput` writes a commented workflow script ready to run based on Excel input file (#25). 
An Excel input file template is available at `system.file("extdata", "WorkflowInput.xlsx", package = "ospsuite.reportingengine")`

```R
excelFile <- system.file("extdata", "WorkflowInput.xlsx", package = "ospsuite.reportingengine")
workflowFile <- createWorkflowFromExcelInput(excelFile)
```

* `setWorkflowParameterDisplayPathsFromFile` overwrites display path names for simulation parameters in workflow `plotDemography` and `plotPKParameters` tasks (#399).
The input needs to be a csv file with `parameter` and `displayPath` in its header.

## Minor improvements and bug fixes

* Default `settings` in workflow `plotDemography` and `plotPKParameters` tasks improved the binning (#383).
* `bins` and `stairstep` are now included in `settings` options of workflow `plotDemography` and `plotPKParameters` tasks (#383).
In addit
