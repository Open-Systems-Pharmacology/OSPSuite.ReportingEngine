# ospsuite.reportingengine 2.0.0

## New features

- Qualification workflows are now available ! (#2)
  - Qualification Workflows have their own [vignette](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/articles/qualification-workflow.html) to help you start (#566)
  - A template qualification workflow R script is available on Github orat `system.file("extdata", "qualification-workflow-template.R", package = "ospsuite.reportingengine")` and can also be downloaded from [Github](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/blob/develop/inst/extdata/qualification-workflow-template.R) (#572)
  - Function `adjustTitlePage` can be used to personalize the report title page (#755)
  - Configuration plans can be re-loaded on `QualificationWorkflow` objects to update the report display (#567)
  - DDI subunits option is available (#642)
- With `ospsuite` version 10, simulations can be run in parallel (#526)
- Workflows account for time offset in simulation sets using option `timeOffset` for time profile plots (#313) or through user defined PK parameters for PK parameter plots (#578).
- User can define their own word report template to tune the styles of their report by providing a reference word document to the `wordConversionTemplate` of their Workflow object. A word reference template is available on [Github](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/blob/develop/inst/extdata/reference.docx) (#756)
- The function `copyReport` can be used to copy markdown/word reports (#814)

## Minor improvements and bug fixes

- With `tlf` version 1.2.0, plots in log scale are better displayed (#603)
- Too long legend captions are now displayed on multiple lines (#568, #602)
- `Output` objects are now cloneable (#542)
- In population workflows, option for adding reference observed data in time profile plots was created (#540).
- Confusing observed data terminology starting _nonmem_ was replaced by _dataset_ (#534)

# ospsuite.reportingengine 1.2.0

## New features

* Descriptor of simulation sets is now available and can be defined in Excel template (#445) as well as using the function `setSimulationDescriptor` (#423).
* Each task property `settings` is now an R6 object (#396). This allows users to have an easy and direct access to setting properties.
* Posibility to read time and measurement units from nonmem columns (#414)

## Minor improvements and bug fixes

* Calculation of time profile residuals can use __Linear__ or __Logarithmic__ scale (#395).
* Units for observed data is appropriately taken into account within dictionary (#414).
* Settings for number format within reports are now available (#424).
They can be updated in global settings using `setDefaultNumericFormat` or within specific tasks through their `settings` property.
* The `settings` property for task `plotTimeProfilesAndResiduals` include 
* Application Ranges can be switched on/off from SimulationSet objects regarding simulations with multiple administrations (#419)
* Population workflows: captions for tables (PK parameters) missing (#421)
* Population workflows: units for tables (PK parameters) missing (#422)
* Population/RatioComparison: box-Whisker Ratio plots (#425)


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
