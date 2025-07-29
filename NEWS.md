# ospsuite.reportingengine (development version)

# ospsuite.reportingengine 2.3.11

## Minor improvements and bug fixes

- Fixed an error in `trimFileName()` that caused the population workflow to break in R 4.5.0. (\#1327)
- Inconsistent exported image size fixed (\#1299)
- Fixed typos in **qualification-workflow-template.R** (\#1334)
- Fixed different axis font sizes when using two y-axes (\#1333)
- Removed an unnecessary warning when the time offset is 0 (\#1289)
- Fixed incorrect values in DDI ratio plots when the PK parameter is `AUC` and the end time is `Inf` (\#1342)
- Fixed SD ranges not being displayed correctly in some comparison time profiles with time shift (\#1343)
- Updated compatibility with **cowplot** version >= 1.2.0 to fix alignment issues in multi-panel plots

# ospsuite.reportingengine 2.2.0

## New features

> [!NOTE]
> Users can now check out [Reporting Engine Test Reports](https://github.com/Open-Systems-Pharmacology/RE-Test-Reports) and access a bunch of template workflows including their R code, models, data and corresponding reports.

- Demography task received significant updates:
   - The [article for demography](../articles/demography.html) has been updated with more comprehensive examples using more representative virtual populations and illustrating the effects of population workflows and settings (#1095, #1102)
   - Categorical parameters, such as Gender, are now accounted for demography range plots and displayed as boxplots (#1088)
   - Demography plots can now display observed data (#535)
- Mass Balance task received significant updates:
   - A dedicated [article for mass balance](../articles/mass-balance.html) has been created in the documentation (#1118)
   - Mass balance plots can leverage user-defined settings from json files as illustrated in the [mass balance article](../articles/mass-balance.html) (#1118)
   - Normalized mass balance plots now use time-dependent cumulative drug mass for normalization (#1039, #1118)
- Qualification DDI plots received the following improvements
   - Delta symbol (&delta;) from Guest *et al.* formula is indicated in table caption (#1069)
   - Guest criterion (&delta;) can be tuned through the configuration plan (#789)
   - DDI plot format is quadratic by default (#1051)
- Formatting to word reports can translate additional `html` tags (#382)
- Goodness of Fit task leverages `groupID` from `Output` objects to group multiple outputs in same time profile and residual plots (#1188).<br>`groupID` is also leveraged in the plots of residuals across the simulations (#1251).

## Minor improvements and bug fixes

- Monte Carlo sampling in ratio comparison workflows is now centralized, performed in the PK parameter calculation step and significantly faster (#536, #1086).
- Wording for ratio comparison workflow figures and tables has been updated (#1087)
- Number of bins in demography and PK parameter range plots is consistent with input (#1128)
- Link/bookmark targets in word reports are better placed (#1084)
- Qualification time profiles work when no observed data is contained (#1082)
- Qualification PKRatio uses *ml/min/kg* as default clearance - *CL* - unit (#1242)
- Static images in qualification reports are better handled (#955, #1071)
- Figure captions are displayed below the figure in the reports (#1053)
- Word report is updated if it already existed (#1055)
- LLOQ line is better displayed in the legend of time profile plots (#478)


# ospsuite.reportingengine 2.1.244

## New features

- Population workflows can use Monte Carlo sampling to handle ratio comparison (#536) as documented in the article [PK Parameters in Population Workflows](../articles/pop-pk-parameters.html#ratio-comparison).
- [`Output`](../reference/Output.html) objects can now include `color` and `fill` properties that will be displayed in the corresponding plots (#418, #883)
- `dataSelection` is now available at both `SimulationSet` and `Output` levels (#881)
- Excel template is up-to-date with the new features (#882) and documented in the article [Excel Template](../articles/excel-template.html)
- Mean and Population workflows can display a title/cover page at the beginning of the report (#854) as documented in article [Add a title page to your workflow report](../articles/add-title-page.html)
- Qualification Time Profiles handle dual axis plots (#934, #985)

## Minor improvements and bug fixes

- In qualification workflows, logging now includes information regarding the configuration plan location (#930)
- By default, plot dimensions are now quadratic (#877)
- Statistics in population workflow time profiles now use geometric mean and sd as default and can be easily updated with enum helper [`StatisticsTypes `](../reference/StatisticsTypes.html) and function `setDefaultTimeProfileStatistics()` (#862)
- Goodness of fit plots and captions were improved (#418, #863, #865, #867, #871, #874, #875, #878, #884, #886, #887, #888)
- Displayed qualification plots now includes following updates:
   - better scaled watermark (#849)
   - margin added to prevent truncated ticklabels (#951)
   - gof and time profile use correct colors (#952)
   - residual label match calculation method (#953)
   - legend does not shrink the plot size (#861)
- Creation of word reports now includes following updates:
   - page breaks at appropriate places (#971)
   - figure/table numbering (#967) are now fixed
   - default reference word document has been updated (#748, #749, #750 and #754)
   - tags for subscript (wrapped by `<sub>...</sub>`) and superscript (wrapped by `<sup>...</sup>`) are converted to word (#968)
   - equations wrapped by `$$...$$` and written in LaTeX format are converted to word
   - bookmarks are now created from markdown anchors (#923)
- Qualification time profiles use base unit if unit is not defined by user (#867, #991)

# ospsuite.reportingengine 2.1

## New features

- Qualification workflows are now available ! (#2)
  - Qualification Workflows have their own [vignette](../articles/qualification-workflow.html) to help you start (#566)
  - A template qualification workflow R script is available on GitHub at `system.file("extdata", "qualification-workflow-template.R", package = "ospsuite.reportingengine")` and can also be downloaded from [GitHub](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/blob/develop/inst/extdata/qualification-workflow-template.R) (#572)
  - Function `adjustTitlePage` can be used to personalize the report title page (#755)
  - Configuration plans can be re-loaded on `QualificationWorkflow` objects to update the report display (#567)
  - DDI subunits option is available (#642)
- With `ospsuite` version 10, simulations can be run in parallel (#526)
- Workflows account for time offset in simulation sets using option `timeOffset` for time profile plots (#313) or through user defined PK parameters for PK parameter plots (#578).
- User can define their own word report template to tune the styles of their report by providing a reference word document to the `wordConversionTemplate` of their Workflow object. A word reference template is available on [GitHub](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/blob/develop/inst/extdata/reference.docx) (#756)
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
* Possibility to read time and measurement units from nonmem columns (#414)

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
