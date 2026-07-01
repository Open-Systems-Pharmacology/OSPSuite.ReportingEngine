# QualificationVersionInfo

Object defining Qualification Version Information to be displayed on
title page

## See also

Other qualification workflow:
[`ConfigurationPlan`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/ConfigurationPlan.md),
[`adjustTitlePage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/adjustTitlePage.md),
[`loadConfigurationPlan()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/loadConfigurationPlan.md),
[`loadQualificationWorkflow()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/loadQualificationWorkflow.md),
[`startQualificationRunner()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/reference/startQualificationRunner.md)

## Public fields

- `qualificationPlanRelease`:

  Qualification Plan Release Version (string, e.g. `"1.1"`)

- `osp`:

  OSP version (string, e.g. `"10.0"`)

- `qualificationFramework`:

  Qualification Framework version (string, e.g. `"3.0"`)

## Methods

### Public methods

- [`QualificationVersionInfo$new()`](#method-QualificationVersionInfo-new)

- [`QualificationVersionInfo$print()`](#method-QualificationVersionInfo-print)

- [`QualificationVersionInfo$updateText()`](#method-QualificationVersionInfo-updateText)

- [`QualificationVersionInfo$getQualificationPlanReleasePattern()`](#method-QualificationVersionInfo-getQualificationPlanReleasePattern)

- [`QualificationVersionInfo$getOSPPattern()`](#method-QualificationVersionInfo-getOSPPattern)

- [`QualificationVersionInfo$getQualificationFrameworkPattern()`](#method-QualificationVersionInfo-getQualificationFrameworkPattern)

- [`QualificationVersionInfo$setQualificationPlanReleasePattern()`](#method-QualificationVersionInfo-setQualificationPlanReleasePattern)

- [`QualificationVersionInfo$setOSPPattern()`](#method-QualificationVersionInfo-setOSPPattern)

- [`QualificationVersionInfo$setQualificationFrameworkPattern()`](#method-QualificationVersionInfo-setQualificationFrameworkPattern)

- [`QualificationVersionInfo$clone()`](#method-QualificationVersionInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `QualificationVersionInfo` object.

#### Usage

    QualificationVersionInfo$new(
      qualificationPlanRelease = NULL,
      osp = NULL,
      qualificationFramework = NULL
    )

#### Arguments

- `qualificationPlanRelease`:

  Qualification Plan Release Version (string, e.g. `"1.1"`)

- `osp`:

  OSP version (string, e.g. `"10.0"`)

- `qualificationFramework`:

  Qualification Framework version (string, e.g. `"3.0"`)

#### Returns

A new `QualificationVersionInfo` object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print version information

#### Usage

    QualificationVersionInfo$print()

#### Returns

A text with system information

------------------------------------------------------------------------

### Method `updateText()`

Update `text` if patterns of version info are found

#### Usage

    QualificationVersionInfo$updateText(text)

#### Arguments

- `text`:

  character string

#### Returns

Updated character string

------------------------------------------------------------------------

### Method `getQualificationPlanReleasePattern()`

Get the Qualification Plan Release Version pattern to be replaced in
title page **Caution, the function `gsub` is used to replace the
pattern** **Escape characters may be included in the pattern**

#### Usage

    QualificationVersionInfo$getQualificationPlanReleasePattern()

#### Returns

A character

------------------------------------------------------------------------

### Method `getOSPPattern()`

Get the OSP version pattern to be replaced in title page **Caution, the
function `gsub` is used to replace the pattern** **Escape characters may
be included in the pattern**

#### Usage

    QualificationVersionInfo$getOSPPattern()

#### Returns

A character

------------------------------------------------------------------------

### Method `getQualificationFrameworkPattern()`

Get the Qualification Framework version pattern to be replaced in title
page **Caution, the function `gsub` is used to replace the pattern**
**Escape characters may be included in the pattern**

#### Usage

    QualificationVersionInfo$getQualificationFrameworkPattern()

#### Returns

A character

------------------------------------------------------------------------

### Method `setQualificationPlanReleasePattern()`

Set the Qualification Plan Release Version pattern to be replaced in
title page **Caution, the function `gsub` is used to replace the
pattern.** **You may need to include escape characters in `pattern`**

#### Usage

    QualificationVersionInfo$setQualificationPlanReleasePattern(pattern)

#### Arguments

- `pattern`:

  characters to be replaced in title page

------------------------------------------------------------------------

### Method `setOSPPattern()`

Set the OSP version pattern to be replaced in title page **Caution, the
function `gsub` is used to replace the pattern.** **You may need to
include escape characters in `pattern`**

#### Usage

    QualificationVersionInfo$setOSPPattern(pattern)

#### Arguments

- `pattern`:

  characters to be replaced in title page

------------------------------------------------------------------------

### Method `setQualificationFrameworkPattern()`

Set the Qualification Framework version pattern to be replaced in title
page **Caution, the function `gsub` is used to replace the pattern.**
**You may need to include escape characters in `pattern`**

#### Usage

    QualificationVersionInfo$setQualificationFrameworkPattern(pattern)

#### Arguments

- `pattern`:

  characters to be replaced in title page

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    QualificationVersionInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
