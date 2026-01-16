# Excel Template

``` r
require(ospsuite.reportingengine)
#> Loading required package: ospsuite.reportingengine
#> Loading required package: tlf
#> Loading required package: ospsuite
```

## Get started

### What for ?

Creating and customizing your workflow using R can be challenging.

Leveraging *Excel*, you can define your workflow, all its elements,
settings and options with a few clicks.

The Excel template was created so that users can design their own
workflows by directly tuning a working example.

### What is it ?

The Excel template is an *xlsx* document that defines all the relevant
information of a working workflow.

The function
[`createWorkflowFromExcelInput()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/createWorkflowFromExcelInput.md)
converts such Excel document into an R script that defines your workflow
in plain R code. Then, you can run your workflow by running the R script
(e.g. using the function
[`source()`](https://rdrr.io/r/base/source.html)).

### Where can I get the template ?

The Excel template is available through the following link
[WorkflowInput.xlsx](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/articles/templates/WorkflowInput.xlsx).

## Standard Excel Sheet Names

The Excel template includes a few standard Excel sheets that are
required for the conversion into R code.

- [Documentation](#documentation)
- [Workflow and Tasks](#workflow-and-tasks)
- [SimulationSets](#simulationsets)
- [Data Sources](#data-sources)
- [Outputs](#outputs)
- [Userdef PK Parameter](#userdef-pk-parameter)
- [PK Parameter](#pk-parameter)
- [SensitivityParameter](#sensitivityparameter)
- [tpDictionary](#tpdictionary)

### Documentation

The Excel sheet *Documentation* aims at documenting your R script. All
of its content will be added as commented text (preceded by `#`
character) in first lines of your R script.

Table : Documentation sheet

| Purpose:          | Please fill in the purpose of the script      |
|:------------------|:----------------------------------------------|
| M&S activity:     | Please fill in the corresponding M&S activity |
| Validation level: | Please fill in the Validation level           |

### Workflow and Tasks

The Excel sheet *Workflow and Tasks* defines the main attributes of your
workflow.

Table : Workflow and Tasks sheet

[TABLE]

### SimulationSets

The Excel sheet *SimulationSets* defines the simulation sets and their
properties of your workflow.

Table : SimulationSets sheet

[TABLE]

### Outputs

The Excel sheet *Outputs* defines the properties of each output paths.

Table : Outputs sheet

[TABLE]

Check section [How to define Output objects ?](#excel-outputs) for more
details on how to set up observed data sets.

### Data Sources

The Excel sheet *Data Sources* defines the observed data sets and their
properties.

Table : Data Sources sheet

[TABLE]

Check section [How to define observed data sets
?](#excel-observed-datasets) for more details on how to set up observed
data sets.

### Userdef PK Parameter

This standard sheet is required when user-defined PK Parameters are
calculated in your simulations.

Check section [How to define PK Parameters ?](#excel-pk) for more
details on how to set up user-defined PK Parameters.

### SensitivityParameter

This standard sheet is a template that can be tuned to inform which
specific input parameters you wish to vary in your sensitivity analysis.

Check section [How to set up sensitivity analyses ?](#excel-sensitivity)
for more details on how to set up the input parameters.

### tpDictionary

This standard sheet is a template that can be tuned to inform the meta
data of your observed datasets.

Check section [How to define observed datasets
?](#excel-observed-datasets) for more details on how to set up the
dictionary of your datasets.

## How to ?

### How to define Output objects ?

To define and include Output objects in your Simulation Sets, users only
need to

1- Define them in the standard Excel sheet *Outputs*

2- Declare the name of the created output(s) in the Excel sheet
*SimulationSets* in the cell corresponding to *outputs*. A drop-down
menu is available and allows you to select directly an output defined in
the sheet *Outputs*. Users can also provide multiple outputs within the
cell, however they need to be separated by a comma (character `,`).

### How to define PK Parameters ?

To define and include PK Parameters users need to

1- Define them in an Excel sheet using the Excel sheet *PK Parameter* as
reference (*PK Conc Single*, *PK Conc Multi* and *PK Fraction* can also
be used as reference)

Table : PK Parameter template sheet

| Name                           | Display name                 | Unit      |
|:-------------------------------|:-----------------------------|:----------|
| C_max                          | C max                        | µg/l      |
| C_max_norm                     | C max norm                   | kg/l      |
| t_max                          | T max                        | h         |
| C_tEnd                         | C End                        | µg/l      |
| AUC_tEnd                       | AUC                          | µg\*h/l   |
| AUC_tEnd_norm                  | AUC norm                     | kg\*h/l   |
| AUC_inf                        | AUC inf                      | µg\*h/l   |
| AUC_inf_norm                   | AUC inf norm                 | kg\*h/l   |
| MRT                            | MRT                          | h         |
| Thalf                          | Thalf                        | h         |
| CL                             | CL                           | ml/min/kg |
| Vss                            | Vss                          | ml/kg     |
| Vd                             | Vd                           | ml/kg     |
| C_max_tD1_tD2                  | C_max_t1_t2                  | µg/l      |
| C_max_tD1_tD2_norm             | C_max_t1_t2_norm             | kg/l      |
| C_max_tDLast_tEnd              | C_max_tLast_tEnd             | µg/l      |
| C_max_tDLast_tEnd_norm         | C_max_tLast_tEnd_norm        | kg/l      |
| t_max_tD1_tD2                  | t_max_t1_t2                  | h         |
| t_max_tDLast_tEnd              | t_max_tLast_tEnd             | h         |
| C_trough_tD2                   | C_trough_t2                  | µg/l      |
| C_trough_tDLast                | C_trough_tLast               | µg/l      |
| AUC_tD1_tD2                    | AUC_t1_t2                    | µg\*h/l   |
| AUC_tD1_tD2_norm               | AUC_t1_t2_norm               | kg\*h/l   |
| AUC_tDLast_minus_1_tDLast      | AUC_tLast_minus_1_tLast      | µg\*h/l   |
| AUC_tDLast_minus_1_tDLast_norm | AUC_tLast_minus_1_tLast_norm | kg\*h/l   |
| AUC_inf_tD1                    | AUC_inf_t1                   | µg\*h/l   |
| AUC_inf_tD1_norm               | AUC_inf_t1_norm              | kg\*h/l   |
| AUC_inf_tDLast                 | AUC_inf_tLast                | µg\*h/l   |
| AUC_inf_tDLast_norm            | AUC_inf_tLast_norm           | kg\*h/l   |
| Thalf_tDLast_tEnd              | Thalf_tLast_tEnd             | h         |
| F_tEnd                         | F_tEnd                       | %         |
| F_max                          | F_max                        | %         |

2- Declare the name of the created sheet in the Excel sheet of your
Output object in the cell corresponding to *pkParameters*

#### User-defined PK Parameters

One additional sheet is required for user-defined PK Parameters. Use the
standard Excel sheet *Userdef PK Parameter* to define your own
user-defined PK Parameter and include the names of the parameters in
your PK Parameters sheet.

### How to define observed data sets ?

Observed data sets are managed using `DataSource` objects.

To define and include observed data users need to

1- Define them in the standard Excel sheet *Data Sources*

2- Declare the name of the sources in the standard Excel sheet
*SimulationSet* within the cell corresponding to *dataSource*. A
drop-down menu is available and allows you to select directly a data
source defined in the sheet *Data Sources*. Users can only provide a
unique dataSource within the corresponding cell.

`DataSource` objects require a `metaDataFile` defining the content of
the observed dataset. This meta data can be provided either by using an
Excel sheet of your Excel document or by using a separate csv file.

- If dictionary is defined as an Excel sheet:
  - Go to the standard Excel sheet *Data Sources*
  - Select the option **SHEET** in the cell corresponding to your
    **DictionaryType**
  - Include the name of the sheet in the cell corresponding to
    **DictionaryLocation**  
    You can use the Excel sheet *tpDictionary* as reference
- If dictionary is defined as a separate csv file:
  - Go to the standard Excel sheet *Data Sources*
  - Select the option **FILE** in the cell corresponding to your
    **DictionaryType**
  - Include the path of the file in the cell corresponding to
    **DictionaryLocation**  
    You can download and use the template
    [tpDictionary.csv](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/articles/templates/tpDictionary.csv)
    as reference

### How to set up sensitivity analyses ?

Because sensitivity analyses calculate and display the relative impact
of selected - or all - input parameters on the PK parameters of those
selected output curves, they can be performed and displayed in many ways

For such reason, workflows provide a lot of options and settings that
will help you design the specific analysis you wish to perform.

To include a sensitivity analysis in your workflow report, you need to
activate the following tasks: **simulate**, **calculatePKParameters**,
**calculateSensitivity**, and **plotSensitivity**.

To activate these tasks, go to the standard Excel sheet *Workflow and
Tasks* and set the Values of the corresponding cells to **Yes**.

In the same Excel sheet, you will find many options for your sensitivity
analysis under the section *Task-specific options*.

The first set of options are related to the calculation of the
sensitivity.

In these options, you can set up the variation range of all or selected
input parameters. This range defines the amplitude of the
variations/perturbations applied around the value in the simulation. For
more details about the variation range, you can check the [OSP Suite
documentation](https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis)
on sensitivity analysis.

Another option is the selection of specific input parameters on which
applying the variations. To include only selected input parameters users
need to

1- Define them in an Excel sheet using the Excel sheet
*SensitivityParameter* as reference

Table : SensitivityParameter template sheet

| Path                                                 |
|:-----------------------------------------------------|
| C1\|Lipophilicity                                    |
| C1\|Specific intestinal permeability (transcellular) |

2- Declare the name of the created sheet in the standard Excel sheet
*Workflow and Tasks* in the cell corresponding to *calculateSensitivity:
variableParameterPaths*

The second set of options are related to the sensitivity plots and helps
you defining how many input parameters are displayed and how they are
displayed.

### How add a Study Design table ?

It is possible to add a special parameters variation which is not
exported in your initial population by including a StudyDesign table to
your workflow.

To define Study Design tables users can either leverage an Excel sheet
of the Excel document or they can use a separate csv file.

- If the Study Design is defined as an Excel sheet:
  - Go to the standard Excel sheet *SimulationSets*
  - Select the option **SHEET** in the cell corresponding to your
    **StudyDesignType**
  - Include the name of the sheet in the cell corresponding to
    **StudyDesignLocation**  
    You can use the Excel sheet *StudyDesign* as reference
- If Study Design is defined as a separate csv file:
  - Go to the standard Excel sheet *SimulationSets*
  - Select the option **FILE** in the cell corresponding to your
    **StudyDesignType**
  - Include the path of the file in the cell corresponding to
    **StudyDesignLocation**  
    You can download and use the template
    [StudyDesign.csv](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/articles/templates/StudyDesign.csv)
    as reference

Table : StudyDesign sheet

    #> New names:
    #> • `Organism|Weight` -> `Organism|Weight...1`
    #> • `Organism|Weight` -> `Organism|Weight...2`

| Organism\|Weight…1 | Organism\|Weight…2 | Gender        | Applications\|IV Bolus\|DrugMass |
|:-------------------|:-------------------|:--------------|:---------------------------------|
| kg                 | kg                 |               | nmol                             |
| SOURCE_MIN         | SOURCE_MAX         | SOURCE_EQUALS | TARGET                           |
| 20                 | 40                 | MALE          | 2                                |
| 20                 | 40                 | FEMALE        | 2.5                              |
| 40                 | 60                 | MALE          | 10                               |
| 40                 | 60                 | FEMALE        | 14                               |
| 60                 |                    |               | 20                               |

### How to include your own code ?

Workflows can be updated by a user-defined function before execution
(e.g. change options, add user-defined tasks, …) by including the
function call before the line `workflow$runWorkflow()` in your R script.

To do so, in the standard Excel sheet *Workflow and Tasks*, you can
include in the cell corresponding to **activitySpecificCode** the name
of the R function.

Note that this function must be placed in the same directory as the
generated R code and **must have 1 argument of the type `"workflow"`**.
