# Test script to create workflow.R from an Excel template

# Two Excel templates are available in the extdata folder
# The best practice is to copy and update them, file.copy() can be used for that purpose

# Template 1: Mean Model workflow
excelFile <- system.file("extdata", "mean-model-workflow-input.xlsx", package = "ospsuite.reportingengine")

# Template 2: Population model workflow
# excelFile <- system.file("extdata", "population-workflow-input.xlsx", package = "ospsuite.reportingengine")

workflowFile <- createWorkflowFromExcelInput(excelFile)

# Two optional parameters can be added:
# workflowFile: name of the R script file created from the Excel file
# workflowFolder: name of the folder in which the wrokflow results will be stored
# createWorkflowFromExcelInput(excelFile, workflowFile = "workflow.R", workflowFolder = "Output")

# Run the resulting file:
source(workflowFile, encoding = 'UTF-8')
