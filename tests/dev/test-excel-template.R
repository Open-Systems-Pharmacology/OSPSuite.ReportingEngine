# Test script to create workflow.R from an Excel template

# An Excel template is available in the extdata folder
# The best practice is to copy and update them, file.copy() can be used for that purpose

# Template is available at the following path
excelFile <- system.file("extdata", "WorkflowInput.xlsx", package = "ospsuite.reportingengine")

workflowFile <- createWorkflowFromExcelInput(excelFile)

# An optional parameter can be added:
# workflowFile: name of the R script file created from the Excel file
# createWorkflowFromExcelInput(excelFile, workflowFile = "workflow.R")

# Run the resulting file:
source(workflowFile, encoding = 'UTF-8')
