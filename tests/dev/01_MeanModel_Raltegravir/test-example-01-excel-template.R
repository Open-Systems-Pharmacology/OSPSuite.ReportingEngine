# Test script to create workflow.R from an Excel template

excelFile <- "workflow-input-01.xlsx"

workflowFile <- createWorkflowFromExcelInput(excelFile)

# Optional parameters can be added:
# workflowFile: name of the R script file created from the Excel file
# removeComments: logical to remove all comments provided as guidance to review the script
# createWorkflowFromExcelInput(excelFile, workflowFile = "workflow.R")

# Run the resulting file:
source(workflowFile, encoding = 'UTF-8')
