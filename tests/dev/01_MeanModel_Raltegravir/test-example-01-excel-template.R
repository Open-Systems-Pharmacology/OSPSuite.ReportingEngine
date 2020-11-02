# Test script to create workflow.R from an Excel template

setwd("C:/Design2Code/OSPSuite.ReportingEngine/tests/dev/01_MeanModel_Raltegravir")
excelFile <- "workflow-input-01.xlsx"

workflowFile <- createWorkflowFromExcelInput(excelFile)

# An optional parameter can be added:
# workflowFile: name of the R script file created from the Excel file
# createWorkflowFromExcelInput(excelFile, workflowFile = "workflow.R")

# Run the resulting file:
source(workflowFile, encoding = 'UTF-8')
