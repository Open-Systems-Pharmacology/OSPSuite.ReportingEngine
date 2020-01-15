# Test Script for workflow

# Clear and load libraries
rm(list = ls())

library(tlf)
library(ospsuite)
library(ospsuite.reportingengine)

# Initializing the workflow creates a folder structure
# (if it does not already exist) automatThis define the workflow structure
popWorkflow <- PopulationWorkflow$new(
  simulationFile = "../tests/data/S1.pkml",
  populationFile = "../tests/data/pop_10.csv"
)

# The print method gives an overview of the task list and if they are active
popWorkflow

# You can go into more detail for each tasks:
popWorkflow$demographyPlot

# You can also turn off/on some tasks
popWorkflow$sensitivityPlot$inactivate()
popWorkflow

# To run the whole workflow
popWorkflow$runWorkflow()

# This line is for showing the result as a subplots
# It can be automatized later on when generating the report
# demographyPlots <- gridExtra::grid.arrange(grobs = popWorkflow$demographyPlot$output, layout_matrix = rbind(c(1,1),c(2,3)))
# plot(demographyPlots)

# plot(popWorkflow$gofPlot$output[["timeProfile"]])
