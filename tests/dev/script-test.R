# Test Script for workflow

rm(list = ls())

library(tlf)
library(ospsuite)
library(ospsuite.reportingengine)

# sim <- loadSimulation("../tests/data/S1.pkml")
# pop <- loadPopulation("../tests/data/pop_10.csv")

# This define the workflow structure
popWorkflow <- PopulationWorkflow$new(
  simulationFile = "../tests/data/S1.pkml",
  populationFile = "../tests/data/pop_10.csv"
)
output1 <- "../S1_pop_10/"
output2 <- "../S1_pop_20/"

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
