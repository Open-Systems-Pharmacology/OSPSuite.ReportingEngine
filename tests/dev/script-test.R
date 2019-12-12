# Test Script for workflow

rm(list=ls())

library(tlf)
library(ospsuite)
library(ospsuite.reportingengine)

#sim <- loadSimulation("../tests/data/S1.pkml")
#pop <- loadPopulation("../tests/data/pop_10.csv")

popWorkflow <- PopulationWorkflow$new(outputFolder = "../tests/WorkflowOutput",
                                      simulationPath = "../tests/data/S1.pkml",
                                      populationPath = "../tests/data/pop_10.csv"
                                      )

popWorkflow$runWorkflow()

# This line is for showing the result as a subplots
# It can be automatized later on when generating the report
demographyPlots <- gridExtra::grid.arrange(grobs = popWorkflow$demographyPlot$output, layout_matrix = rbind(c(1,1),c(2,3)))
plot(demographyPlots)

plot(popWorkflow$timeProfilePlot$output)