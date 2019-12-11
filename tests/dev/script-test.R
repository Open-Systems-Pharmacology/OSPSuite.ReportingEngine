# Test Script for workflow

rm(list=ls())

library(tlf)
library(ospsuite)
library(ospsuite.reportingengine)

sim <- loadSimulation("../tests/data/S1.pkml")
pop <- loadPopulation("../tests/data/pop_10.csv")

popWorkflow <- PopulationWorkflow$new()

plots <- popWorkflow$demographyPlot(sim)
