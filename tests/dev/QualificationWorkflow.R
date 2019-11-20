# Test of Qualification Workflow from Reporting Engine
rm(list = ls())
setwd("C:/Design2Code/OSPSuite.ReportingEngine/tests/dev")

library(ggplot2)
# library(tlf)
library(ospsuite.reportingengine)

# Read configuration from .json
jsonFile <- "reporting engine input/report-configuration-plan.json"
reInput <- "reporting engine input"
reOutput <- "reporting engine output"

configurationPlan <- ConfigurationPlan$new(
  jsonFile,
  reInput,
  reOutput
)
