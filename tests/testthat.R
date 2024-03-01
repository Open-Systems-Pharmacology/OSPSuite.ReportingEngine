library(testthat)
library(ospsuite.reportingengine)

# Ensure default are set before tests
resetRESettingsToDefault()

test_check("ospsuite.reportingengine", encoding = "utf-8")

# Ensure default are reset after tests
resetRESettingsToDefault()