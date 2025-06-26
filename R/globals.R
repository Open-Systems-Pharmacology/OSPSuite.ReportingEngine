# Remove check warning for the variables listed below
# Variables called withing dplyr and tidyr functions
dplyrVariables <- c(
  "Amount",
  "drugMass",
  "DrugMass",
  "GeoMean",
  "GeoSD",
  "IndividualId",
  "Legend",
  "Mean",
  "MeanLogX",
  "MeanLogY",
  "MeanX",
  "MeanY",
  "newEnv",
  "NormalizedAmount",
  "Parameter",
  "QuantityPath",
  "SD",
  "SDLogX",
  "SDLogY",
  "SDX",
  "SDY",
  "SimulationSetName",
  "Time",
  "Value"
)
rmpiVariables <- c(
  "mpi.comm.rank",
  "population",
  "sim"
)
lintVariables <- c(".Random.seed")

utils::globalVariables(c(dplyrVariables, rmpiVariables, lintVariables))
