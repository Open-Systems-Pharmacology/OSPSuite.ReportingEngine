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

utils::globalVariables(c(dplyrVariables, rmpiVariables))
