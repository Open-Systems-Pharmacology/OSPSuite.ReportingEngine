#------------------------------------------------------------------------------#
# Advanced example 01: Plot time profile and residuals for mean model workflow #
#------------------------------------------------------------------------------#

library(ospsuite.reportingengine)

workflowFolder <- "myWorkflowResults"

# Since this example focuses only on the plot,
# The simulation of the results is called here
if (!dir.exists(file.path(workflowFolder, "SimulationResults"))) {
  source("test-example-01-simulations.R")
}

pkAUC <- PkParameterInfo$new("AUC_inf", displayName = "AUC infinity")
pkCmax <- PkParameterInfo$new("C_max", displayName = "Cmax")

output10mg <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Raltegravir simulated data",
  displayUnit = "mg/l",
  dataSelection = "Grouping %in% '10mg_'",
  dataDisplayName = "Raltegravir observed data",
  pkParameters = c(pkAUC, pkCmax)
)
output50mg <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Raltegravir simulated data",
  displayUnit = "mg/l",
  dataFilter = "Grouping %in% '50mg'",
  dataDisplayName = "Raltegravir observed data",
  pkParameters = c(pkAUC, pkCmax)
)

meanModelSets <- list(
  SimulationSet$new(
    simulationSetName = "Raltegravir 10 mg",
    simulationFile = "PKML/Raltegravir 10 mg   (lactose formulation).pkml",
    outputs = output10mg,
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
  ),
  SimulationSet$new(
    simulationSetName = "Raltegravir 50 mg",
    simulationFile = "PKML/Raltegravir 50 mg  (lactose formulation).pkml",
    outputs = output50mg,
    observedDataFile = "Raltegravir_PK.csv",
    observedMetaDataFile = "tpDictionary.csv"
  )
)

myMeanWorkflow <- MeanModelWorkflow$new(simulationSets = meanModelSets, workflowFolder = workflowFolder)
myMeanWorkflow$activateTasks(c("plotGoF"))
myMeanWorkflow$inactivateTasks(c("simulate"))

myMeanWorkflow$runWorkflow()
