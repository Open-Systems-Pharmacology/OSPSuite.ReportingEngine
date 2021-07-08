# Example 03 test script to upload a user-defined plot task
# Template function is available in "../script-template-user-defined-function.R"
require(ospsuite.reportingengine)
source("../script-template-user-defined-function.R")

# Output will create `PkParameterInfo` objects if the names of the PK parameters are directly input
output <- Output$new(
  path = "Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)",
  displayName = "Venous Blood",
  pkParameters = c("AUC_inf", "C_max")
)

popModelSets <- list(
  Larson = PopulationSimulationSet$new(
    referencePopulation = FALSE,
    simulationSetName = "Larson",
    simulationFile = "PKML/Larson 2013 8-18y meal.pkml",
    populationFile = "Larson 2013 8-18y meal-Population.csv",
    outputs = output
  ),
  Adult = PopulationSimulationSet$new(
    referencePopulation = TRUE,
    simulationSetName = "Adults",
    simulationFile = "PKML/Raltegravir 400mg filmcoated tablet.pkml",
    populationFile = "Raltegravir Adult Population.csv",
    outputs = output
  )
)

# Create workflow on which user defined task needs to be added
userTestWorkflow <- PopulationWorkflow$new(
  workflowType = PopulationWorkflowTypes$parallelComparison,
  simulationSets = popModelSets,
  workflowFolder = "myTestFolder"
)

# load user defined task
addUserDefinedTask(userTestWorkflow,
  plotAUCRatios,
  taskName = "AUCRatios",
  active = TRUE,
  settings = NULL
)

# Title within the report of user defined task #1
userTestWorkflow$userDefinedTasks[[1]]$title <- "AUC Ratios across populations"

# Since user defined task will require simulated PK Parameters from task calculatePKParameters
# simulate and calculatePKParameters task need to be run
userTestWorkflow$inactivateTasks()
userTestWorkflow$activateTasks(c("simulate", "calculatePKParameters"))

userTestWorkflow$runWorkflow()
