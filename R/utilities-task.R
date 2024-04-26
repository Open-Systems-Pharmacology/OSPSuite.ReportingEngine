#' @title StandardSimulationTasks
#' @description Names of simulation tasks performed by both `MeanModelWorkflow` and `PopulationWorkflow` objects
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists all available standard simulation task names available in both mean and population workflows
#' StandardSimulationTasks
#'
StandardSimulationTasks <- enum(c("simulate", "calculatePKParameters", "calculateSensitivity"))

#' @title StandardPlotTasks
#' @description Names of plot tasks performed by both `MeanModelWorkflow` and `PopulationWorkflow` objects
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists all available standard plot task names available in both mean and population workflows
#' StandardPlotTasks
#'
StandardPlotTasks <- enum(c("plotTimeProfilesAndResiduals", "plotPKParameters", "plotSensitivity"))

#' @title AllAvailableTasks
#' @description Names of all existing tasks that can be performed by `MeanModelWorkflow` or `PopulationWorkflow` objects
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists all available task names available in mean or population workflows
#' AllAvailableTasks
#'
AllAvailableTasks <- c(
  StandardSimulationTasks,
  StandardPlotTasks,
  enum(c("plotDemography", "plotAbsorption", "plotMassBalance"))
)

#' @title activateWorkflowTasks
#' @description activates a series of `Tasks` from a `Workflow`
#' @param workflow A `Workflow` object
#' @param tasks names of the tasks to activate
#' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Use enum helper to get task names
#' activateWorkflowTasks(
#'   workflow = myWorkflow,
#'   tasks = StandardSimulationTasks$simulate
#' )
#'
#' # Default will activate every task of workflow
#' activateWorkflowTasks(workflow = myWorkflow)
#' }
#'
activateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")
  validateIsIncludedAndLog(tasks, workflow$getAllTasks(), groupName = "names of available workflow tasks")

  for (task in tasks) {
    workflow[[task]]$activate()
  }
}

#' @title inactivateWorkflowTasks
#' @description inactivates a series of `Tasks` from a `Workflow`
#' @param workflow `MeanModelWorkflow` or `PopulationWorkflow` object
#' @param tasks names of the tasks to activate
#' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Use enum helper to get task names
#' inactivateWorkflowTasks(
#'   workflow = myWorkflow,
#'   tasks = StandardSimulationTasks$simulate
#' )
#'
#' # Default will inactivate every task of workflow
#' inactivateWorkflowTasks(workflow = myWorkflow)
#' }
#'
inactivateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")
  validateIsIncludedAndLog(tasks, workflow$getAllTasks(), groupName = "names of available workflow tasks")

  for (task in tasks) {
    workflow[[task]]$inactivate()
  }
}

#' @title loadSimulateTask
#' @description
#' Define `simulate` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `TRUE`
#' @param settings specific settings for `simulate` task
#' @return A `SimulationTask` object
#' @export
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default simulation task for workflow
#' myWorkflow$simulate <- loadSimulateTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["otherSimulations"]] <- loadSimulateTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadSimulateTask <- function(workflow, active = TRUE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  return(SimulationTask$new(
    getTaskResults = simulateWorkflowModels,
    nameTaskResults = getObjectNameAsString(simulateWorkflowModels),
    outputFolder = defaultTaskOutputFolders$simulate,
    outputs = getSimulationResultFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    settings = settings,
    message = defaultWorkflowMessages$simulate
  ))
}

#' @title loadCalculatePKParametersTask
#' @description
#' Define `calculatePKParameters` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `calculatePKParameters` task
#' @return A `CalculatePKParametersTask` object
#' @export
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default calculatePKParameters task for workflow
#' myWorkflow$calculatePKParameters <- loadCalculatePKParametersTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["pkParameters"]] <- loadCalculatePKParametersTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadCalculatePKParametersTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  calculatePKParametersTask <- CalculatePKParametersTask$new(
    getTaskResults = calculatePKParameters,
    nameTaskResults = getObjectNameAsString(calculatePKParameters),
    outputFolder = defaultTaskOutputFolders$calculatePKParameters,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = getSimulationResultFileNames(workflow),
    outputs = getPkAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    settings = settings,
    message = defaultWorkflowMessages$calculatePKParameters
  )
  if (!isOfType(workflow, "PopulationWorkflow")) {
    return(calculatePKParametersTask)
  }
  if (!isIncluded(workflow$workflowType, PopulationWorkflowTypes$ratioComparison)) {
    return(calculatePKParametersTask)
  }
  calculatePKParametersTask$ratioComparison <- TRUE
  return(calculatePKParametersTask)
}

#' @title loadCalculateSensitivityTask
#' @description
#' Define `calculateSensitivity` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `calculateSensitivity` task
#' @return A `PopulationSensitivityAnalysisTask` for `PopulationWorkflow` objects.
#' A `SensitivityAnalysisTask` object otherwise
#' @export
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default calculateSensitivity task for workflow
#' myWorkflow$calculateSensitivity <- loadCalculateSensitivityTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["sensitivity"]] <- loadCalculateSensitivityTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadCalculateSensitivityTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationSensitivityAnalysisTask$new(
      getTaskResults = runPopulationSensitivityAnalysis,
      nameTaskResults = getObjectNameAsString(runPopulationSensitivityAnalysis),
      outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
      outputs = getPopulationSensitivityAnalysisResultsFileNames(workflow),
      inputFolder = defaultTaskOutputFolders$calculatePKParameters,
      inputs = getPkAnalysisResultsFileNames(workflow),
      workflowFolder = workflow$workflowFolder,
      settings = settings,
      active = active,
      message = defaultWorkflowMessages$sensitivityAnalysis
    ))
  }

  return(SensitivityAnalysisTask$new(
    getTaskResults = runSensitivity,
    nameTaskResults = getObjectNameAsString(runSensitivity),
    outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
    outputs = getMeanSensitivityAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    settings = settings,
    message = defaultWorkflowMessages$sensitivityAnalysis
  ))
}

#' @title loadPlotTimeProfilesAndResidualsTask
#' @description
#' Define `plotTimeProfilesAndResiduals` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `TRUE`
#' @param settings specific settings for `simulate` task
#' @return A `GofPlotTask` object
#' @import ospsuite.utils
#' @export
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default plotTimeProfilesAndResiduals task for workflow
#' myWorkflow$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotGoodnessOfFit"]] <- loadPlotTimeProfilesAndResidualsTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotTimeProfilesAndResidualsTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)
  validateIsOfType(settings, "TaskSettings", nullAllowed = TRUE)
  settings <- settings %||% GofTaskSettings$new(AllAvailableTasks$plotTimeProfilesAndResiduals)

  taskFunction <- plotMeanGoodnessOfFit
  nameFunction <- getObjectNameAsString(plotMeanGoodnessOfFit)
  if (isOfType(workflow, "PopulationWorkflow")) {
    taskFunction <- plotPopulationGoodnessOfFit
    nameFunction <- getObjectNameAsString(plotPopulationGoodnessOfFit)
  }

  return(GofPlotTask$new(
    reportTitle = defaultWorkflowTitles$plotGoF,
    reportReference = defaultWorkflowReferences$plotGoF,
    fileName = defaultWorkflowAppendices$plotGoF,
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    outputFolder = defaultTaskOutputFolders$plotGoF,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = getSimulationResultFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotGoF,
    settings = settings
  ))
}


#' @title loadPlotPKParametersTask
#' @description
#' Define `plotPKParameters` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `plotPKParameters` task
#' @return A `PopulationSensitivityAnalysisTask` for `PopulationWorkflow` objects.
#' A `SensitivityAnalysisTask` object otherwise
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default plotPKParameters task for workflow
#' myWorkflow$plotPKParameters <- loadPlotPKParametersTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotPK"]] <- loadPlotPKParametersTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotPKParametersTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)
  validateIsOfType(settings, "TaskSettings", nullAllowed = TRUE)
  settings <- settings %||% TaskSettings$new(AllAvailableTasks$plotPKParameters)

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationPlotTask$new(
      workflowType = workflow$workflowType,
      xParameters = getDefaultPkParametersXParameters(workflow$workflowType),
      yParameters = NULL,
      reportTitle = defaultWorkflowTitles$plotPKParameters,
      reportReference = defaultWorkflowReferences$plotPKParameters,
      fileName = defaultWorkflowAppendices$plotPKParameters,
      getTaskResults = plotPopulationPKParameters,
      nameTaskResults = getObjectNameAsString(plotPopulationPKParameters),
      outputFolder = defaultTaskOutputFolders$plotPKParameters,
      inputFolder = defaultTaskOutputFolders$calculatePKParameters,
      inputs = getPkAnalysisResultsFileNames(workflow),
      workflowFolder = workflow$workflowFolder,
      active = active,
      message = defaultWorkflowMessages$plotPKParameters,
      settings = settings
    ))
  }
  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotPKParameters,
    reportReference = defaultWorkflowReferences$plotPKParameters,
    fileName = defaultWorkflowAppendices$plotPKParameters,
    getTaskResults = plotMeanPKParameters,
    nameTaskResults = getObjectNameAsString(plotMeanPKParameters),
    outputFolder = defaultTaskOutputFolders$plotPKParameters,
    inputFolder = defaultTaskOutputFolders$calculatePKParameters,
    inputs = getPkAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotPKParameters,
    settings = settings
  ))
}

#' @title loadPlotSensitivityTask
#' @description
#' Define `plotSensitivity` task and its settings
#' @param workflow `Workflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `plotSensitivity` task
#' @return A `PopulationSensitivityAnalysisTask` for `PopulationWorkflow` objects.
#' A `SensitivityAnalysisTask` object otherwise
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default plotSensitivity task for workflow
#' myWorkflow$plotSensitivity <- loadPlotSensitivityTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotSensitivity"]] <- loadPlotSensitivityTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotSensitivityTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)
  validateIsOfType(settings, "SensitivityPlotSettings", nullAllowed = TRUE)
  settings <- settings %||% SensitivityPlotSettings$new()

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationPlotTask$new(
      workflowType = workflow$workflowType,
      xParameters = NULL,
      yParameters = NULL,
      reportTitle = defaultWorkflowTitles$plotSensitivity,
      reportReference = defaultWorkflowReferences$plotSensitivity,
      fileName = defaultWorkflowAppendices$plotSensitivity,
      getTaskResults = plotPopulationSensitivity,
      nameTaskResults = getObjectNameAsString(plotPopulationSensitivity),
      outputFolder = defaultTaskOutputFolders$plotSensitivity,
      inputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
      inputs = getPopulationSensitivityAnalysisResultsFileNames(workflow),
      workflowFolder = workflow$workflowFolder,
      active = active,
      message = defaultWorkflowMessages$plotSensitivity,
      settings = settings
    ))
  }
  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotSensitivity,
    reportReference = defaultWorkflowReferences$plotSensitivity,
    fileName = defaultWorkflowAppendices$plotSensitivity,
    getTaskResults = plotMeanSensitivity,
    nameTaskResults = getObjectNameAsString(plotMeanSensitivity),
    outputFolder = defaultTaskOutputFolders$plotSensitivity,
    inputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
    inputs = getMeanSensitivityAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotSensitivity,
    settings = settings
  ))
}


#' @title loadPlotMassBalanceTask
#' @description
#' Define `plotMassBalance` task and its settings
#' @param workflow `MeanModelWorkflow` object
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `plotMassBalance` task
#' @return A `PlotTask` object
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default plotMassBalance task for workflow
#' myWorkflow$plotMassBalance <- loadPlotMassBalanceTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotMassBalance"]] <- loadPlotMassBalanceTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotMassBalanceTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "MeanModelWorkflow")
  validateIsLogical(active)
  validateIsOfType(settings, "TaskSettings", nullAllowed = TRUE)

  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotMassBalance,
    reportReference = defaultWorkflowReferences$plotMassBalance,
    fileName = defaultWorkflowAppendices$plotMassBalance,
    getTaskResults = plotMeanMassBalance,
    nameTaskResults = getObjectNameAsString(plotMeanMassBalance),
    outputFolder = defaultTaskOutputFolders$plotMassBalance,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotMassBalance,
    settings = settings %||% TaskSettings$new(AllAvailableTasks$plotMassBalance)
  ))
}

#' @title loadPlotAbsorptionTask
#' @description
#' Define `plotAbsorption` task and its settings
#' @param workflow `MeanModelWorkflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `plotAbsorption` task
#' @return A `PlotTask` object
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # (Re)load a default plotAbsorption task for workflow
#' myWorkflow$plotAbsorption <- loadPlotAbsorptionTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotAbsorption"]] <- loadPlotAbsorptionTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotAbsorptionTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "MeanModelWorkflow")
  validateIsLogical(active)
  validateIsOfType(settings, "TaskSettings", nullAllowed = TRUE)

  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotAbsorption,
    reportReference = defaultWorkflowReferences$plotAbsorption,
    fileName = defaultWorkflowAppendices$plotAbsorption,
    getTaskResults = plotMeanAbsorption,
    nameTaskResults = getObjectNameAsString(plotMeanAbsorption),
    outputFolder = defaultTaskOutputFolders$plotAbsorption,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotAbsorption,
    settings = settings %||% TaskSettings$new(AllAvailableTasks$plotAbsorption)
  ))
}

#' @title loadPlotDemographyTask
#' @description
#' Define `plotDemography` task and its settings
#' @param workflow `PopulationWorkflow` object or derived class
#' @param active logical defining if `Task` will be run by workflow.
#' Default value is `FALSE`
#' @param settings specific settings for `plotDemography` task
#' @return A `PlotTask` object
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(
#'   workflowType,
#'   workflowFolder,
#'   simulationSets
#' )
#'
#' # (Re)load a default plotDemography task for workflow
#' myWorkflow$plotDemography <- loadPlotDemographyTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#'
#' # Load a user-defined simulation task for workflow
#' myWorkflow$userDefinedTasks[["plotDemography"]] <- loadPlotDemographyTask(
#'   workflow = myWorkflow,
#'   active = TRUE
#' )
#' }
#'
loadPlotDemographyTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsLogical(active)
  validateIsOfType(settings, "TaskSettings", nullAllowed = TRUE)

  return(PopulationPlotTask$new(
    workflowType = workflow$workflowType,
    xParameters = getDefaultDemographyXParameters(workflow$workflowType),
    yParameters = NULL,
    reportTitle = defaultWorkflowTitles$plotDemography,
    reportReference = defaultWorkflowReferences$plotDemography,
    fileName = defaultWorkflowAppendices$plotDemography,
    getTaskResults = plotDemographyParameters,
    nameTaskResults = getObjectNameAsString(plotDemographyParameters),
    outputFolder = defaultTaskOutputFolders$plotDemography,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotDemography,
    settings = settings %||% TaskSettings$new(AllAvailableTasks$plotDemography)
  ))
}

#' @title getSimulationResultFileNames
#' @description
#' Get the expected simulation result files obtained from a workflow
#' @param workflow `Workflow` object or derived class
#' @return Names of the the expected simulation result files
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Get expected paths and file names for workflow
#' getSimulationResultFileNames(myWorkflow)
#' }
#'
getSimulationResultFileNames <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  simulationResultFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    simulationResultFileNames <- c(simulationResultFileNames, structureSet$simulationResultFileNames)
  }
  return(simulationResultFileNames)
}

#' @title getPKAnalysisResultsFileNames
#' @description
#' Get the expected PK analysis result files obtained from a workflow
#' @param workflow `Workflow` object or derived class
#' @return Names of the the expected PK analysis result files
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Get expected paths and file names for workflow
#' getPKAnalysisResultsFileNames(myWorkflow)
#' }
#'
getPKAnalysisResultsFileNames <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  pkAnalysisResultsFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    pkAnalysisResultsFileNames <- c(pkAnalysisResultsFileNames, structureSet$pkAnalysisResultsFileNames)
  }
  return(pkAnalysisResultsFileNames)
}

#' @rdname getPKAnalysisResultsFileNames
#' @export
getPkAnalysisResultsFileNames <- getPKAnalysisResultsFileNames

#' @title getMeanSensitivityAnalysisResultsFileNames
#' @description
#' Get the expected sensitivity analysis result files obtained from a mean model workflow
#' @param workflow `MeanModelWorkflow` object or derived class
#' @return Names of the the expected sensitivity analysis result files
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Get expected paths and file names for workflow
#' getMeanSensitivityAnalysisResultsFileNames(myWorkflow)
#' }
#'
getMeanSensitivityAnalysisResultsFileNames <- function(workflow) {
  validateIsOfType(workflow, "MeanModelWorkflow")
  sensitivityAnalysisResultsFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    sensitivityAnalysisResultsFileNames <- c(sensitivityAnalysisResultsFileNames, structureSet$sensitivityAnalysisResultsFileNames)
  }
  return(sensitivityAnalysisResultsFileNames)
}

#' @title getPopulationSensitivityAnalysisResultsFileNames
#' @description
#' Get the expected sensitivity analysis result files obtained from a population workflow
#' @param workflow `PopulationWorkflow` object or derived class
#' @return Names of the the expected sensitivity analysis result files
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(
#'   workflowType,
#'   workflowFolder,
#'   simulationSets
#' )
#'
#' # Get expected paths and file names for workflow
#' getPopulationSensitivityAnalysisResultsFileNames(myWorkflow)
#' }
#'
getPopulationSensitivityAnalysisResultsFileNames <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  sensitivityAnalysisResultsFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    sensitivityAnalysisResultsFileNames <- c(sensitivityAnalysisResultsFileNames, structureSet$popSensitivityAnalysisResultsIndexFile)
  }
  return(sensitivityAnalysisResultsFileNames)
}

#' @title getTaskInputs
#' @description
#' Get the names of the files required to perform the task
#' @param task `Task` object or derived class
#' @return Names of the files required to perform the task
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Get paths of required files for running a workflow task
#' getTaskInputs(myWorkflow$plotSensitivity)
#' }
#'
getTaskInputs <- function(task) {
  validateIsOfType(task, "Task")
  return(task$getInputs())
}

#' @title checkTaskInputsExist
#' @description
#' Check if the files required by the task exist
#' @param task `Task` object or derived class
#' @return Named list of logical values assessing if the files exist
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Checks if paths of required files for running a workflow task exist
#' checkTaskInputsExist(myWorkflow$plotSensitivity)
#' }
#'
checkTaskInputsExist <- function(task) {
  validateIsOfType(task, "Task")
  return(sapply(task$getInputs(), file.exists))
}


#' @title addUserDefinedTask
#' @description
#' Add a user-defined function as a task accessible in the `Workflow` field `$userDefinedTasks`
#' @param workflow `Workflow` object or derived class
#' @param taskFunction Function to be called by `Task` object.
#' For `MeanModelWorkflow`, input arguments of `taskFunction` should include: `simulationSet`, and `settings`.
#' For `PopulationWorkflow`, input arguments of `taskFunction` should include: `simulationSets`, `xParameters`, `yParameters` and `settings`
#' @param taskName character name of task to initialize reporting of task
#' @param active logical to set if task is run by workflow
#' @param settings list of input arguments that can be used by `settings` input argument of `taskFunction`
#' @return Updated `Workflow` object
#' @export
#' @family workflow tasks
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)
#'
#' # Create a function to output results for workflow
#' # Mean model workflows functions usually expect SimulationStructure objects as inputs
#' userDefinedFunction <- function(structureSet, settings = NULL) {
#'   # Insert some code to calculate results and tune their output
#'   userResults <- list()
#'   userPlot <- ggplot()
#'   userTable <- data.frame()
#'   userText <- "Comments about the results of thMy user results"
#'
#'   userResults[[1]] <- saveTaskResults(
#'     id = "user-result-1",
#'     plot = userPlot,
#'     plotCaption = "Title of Figure 1",
#'     includePlot = TRUE,
#'     table = userTable,
#'     tableCaption = "Title of Table 1",
#'     includeTable = TRUE,
#'     textChunk = userText,
#'     includeTextChunk = TRUE
#'   )
#'
#'   # Tasks will run through the list of userResults
#'   # And add their outputs to the report
#'   return(userResults)
#' }
#'
#' # Add a user-defined task to workflow
#' addUserDefinedTask(
#'   workflow = myWorkflow,
#'   taskFunction = userDefinedFunction,
#'   taskName = "userDefinedTask",
#'   active = TRUE
#' )
#'
#' # Checks structure of task using
#' # myWorkflow$userDefinedTasks[[index]]$
#' }
#'
addUserDefinedTask <- function(workflow,
                               taskFunction,
                               taskName = "userDefinedTask",
                               active = TRUE,
                               settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsOfType(taskFunction, "function")
  validateIsString(taskName)
  validateIsLogical(active)

  # Get the names of input arguments of the function
  argumentNames <- names(formals(taskFunction))
  if (isOfType(workflow, "MeanModelWorkflow")) {
    # PlotTask arguments
    validateIsIncludedAndLog(
      c("structureSet", "settings"),
      argumentNames,
      groupName = "Task function arguments"
    )

    workflow$userDefinedTasks <- c(
      workflow$userDefinedTasks,
      PlotTask$new(
        reportTitle = taskName,
        fileName = paste0("appendix-", taskName, ".md"),
        getTaskResults = taskFunction,
        nameTaskResults = getObjectNameAsString(taskFunction),
        outputFolder = taskName,
        workflowFolder = workflow$workflowFolder,
        active = active,
        message = paste0(taskName, " (user defined)"),
        settings = settings
      )
    )
  }

  if (isOfType(workflow, "PopulationWorkflow")) {
    # PopulationPlotTask arguments
    validateIsIncludedAndLog(
      c("structureSets", "settings", "workflowType", "xParameters", "yParameters"),
      argumentNames,
      groupName = "Task function arguments"
    )

    workflow$userDefinedTasks <- c(
      workflow$userDefinedTasks,
      PopulationPlotTask$new(
        workflowType = workflow$workflowType,
        xParameters = NULL,
        yParameters = NULL,
        reportTitle = taskName,
        fileName = paste0("appendix-", taskName, ".md"),
        getTaskResults = taskFunction,
        nameTaskResults = getObjectNameAsString(taskFunction),
        outputFolder = taskName,
        workflowFolder = workflow$workflowFolder,
        active = active,
        message = paste0(taskName, " (user defined)"),
        settings = settings
      )
    )
  }
  logDebug(paste0("User defined task '", taskName, "' successfully loaded on workflow"))
  return(invisible())
}

#' @title loadPlotTimeProfilesTask
#' @description
#' Define `plotTimeProfiles` task and its settings
#' @param workflow `QualificationWorkflow` object
#' @param configurationPlan A `ConfigurationPlan` object
#' @return A `QualificationTask` object
#' @export
#' @import ospsuite.utils
#' @family workflow tasks
loadQualificationTimeProfilesTask <- function(workflow, configurationPlan) {
  validateIsOfType(workflow, "QualificationWorkflow")
  validateIsOfType(configurationPlan, "ConfigurationPlan")

  # Time Profiles task is only active if the field is defined & not empty
  active <- !isOfLength(configurationPlan$plots$TimeProfile, 0)

  taskFunction <- plotQualificationTimeProfiles
  nameFunction <- getObjectNameAsString(plotQualificationTimeProfiles)

  # Get list of task required input files
  inputFiles <- NULL
  outputs <- getTimeProfileOutputsDataframe(configurationPlan)
  if (!isEmpty(outputs)) {
    # data.frame to list
    outputs <- split(outputs, seq_len(nrow(outputs)))
    inputFiles <- as.character(sapply(outputs, function(output) {
      configurationPlan$getSimulationResultsPath(project = output$project, simulation = output$simulation)
    }))
  }

  return(QualificationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = inputFiles,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotTimeProfiles,
    settings = list(axes = getAxesPropertiesFromName(
      configurationPlan = configurationPlan,
      plotName = "TimeProfile"
    ))
  ))
}


#' @title loadGOFMergedTask
#' @param workflow `QualificationWorkflow` object
#' @param configurationPlan A `ConfigurationPlan` object
#' @return A `QualificationTask` object
#' @export
#' @family workflow tasks
loadGOFMergedTask <- function(workflow, configurationPlan) {
  validateIsOfType(workflow, "QualificationWorkflow")
  validateIsOfType(configurationPlan, "ConfigurationPlan")

  # Time Profiles task is only active if the field is defined & not empty
  active <- !isOfLength(configurationPlan$plots$GOFMergedPlots, 0)

  taskFunction <- plotQualificationGOFs
  nameFunction <- getObjectNameAsString(plotQualificationGOFs)

  # Get list of task required input files
  inputFiles <- NULL
  outputs <- getGOFOutputsDataframe(configurationPlan)
  if (!isEmpty(outputs)) {
    # data.frame to list
    outputs <- split(outputs, seq_len(nrow(outputs)))
    inputFiles <- as.character(sapply(outputs, function(output) {
      configurationPlan$getSimulationResultsPath(project = output$project, simulation = output$simulation)
    }))
  }

  return(QualificationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = inputFiles,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotGOFMerged,
    settings = list(
      predictedVsObserved = list(axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "GOFMergedPlotsPredictedVsObserved"
      )),
      residualsOverTime = list(axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "GOFMergedPlotsResidualsOverTime"
      )),
      digits = reEnv$formatNumericsDigits,
      nsmall = reEnv$formatNumericsSmall,
      scientific = reEnv$formatNumericsScientific
    )
  ))
}


#' @title loadQualificationComparisonTimeProfileTask
#' @description
#' Define `plotComparisonTimeProfileTask` task and its settings
#' @param workflow `QualificationWorkflow` object
#' @param configurationPlan A `ConfigurationPlan` object
#' @return A `QualificationTask` object
#' @export
#' @family workflow tasks
loadQualificationComparisonTimeProfileTask <- function(workflow, configurationPlan) {
  validateIsOfType(workflow, "QualificationWorkflow")
  validateIsOfType(configurationPlan, "ConfigurationPlan")

  # Time Profiles task is only active if the field is defined & not empty
  active <- !isOfLength(configurationPlan$plots$ComparisonTimeProfilePlots, 0)

  taskFunction <- plotQualificationComparisonTimeProfile
  nameFunction <- getObjectNameAsString(plotQualificationComparisonTimeProfile)

  # Get list of task required input files
  inputFiles <- NULL
  outputs <- getComparisonTimeProfileOutputsDataframe(configurationPlan)
  if (!isEmpty(outputs)) {
    # data.frame to list
    outputs <- split(outputs, seq_len(nrow(outputs)))
    inputFiles <- as.character(sapply(outputs, function(output) {
      configurationPlan$getSimulationResultsPath(project = output$project, simulation = output$simulation)
    }))
  }

  return(QualificationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = inputFiles,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotComparisonTimeProfiles,
    settings = list(
      axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "ComparisonTimeProfile"
      )
    )
  ))
}

#' @title loadPlotPKRatioTask
#' @description
#' Define `plotPKRatio` task and its settings
#' @param workflow `QualificationWorkflow` object
#' @param configurationPlan A `ConfigurationPlan` object
#' @return A `QualificationTask` object
#' @export
#' @family workflow tasks
loadPlotPKRatioTask <- function(workflow, configurationPlan) {
  validateIsOfType(workflow, "QualificationWorkflow")
  validateIsOfType(configurationPlan, "ConfigurationPlan")

  # Time Profiles task is only active if the field is defined & not empty
  active <- !isOfLength(configurationPlan$plots$PKRatioPlots, 0)

  taskFunction <- plotQualificationPKRatio
  nameFunction <- getObjectNameAsString(plotQualificationPKRatio)

  # Get list of task required input files
  inputFiles <- NULL
  outputs <- getPKRatioOutputsDataframe(configurationPlan)
  if (!isEmpty(outputs)) {
    # data.frame to list
    outputs <- split(outputs, seq_len(nrow(outputs)))
    inputFiles <- as.character(sapply(outputs, function(output) {
      configurationPlan$getPKAnalysisResultsPath(project = output$project, simulation = output$simulation)
    }))
  }

  return(QualificationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = inputFiles,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotPKRatio,
    settings = list(
      axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "PKRatioPlots"
      ),
      digits = reEnv$formatNumericsDigits,
      nsmall = reEnv$formatNumericsSmall,
      scientific = reEnv$formatNumericsScientific,
      # Reference unit for table artifact as named list
      # Note that
      # 1) it can be updated before running workflows
      # 2) configuration plan inputs could override these
      # 3) all PK Parameters use their default ospsuite unit
      units = as.list(
        c(
          # Keep these 2 for backward compatibility
          AUC = ospsuite::ospUnits$`AUC [mass]`$`µg*h/l`,
          CL = ospsuite::ospUnits$Flow$`l/h`,
          # All remaining PK Parameters
          sapply(
            ospsuite::allPKParameterNames(),
            function(pkParameterName) {
              ospsuite::pkParameterByName(pkParameterName)$displayUnit
            }
          )
        )
      )
    )
  ))
}


#' @title loadPlotDDIRatioTask
#' @description
#' Define `plotDDIRatio` task and its settings
#' @param workflow `QualificationWorkflow` object
#' @param configurationPlan A `ConfigurationPlan` object
#' @return A `QualificationTask` object
#' @export
#' @family workflow tasks
loadPlotDDIRatioTask <- function(workflow, configurationPlan) {
  validateIsOfType(workflow, "QualificationWorkflow")
  validateIsOfType(configurationPlan, "ConfigurationPlan")

  # Time Profiles task is only active if the field is defined & not empty
  active <- !isOfLength(configurationPlan$plots$DDIRatioPlots, 0)

  taskFunction <- plotQualificationDDIs
  nameFunction <- getObjectNameAsString(substitute(plotQualificationDDIs))

  # Get list of task required input files
  inputFiles <- NULL
  outputs <- getDDIOutputsDataframe(configurationPlan)
  if (!isEmpty(outputs)) {
    # data.frame to list
    outputs <- split(outputs, seq_len(nrow(outputs)))
    inputFiles <- as.character(sapply(outputs, function(output) {
      configurationPlan$getPKAnalysisResultsPath(project = output$project, simulation = output$simulation)
    }))
  }

  return(QualificationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = inputFiles,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotDDIRatio,
    settings = list(
      predictedVsObserved = list(axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "DDIRatioPlotsPredictedVsObserved"
      )),
      residualsOverTime = list(axes = getAxesPropertiesFromName(
        configurationPlan = configurationPlan,
        plotName = "DDIRatioPlotsResidualsVsObserved"
      )),
      digits = reEnv$formatNumericsDigits,
      nsmall = reEnv$formatNumericsSmall,
      scientific = reEnv$formatNumericsScientific
    )
  ))
}
