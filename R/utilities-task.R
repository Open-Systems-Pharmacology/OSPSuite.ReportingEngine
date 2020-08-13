#' @title StandardSimulationTasks
#' @description Names of simulation tasks performed by both `MeanModelWorkflow` and `PopulationWorkflow` objects
#' @export
StandardSimulationTasks <- enum(c("simulate", "calculatePKParameters", "calculateSensitivity"))

#' @title StandardPlotTasks
#' @description Names of plot tasks performed by both `MeanModelWorkflow` and `PopulationWorkflow` objects
#' @export
StandardPlotTasks <- enum(c("plotTimeProfilesAndResiduals", "plotPKParameters", "plotSensitivity"))

#' @title activateWorkflowTasks
#' @description activates a series of `Tasks` from a `Workflow`
#' @param workflow `MeanModelWorklfow` or `PopulationWorklfow` object
#' @param tasks names of the tasks to activate
#' Default activates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
activateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")
  validateIsIncluded(tasks, workflow$getAllTasks(), groupName = "names of available workflow tasks", logFolder = workflow$workflowFolder)

  for (task in tasks) {
    workflow[[task]]$activate()
  }
}

#' @title inactivateWorkflowTasks
#' @description inactivates a series of `Tasks` from a `Workflow`
#' @param workflow `MeanModelWorklfow` or `PopulationWorklfow` object
#' @param tasks names of the tasks to activate
#' Default inactivates all tasks of the workflow using workflow method `workflow$getAllTasks()`
#' @export
inactivateWorkflowTasks <- function(workflow, tasks = workflow$getAllTasks()) {
  validateIsOfType(workflow, "Workflow")
  validateIsIncluded(tasks, workflow$getAllTasks(), groupName = "names of available workflow tasks", logFolder = workflow$workflowFolder)

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
loadSimulateTask <- function(workflow, active = TRUE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)
  taskFunction <- simulateModel
  nameFunction <- deparse(substitute(simulateModel))
  if (isOfType(workflow, "PopulationWorkflow")) {
    taskFunction <- simulateModelForPopulation
    nameFunction <- deparse(substitute(simulateModelForPopulation))
  }

  return(SimulationTask$new(
    getTaskResults = taskFunction,
    nameTaskResults = nameFunction,
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
loadCalculatePKParametersTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  return(CalculatePKParametersTask$new(
    getTaskResults = calculatePKParameters,
    nameTaskResults = deparse(substitute(calculatePKParameters)),
    outputFolder = defaultTaskOutputFolders$calculatePKParameters,
    inputFolder = defaultTaskOutputFolders$simulate,
    inputs = getSimulationResultFileNames(workflow),
    outputs = getPkAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    settings = settings,
    message = defaultWorkflowMessages$calculatePKParameters
  ))
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
loadCalculateSensitivityTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationSensitivityAnalysisTask$new(
      getTaskResults = runPopulationSensitivityAnalysis,
      nameTaskResults = deparse(substitute(runPopulationSensitivityAnalysis)),
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
    nameTaskResults = deparse(substitute(runSensitivity)),
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
loadPlotTimeProfilesAndResidualsTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  taskFunction <- plotMeanGoodnessOfFit
  if (isOfType(workflow, "PopulationWorkflow")) {
    taskFunction <- plotPopulationGoodnessOfFit
  }

  return(GofPlotTask$new(
    reportTitle = defaultWorkflowTitles$plotGoF,
    fileName = defaultWorkflowAppendices$plotGoF,
    getTaskResults = taskFunction,
    nameTaskResults = deparse(substitute(taskFunction)),
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
loadPlotPKParametersTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationPlotTask$new(
      workflowType = workflow$workflowType,
      xParameters = getDefaultPkParametersXParameters(workflow$workflowType),
      yParameters = NULL,
      reportTitle = defaultWorkflowTitles$plotPKParameters,
      fileName = defaultWorkflowAppendices$plotPKParameters,
      getTaskResults = plotPopulationPKParameters,
      nameTaskResults = deparse(substitute(plotPopulationPKParameters)),
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
    fileName = defaultWorkflowAppendices$plotPKParameters,
    getTaskResults = plotMeanPKParameters,
    nameTaskResults = deparse(substitute(plotMeanPKParameters)),
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
loadPlotSensitivityTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "Workflow")
  validateIsLogical(active)

  if (isOfType(workflow, "PopulationWorkflow")) {
    return(PopulationPlotTask$new(
      workflowType = workflow$workflowType,
      xParameters = NULL,
      yParameters = NULL,
      reportTitle = defaultWorkflowTitles$plotSensitivity,
      fileName = defaultWorkflowAppendices$plotSensitivity,
      getTaskResults = plotPopulationSensitivity,
      nameTaskResults = deparse(substitute(plotPopulationSensitivity)),
      outputFolder = defaultTaskOutputFolders$plotSensitivity,
      inputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
      inputs = getPopulationSensitivityAnalysisResultsFileNames(workflow),
      workflowFolder = workflow$workflowFolder,
      active = active,
      message = defaultWorkflowMessages$plotSensitivity,
      settings = settings %||% SensitivityPlotSettings$new()
    ))
  }
  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotSensitivity,
    fileName = defaultWorkflowAppendices$plotSensitivity,
    getTaskResults = plotMeanSensitivity,
    nameTaskResults = deparse(substitute(plotMeanSensitivity)),
    outputFolder = defaultTaskOutputFolders$plotSensitivity,
    inputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
    inputs = getMeanSensitivityAnalysisResultsFileNames(workflow),
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotSensitivity,
    settings = settings %||% SensitivityPlotSettings$new()
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
loadPlotMassBalanceTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "MeanModelWorkflow")
  validateIsLogical(active)

  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotMassBalance,
    fileName = defaultWorkflowAppendices$plotMassBalance,
    getTaskResults = plotMeanMassBalance,
    nameTaskResults = deparse(substitute(plotMeanMassBalance)),
    outputFolder = defaultTaskOutputFolders$plotMassBalance,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotMassBalance,
    settings = settings
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
loadPlotAbsorptionTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "MeanModelWorkflow")
  validateIsLogical(active)

  return(PlotTask$new(
    reportTitle = defaultWorkflowTitles$plotAbsorption,
    fileName = defaultWorkflowAppendices$plotAbsorption,
    getTaskResults = plotMeanAbsorption,
    nameTaskResults = deparse(substitute(plotMeanAbsorption)),
    outputFolder = defaultTaskOutputFolders$plotAbsorption,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotAbsorption,
    settings = settings
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
loadPlotDemographyTask <- function(workflow, active = FALSE, settings = NULL) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsLogical(active)

  return(PopulationPlotTask$new(
    workflowType = workflow$workflowType,
    xParameters = getDefaultDemographyXParameters(workflow$workflowType),
    yParameters = NULL,
    reportTitle = defaultWorkflowTitles$plotDemography,
    fileName = defaultWorkflowAppendices$plotDemography,
    getTaskResults = plotDemographyParameters,
    nameTaskResults = deparse(substitute(plotDemographyParameters)),
    outputFolder = defaultTaskOutputFolders$plotDemography,
    workflowFolder = workflow$workflowFolder,
    active = active,
    message = defaultWorkflowMessages$plotDemography,
    settings = settings
  ))
}

#' @title getSimulationResultFileNames
#' @description
#' Get the expected simulation result files obtained from a workflow
#' @param workflow `Workflow` object or derived class
#' @return Names of the the expected simulation result files
getSimulationResultFileNames <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  simulationResultFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    simulationResultFileNames <- c(simulationResultFileNames, structureSet$simulationResultFileNames)
  }
  return(simulationResultFileNames)
}

#' @title getPkAnalysisResultsFileNames
#' @description
#' Get the expected PK analysis result files obtained from a workflow
#' @param workflow `Workflow` object or derived class
#' @return Names of the the expected PK analysis result files
getPkAnalysisResultsFileNames <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  pkAnalysisResultsFileNames <- NULL
  for (structureSet in workflow$simulationStructures) {
    pkAnalysisResultsFileNames <- c(pkAnalysisResultsFileNames, structureSet$pkAnalysisResultsFileNames)
  }
  return(pkAnalysisResultsFileNames)
}

#' @title getMeanSensitivityAnalysisResultsFileNames
#' @description
#' Get the expected sensitivity analysis result files obtained from a mean model workflow
#' @param workflow `MeanModelWorkflow` object or derived class
#' @return Names of the the expected sensitivity analysis result files
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
checkTaskInputsExist <- function(task) {
  validateIsOfType(task, "Task")
  return(sapply(task$getInputs(), file.exists))
}
