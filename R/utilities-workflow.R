#' @title getWorkflowReferencePopulationName
#' @description Get `simulationSetName` from the reference `PopulationSimulationSet` of a `Workflow` object
#' @param workflow A `PopulationWorkflow` object
#' @export
#' @family workflow helpers
getWorkflowReferencePopulationName <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  if (!isOfType(workflow, "PopulationWorkflow")) {
    return()
  }
  # Warn user in case his workflow ill-defined its reference population
  tryCatch(
    {
      simulationSets <- lapply(
        workflow$simulationStructures,
        function(structureSet) {
          structureSet$simulationSet
        }
      )
      validateHasReferencePopulation(workflow$workflowType, simulationSets)
    },
    error = function(e) {
      logError(messages$warningNoReferencePopulation(workflow$workflowType))
    }
  )
  referencePopulationName <- getReferencePopulationName(workflow$simulationStructures)
  return(referencePopulationName)
}

#' @title getReferencePopulationName
#' @description Get Simulation Set Name from reference Population Simulation Set
#' @param structureSets An array of `SimulationStructure` objects
#' @keywords internal
getReferencePopulationName <- function(structureSets) {
  allSimulationReferences <- sapply(
    structureSets,
    function(structureSet) {
      structureSet$simulationSet$referencePopulation
    }
  )
  referenceIndex <- head(which(allSimulationReferences), 1)
  # For parallel without reference population
  if (isEmpty(referenceIndex)) {
    return(NULL)
  }
  referencePopulationName <- structureSets[[referenceIndex]]$simulationSet$simulationSetName
  return(referencePopulationName)
}


#' @title setWorkflowParameterDisplayPathsFromFile
#' @description Set mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param fileName name of file that includes mapping of Parameters with their display paths
#' Names in header should include `parameter` and `displayPath`.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
setWorkflowParameterDisplayPathsFromFile <- function(fileName, workflow) {
  validateIsOfType(workflow, "Workflow")
  validateIsString(fileName)
  validateIsFileExtension(fileName, "csv")
  parameterDisplayPaths <- readObservedDataFile(fileName)
  workflow$setParameterDisplayPaths(parameterDisplayPaths)
  return(invisible())
}

#' @title setWorkflowParameterDisplayPaths
#' @description Set mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param parameterDisplayPaths data.frame mapping Parameters with their display paths
#' Variables of the data.frame should include `parameter` and `displayPath`.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
setWorkflowParameterDisplayPaths <- function(parameterDisplayPaths, workflow) {
  validateIsOfType(workflow, "Workflow")
  workflow$setParameterDisplayPaths(parameterDisplayPaths)
  return(invisible())
}

#' @title getWorkflowParameterDisplayPaths
#' @description Get mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
getWorkflowParameterDisplayPaths <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  return(workflow$getParameterDisplayPaths())
}

#' @title getXParametersForDemogrpahyPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x parameters used for demography range plots
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getXParametersForDemogrpahyPlot(workflow = myWorkflow)
#' }
#'
getXParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotDemography$xParameters)
}

#' @title getYParametersForDemogrpahyPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y parameters used for demography histogram and range plots
#' @import ospsuite.utils
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getYParametersForDemogrpahyPlot(workflow = myWorkflow)
#' }
#'
getYParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotDemography$yParameters %||% DemographyDefaultParameters)
}

#' @title setXParametersForDemogrpahyPlot
#' @description Set x parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in x-axis for range plots
#' setXParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
setXParametersForDemogrpahyPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters), nullAllowed = TRUE)

  workflow$plotDemography$xParameters <- parameters

  logDebug(paste0(
    "X-parameters: '",
    paste0(c(parameters), collapse = "', '"),
    "' set for demography plot."
  ))
  return(invisible())
}

#' @title addXParametersForDemogrpahyPlot
#' @description Append x parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' addXParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addXParametersForDemogrpahyPlot <- function(workflow, parameters) {
  updatedParameters <- c(getXParametersForDemogrpahyPlot(workflow), parameters)
  setXParametersForDemogrpahyPlot(workflow, updatedParameters)
}

#' @title setYParametersForDemogrpahyPlot
#' @description Set y-parameters for histograms and range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in y-axis for range plots and histograms
#' setYParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
setYParametersForDemogrpahyPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

  workflow$plotDemography$yParameters <- parameters

  logDebug(paste0(
    "Y-parameters: '",
    paste0(c(parameters), collapse = "', '"),
    "' set for demography plot."
  ))
  return(invisible())
}

#' @title addYParametersForDemogrpahyPlot
#' @description Append y parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in y-axis for range plots and histograms
#' addYParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addYParametersForDemogrpahyPlot <- function(workflow, parameters) {
  updatedParameters <- c(getYParametersForDemogrpahyPlot(workflow), parameters)
  setYParametersForDemogrpahyPlot(workflow, updatedParameters)
}

#' @title getXParametersForPKParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x-parameters used for PK parameters range plots
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getXParametersForPKParametersPlot(workflow = myWorkflow)
#' }
#'
getXParametersForPKParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotPKParameters$xParameters)
}

#' @title getYParametersForPKParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y-parameters used for PK parameters range plots and boxplots
#' @export
#' @import ospsuite.utils
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in y-axis for range plots and boxplots
#' getYParametersForPKParametersPlot(workflow = myWorkflow)
#' }
#'
getYParametersForPKParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  yParameters <- workflow$plotPKParameters$yParameters %||% workflow$simulationStructures[[1]]$simulationSet$outputs

  return(yParameters)
}

#' @title setXParametersForPKParametersPlot
#' @description Set x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in x-axis for range plots
#' setXParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
setXParametersForPKParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

  workflow$plotPKParameters$xParameters <- parameters

  logDebug(paste0(
    "X-parameters: '", paste0(c(parameters), collapse = "', '"),
    "' set for PK parameters plot."
  ))
  return(invisible())
}

#' @title addXParametersForPKParametersPlot
#' @description Append x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in x-axis for range plots
#' addXParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addXParametersForPKParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getXParametersForPKParametersPlot(workflow), parameters)
  setXParametersForPkParametersPlot(workflow, updatedParameters)
}

#' @title setYParametersForPKParametersPlot
#' @description Set y-parameters for boxplots and range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of R6 class `Output` objects to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in y-axis for range plots and boxplots
#' setYParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = Output$new(path, pkParameters)
#' )
#' }
#'
setYParametersForPKParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsOfType(c(parameters), "Output")

  workflow$plotPKParameters$yParameters <- parameters

  for (output in c(parameters)) {
    logDebug(paste0(
      "Y-parameters: '", paste0(c(output$pkParameters), collapse = "', '"),
      "' for '", output$path, "' set for PK parameters plot."
    ))
  }
  return(invisible())
}

#' @title addYParametersForPKParametersPlot
#' @description Append y-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in y-axis for range plots and boxplots
#' addYParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = Output$new(path, pkParameters)
#' )
#' }
#'
addYParametersForPKParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getYParametersForPkParametersPlot(workflow), parameters)
  setYParametersForPkParametersPlot(workflow, updatedParameters)
}

#' @rdname getXParametersForPKParametersPlot
#' @export
getXParametersForPkParametersPlot <- getXParametersForPKParametersPlot

#' @rdname getYParametersForPKParametersPlot
#' @export
getYParametersForPkParametersPlot <- getYParametersForPKParametersPlot

#' @rdname setXParametersForPKParametersPlot
#' @export
setXParametersForPkParametersPlot <- setXParametersForPKParametersPlot

#' @rdname setYParametersForPKParametersPlot
#' @export
setYParametersForPkParametersPlot <- setYParametersForPKParametersPlot

#' @rdname addXParametersForPKParametersPlot
#' @export
addXParametersForPkParametersPlot <- addXParametersForPKParametersPlot

#' @rdname addYParametersForPKParametersPlot
#' @export
addYParametersForPkParametersPlot <- addYParametersForPKParametersPlot
