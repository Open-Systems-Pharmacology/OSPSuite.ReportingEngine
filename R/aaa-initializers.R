getClassAncestry <- function(clsVec){
  clsVec <- c(clsVec)
  nv <- clsVec[[length(clsVec)]]$get_inherit()
  if(is.null( nv )){
    return(clsVec)
  }
  return(getClassAncestry( c(clsVec,nv) ))
}

getAncestralInitializerList <- function(childClass){
  return(lapply(getClassAncestry(childClass),function(cls){ cls$public_methods$initialize }))
}

parseFunctionBody <- function(functionToParse) {
  ospsuite.utils::validateIsIncluded(values = typeof(functionToParse),parentValues= c("closure","function"))
  #read body of functionToParse, convert to character, removing first element (curly brackets), paste together all function commands into a string (separated by `;`)
  return(paste(tail(as.character(body(functionToParse)), -1), collapse = ";"))
}

#' @param parentInitializersList is an list of parent initializers. ordered from the most to the least superior.
#' @param extendedInitializer is an initializer function to be called after calling all parent classes by order of superiority
makeChildInitializer <- function(parentInitializersList, extendedInitializer) {

  ospsuite.utils::validateIsOfType(parentInitializersList,"list")
  sapply(parentInitializersList,function(fn){ ospsuite.utils::validateIsIncluded(values = typeof(fn),parentValues= c("closure","function")) })
  ospsuite.utils::validateIsIncluded(values = typeof(extendedInitializer),parentValues= c("closure","function"))

  #Recursive application of this function for cases in which there is multilevel inheritance, where parentInitializersList
  #Each recursion will return an `extendedInitializer` that is an amalgamation of the all but the first elements of parentInitializersList with extendedInitializer
  if (length(parentInitializersList) > 1) {
    extendedInitializer <- makeChildInitializer(tail(parentInitializersList, -1), extendedInitializer)
  }

  #If parentInitializersList includes only one element, amalgamate that function with extendedInitializer
  parentInitializer <- parentInitializersList[[1]]

  #Parse the body of the parent and child initializers into a series of commands (as strings) separated by semicolons (';')
  parentInitializerBody <- parseFunctionBody(parentInitializer)
  extendedInitializerBody <- parseFunctionBody(extendedInitializer)

  #amalgamate (as a string) the bodies of the parent and child classes into a new function with no input arguments
  childInitializerBody <- paste0("function(){
       eval(parse(text = '", parentInitializerBody, "' ))
       eval(parse(text = '", extendedInitializerBody, "' ))
     }")

  #create a function object based on the string `childInitializerBody``
  childInitializer <- eval(parse(text = childInitializerBody))

  # Set the arguments to the childInitializer function to be the union of the arguments of the `parentInitializer` function and the `extendedInitializer` function
  # Remove any duplicated arguments.  Arguments of the extendedInitializer overwrite arguments of the parentInitializer with the same name.
  childInitializerFormals <- c(formals(parentInitializer), formals(extendedInitializer))
  formals(childInitializer) <- childInitializerFormals[!duplicated(names(childInitializerFormals), fromLast = TRUE)]

  return(childInitializer)
}


#' @param simulationSets list of `SimulationSet` R6 class objects
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @param createWordReport logical of option for creating Markdown-Report only but not a Word-Report.
#' @param watermark displayed watermark in figures background
#' @param simulationSetDescriptor character Descriptor of simulation sets indicated in reports
#' @param numberSections logical defining if the report sections should be numbered
#' @param theme A `Theme` object from `{tlf}` package
workflowInitializeFunction <- function(simulationSets,
                                       workflowFolder,
                                       createWordReport = TRUE,
                                       watermark = NULL,
                                       simulationSetDescriptor = NULL,
                                       numberSections = TRUE,
                                       theme = NULL) {
  private$.reportingEngineInfo <- ReportingEngineInfo$new()
  # Empty list on which users can load tasks
  self$userDefinedTasks <- list()

  ospsuite.utils::validateIsString(workflowFolder)
  ospsuite.utils::validateIsString(watermark, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(simulationSetDescriptor, nullAllowed = TRUE)
  ospsuite.utils::validateIsOfType(c(simulationSets), "SimulationSet")
  ospsuite.utils::validateIsLogical(createWordReport)
  ospsuite.utils::validateIsLogical(numberSections)

  self$createWordReport <- createWordReport
  self$numberSections <- numberSections
  if (!ospsuite.utils::isOfType(simulationSets, "list")) {
    simulationSets <- list(simulationSets)
  }

  allSimulationSetNames <- sapply(simulationSets, function(set) {
    set$simulationSetName
  })
  validateNoDuplicatedEntries(allSimulationSetNames)

  self$workflowFolder <- workflowFolder
  workflowFolderCheck <- file.exists(self$workflowFolder)

  if (workflowFolderCheck) {
    logWorkflow(
      message = workflowFolderCheck,
      pathFolder = self$workflowFolder,
      logTypes = c(LogTypes$Debug)
    )
  }
  dir.create(self$workflowFolder, showWarnings = FALSE, recursive = TRUE)

  logWorkflow(
    message = private$.reportingEngineInfo$print(),
    pathFolder = self$workflowFolder
  )

  self$reportFileName <- file.path(self$workflowFolder, paste0(defaultFileNames$reportName(), ".md"))
  self$taskNames <- ospsuite.utils::enum(self$getAllTasks())

  self$simulationStructures <- list()
  simulationSets <- c(simulationSets)
  for (simulationSetIndex in seq_along(simulationSets)) {
    self$simulationStructures[[simulationSetIndex]] <- SimulationStructure$new(
      simulationSet = simulationSets[[simulationSetIndex]],
      workflowFolder = self$workflowFolder
    )
  }
  self$setSimulationDescriptor(simulationSetDescriptor %||% reEnv$defaultSimulationSetDescriptor)

  # Load default workflow theme, and sync the watermark
  setDefaultTheme(theme)
  self$setWatermark(watermark)
}


meanModelWorkflowExtensionInitializeFunction <- function() {
  self$simulate <- loadSimulateTask(self)
  self$calculatePKParameters <- loadCalculatePKParametersTask(self)
  self$calculateSensitivity <- loadCalculateSensitivityTask(self)

  self$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(self)
  self$plotMassBalance <- loadPlotMassBalanceTask(self)
  self$plotAbsorption <- loadPlotAbsorptionTask(self)
  self$plotPKParameters <- loadPlotPKParametersTask(self)
  self$plotSensitivity <- loadPlotSensitivityTask(self)

  self$taskNames <- ospsuite.utils::enum(self$getAllTasks())
}
