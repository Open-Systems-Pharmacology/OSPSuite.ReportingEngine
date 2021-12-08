exec <- function(text){
  eval(parse(text = text))
}

makeChildInitializer <- function(parentInitializerName, extendedInitializerName){

  #Get a list of arguments for the `parentInitializer` function and the `extendedInitializer` function
  parentInitializer.formals <- formals(eval(parse(text = parentInitializerName)))
  extendedInitializer.formals <- formals(eval(parse(text = extendedInitializerName)))

  childInitializer <- eval(parse(text = paste0("function(){
  eval(parse(text = (paste(tail(as.character(body(",parentInitializerName,")),-1),collapse = ';') )))
  eval(parse(text = (paste(tail(as.character(body(",extendedInitializerName,")),-1),collapse = ';') )))
  }")))


  #Set the arguments to the childInitializer function to be the union of the arguments of the `parentInitializer` function and the `extendedInitializer` function
  formals(childInitializer) <- c( formals(eval(parse(text = parentInitializerName))) , formals(eval(parse(text = extendedInitializerName))) )

  return(childInitializer)
}

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

  validateIsString(workflowFolder)
  validateIsString(watermark, nullAllowed = TRUE)
  validateIsString(simulationSetDescriptor, nullAllowed = TRUE)
  validateIsOfType(c(simulationSets), "SimulationSet")
  validateIsLogical(createWordReport)
  validateIsLogical(numberSections)

  self$createWordReport <- createWordReport
  self$numberSections <- numberSections
  if (!isOfType(simulationSets, "list")) {
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
  self$taskNames <- ospsuite::enum(self$getAllTasks())

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

  self$taskNames <- ospsuite::enum(self$getAllTasks())
}
