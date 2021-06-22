#' @title loadQualificationWorkflow
#' @description Load a `ConfigurationPlan` object from json file
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
#' @return A `QualificationWorkflow` object
#' @export
loadQualificationWorkflow <- function(workflowFolder, configurationPlanFile) {
  configurationPlan <- loadConfigurationPlan(
    workflowFolder = workflowFolder,
    configurationPlanFile = configurationPlanFile
  )
  # TODO outputs and pk parameters may need to be defined
  # Either as part of simulationSets or as a settings for the task
  # Currently, no output is saved as simulationResults
  simulationSets <- list()
  for (simulationIndex in 1:nrow(configurationPlan$simulationMappings)) {
    project <- configurationPlan$simulationMappings$project[simulationIndex]
    simulationName <- configurationPlan$simulationMappings$simulation[simulationIndex]
    simulationFile <- configurationPlan$getSimulationPath(project = project, simulation = simulationName)

    simulation <- loadSimulation(simulationFile)
    outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
      Output$new(output$path)
    })

    # simulationSetName defined as project-simulation uniquely identifies the simulation
    simulationSets[[simulationIndex]] <- SimulationSet$new(
      simulationSetName = paste(project, simulationName, sep = "-"),
      simulationFile = simulationFile,
      outputs = c(outputs)
    )
  }
  workflow <- QualificationWorkflow$new(
    simulationSets = simulationSets,
    workflowFolder = workflowFolder,
    configurationPlan = configurationPlan
  )
  return(workflow)
}

#' @title loadConfigurationPlan
#' @description Load a `ConfigurationPlan` object from json file
#' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
#' @param workflowFolder path of the output folder created or used by the Workflow.
#' @return A `ConfigurationPlan` object including the content of json file
#' @export
loadConfigurationPlan <- function(configurationPlanFile, workflowFolder) {
  jsonConfigurationPlan <- jsonlite::fromJSON(configurationPlanFile, simplifyDataFrame = FALSE)

  # Check if mandatory variables were input
  # Matlab version had as well ObservedDataSets and Inputs, but they don't need to be mandatory in R
  validateIsIncluded(c("SimulationMappings", "Plots", "Sections"), names(jsonConfigurationPlan))

  # Create `ConfigurationPlan` object
  configurationPlan <- ConfigurationPlan$new()
  # The workflow and reference folders are required to know from where accessing the files
  configurationPlan$workflowFolder <- workflowFolder
  configurationPlan$referenceFolder <- normalizePath(path = dirname(configurationPlanFile), winslash = .Platform$file.sep)

  # Assiociate fields defined in json to ConfigurationPlan object using expression
  jsonFieldNames <- names(jsonConfigurationPlan)
  # jsonFieldNames is almost camel case, only first letter needs to be switched to lower case
  fieldNames <- paste0(tolower(substring(jsonFieldNames, 1, 1)), substring(jsonFieldNames, 2))
  eval(parse(text = paste0("configurationPlan$", fieldNames, "<- jsonConfigurationPlan$", jsonFieldNames)))

  return(configurationPlan)
}

#' @title sectionsAsDataFrame
#' @description Recursively parse Sections field of configuration plan
#' to create a data.frame easier to handle by the wrkflow
#' @param sectionsIn list including Id and Title of section
#' @param sectionsOut data.frame including id, path, title
#' @param parentFolder For subsections only, path of parent section
#' @param sectionLevel Section level defining the level of markdown title
#' @return A data.frame including information about every section and subsection
sectionsAsDataFrame <- function(sectionsIn, sectionsOut = data.frame(), parentFolder = NULL, sectionLevel = 1) {
  # If sections are already as a data.frame format,
  # return them after checking that every field is present
  if (isOfType(sectionsIn, "data.frame")) {
    validateIsIncluded(c("id", "title", "content", "index", "path", "md"), names(sectionsIn))
    return(sectionsIn)
  }
  # Parse every section
  for (section in sectionsIn) {
    # sectionIndex ensures that folder names are in correct order and have unique names
    sectionIndex <- nrow(sectionsOut) + 1
    validateIsIncluded(c("Id", "Title"), names(section))
    # Actual section path will be relative to the workflowFolder
    # and is wrapped in method configurationPlan$getSectionPath(id)
    sectionPath <- paste0(parentFolder,
                          sprintf("%0.3d_%s", sectionIndex, removeForbiddenLetters(section$Title)),
                          sep = .Platform$file.sep
    )

    sectionMarkdown <- sprintf("%0.3d_%s.md", sectionIndex, removeForbiddenLetters(section$Title))

    # section data.frame with every useful information
    sectionOut <- data.frame(
      id = section$Id,
      title = section$Title,
      content = section$Content %||% NA,
      index = sectionIndex,
      path = sectionPath,
      md = sectionMarkdown,
      level = sectionLevel,
      stringsAsFactors = FALSE
    )
    sectionsOut <- rbind.data.frame(sectionsOut, sectionOut, stringsAsFactors = FALSE)

    # If subsections are included and not empty
    # Update sectionsOut data.frame
    if (!isOfLength(section$Sections, 0)) {
      sectionsOut <- sectionsAsDataFrame(
        sectionsIn = section$Sections,
        sectionsOut = sectionsOut,
        parentFolder = sectionPath,
        sectionLevel = sectionLevel + 1
      )
    }
  }
  return(sectionsOut)
}

#' @title createSectionOutput
#' @description Create the Sections output and their markdown content
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder path where logs are saved
#' @return Names of created appendices
createSectionOutput <- function(configurationPlan, logFolder = getwd()) {
  # Intro
  resetReport(configurationPlan$getIntroMarkdown())
  appendices <- configurationPlan$getIntroMarkdown()
  for (intro in configurationPlan$intro) {
    configurationPlan$copyIntro(intro, logFolder = logFolder)
  }
  # Sections
  for (sectionIndex in configurationPlan$sections$index) {
    sectionId <- configurationPlan$sections$id[sectionIndex]
    # Create section output path
    dir.create(configurationPlan$getSectionPath(sectionId), showWarnings = FALSE, recursive = TRUE)
    # Initialize markdown appendices
    resetReport(configurationPlan$getSectionMarkdown(sectionId), logFolder = logFolder)
    appendices <- c(appendices, configurationPlan$getSectionMarkdown(sectionId))
    # Possibility to add title to section content if not defined in sections
    # addTextChunk(configurationPlan$getSectionTitle(sectionId),
    #             fileName = configurationPlan$getSectionMarkdown(sectionId),
    #             logFolder = logFolder)

    # Add info from content to section content
    configurationPlan$copySectionContent(sectionId, logFolder = logFolder)
  }
  # Inputs
  for (input in configurationPlan$inputs) {
    configurationPlan$copyInput(input, logFolder = logFolder)
  }
  return(appendices)
}



#' @title getOutputsFromTimeProfileConfiguration
#' @description Get a vector of output paths that are to be used in a time profile plot
#' @param plot is a descriptor of the plot that is read from the `ConfigurationPlan` that is generated by the configuration plan json
#' @return A vector of output paths that are to be used in the time profile plot descriptor `plot`
getOutputsFromTimeProfileConfiguration <- function(plot) {
  validateIsIncluded(values = "Plot", parentValues = names(plot), nullAllowed = TRUE)
  validateIsIncluded(values = "Curves", parentValues = names(plot[["Plot"]]), nullAllowed = FALSE)

  paths <- NULL
  for (curve in plot$Plot$Curves) {
    validateIsString(object = curve$Y)
    if (ospsuite::toPathArray(curve$Y)[2] == "ObservedData") {
      next
    }
    paths <- c(paths, ospsuite::toPathString(tail(ospsuite::toPathArray(curve$Y), -1)))
  }
  return(unique(paths))
}



#' @title getOutputsFromGOFMergedPlotsConfiguration
#' @description Get a vector of output paths that are to be used in a GOF merged plot
#' @param plot is a descriptor of the plot that is read from the `ConfigurationPlan` that is generated by the configuration plan json
#' @return A vector of output paths that are to be used in the GOF merged plot descriptor `plot`
getOutputsFromGOFMergedPlotsConfiguration <- function(plot) {
  validateIsIncluded(values = "Groups", parentValues = names(plot), nullAllowed = TRUE)
  paths <- NULL
  for (group in plot$Groups) {
    validateIsIncluded(values = "OutputMappings", parentValues = names(group), nullAllowed = TRUE)
    for (outputMapping in group$OutputMappings) {
      validateIsIncluded(values = "Output", parentValues = names(outputMapping), nullAllowed = TRUE)
      validateIsString(object = outputMapping$Output)
      paths <- c(paths, outputMapping$Output)
    }
  }
  return(unique(paths))
}


# Vector of allowable plot types in configuration plan.
# TODO deprecate vector using enum ConfigurationPlots
QualificationPlotTypes <- c("GOFMergedPlots", "ComparisonTimeProfilePlots", "DDIRatioPlots", "TimeProfile")




#' @title separateVariableFromUnit
#' @description Split a string containing a varialbe and unit description into a list of two strings that separates the variable and the unit.
#' @param variableUnitString is a string that has the format 'Variable [unit]' or 'Variable'
#' @return A named list, with fields 'name' and 'unit'.  If variableUnitString lacks a '[unit]' substring, then the unit field is returned as an empty string.
separateVariableFromUnit <- function(variableUnitString) {
  splitVariableUnitString <- strsplit(x = sub("[ []", replacement = "", x = variableUnitString), split = "[][]")[[1]]
  return(list(name = splitVariableUnitString[1],
              unit = ifelse(test = is.na(x = splitVariableUnitString[2] ) , yes = "",no = splitVariableUnitString[2])))
}


#' @title parseObservationsDataFrame
#' @description Function to read the variable names and units in the first two columns of an observations data frame used for qualification. First column stores timepoints.  Second column stores measurements of a quantity corresponding to timepoints in first column.
#' @param observationsDataFram
#' @return A named list with `time` and `output` fields.  Each field contains a list that is output by `separateVariableFromUnit` with fields that store the variable name and the unit.
parseObservationsDataFrame <- function(observationsDataFrame){
  namesObservationsDataFrame <- names(observationsDataFrame)
  return(list(time = separateVariableFromUnit(namesObservationsDataFrame[1]),
              output = separateVariableFromUnit(namesObservationsDataFrame[2])))
}



#' @title massMoleConversion
#' @description Function to map `Concentration (mass)` or `Mass` dimensions to `Concentration (molar)` and `Amount` respectively.
#' @param dimension is string that is from among the valid OSP dimensions
#' @return If `dimension` is `Concentration (mass)` or `Mass`, then return `Concentration (molar)` or `Amount` respectively, otherwise return `dimension`.
massMoleConversion <- function(dimension){
  massMoleConversionList <- list()
  massMoleConversionList[[ospDimensions$Mass]] <- ospDimensions$Amount
  massMoleConversionList[[ospDimensions$`Concentration (mass)`]] <- ospDimensions$`Concentration (molar)`
  return(ifelse(test = dimension %in% c(ospDimensions$Mass,ospDimensions$`Concentration (mass)`),
                yes = massMoleConversionList[[dimension]],
                no = dimension))
}


#' @title getAxesSettings
#' @description Read axes settings for plots.
#' @param axesSettingsFromConfigurationPlot is a field from the `configurationPlan$plots` list
#' @return `axesSettings`, a list of settings for each of the X and Y axis.  Each list contains the unit, dimensions, and scaling type for each axes and option to plot grid lines.
getAxesSettings <- function(axesSettingsFromConfigurationPlot){
  axesSettings <- lapply(axesSettingsFromConfigurationPlot,function(x){list(unit = x$Unit,
                                                                            dimension = x$Dimension,
                                                                            gridLines = x$GridLines,
                                                                            scaling = x$Scaling)})
  names(axesSettings) <- sapply(axesSettingsFromConfigurationPlot,function(x)x$Type)
  return(axesSettings)
}


#' @title getQualificationGOFPlotData
#' @description Build dataframes and metadata for each GOF plot
#' @param configurationPlan A `ConfigurationPlan` object
#' @return plotGOFdata, a list of lists of the form list(dataframe,metadata) specific to each GOF plot
getQualificationGOFPlotData <- function(configurationPlan){

  plotGOFdata <- list()
  for (plt in seq_along(configurationPlan$plots$GOFMergedPlots)){

    gofPlotConfiguration <- configurationPlan$plots$GOFMergedPlots[[plt]]


    plotGOFDataframe <- NULL
    plotGOFMetadata <- list()
    plotGOFMetadata$title <- gofPlotConfiguration$Title
    plotGOFMetadata$sectionID <- gofPlotConfiguration$SectionId
    plotGOFMetadata$plotTypes <-  ospsuite::toPathArray(gofPlotConfiguration$PlotType)


    plotGOFMetadata$axesSettings <- lapply(plotGOFMetadata$plotTypes,function(pltType){ getAxesSettings( configurationPlan$plots$AxesSettings[[ gofPlotAxesSettings[[pltType]]  ]] ) } )
    names(plotGOFMetadata$axesSettings) <- plotGOFMetadata$plotTypes

    plotGOFMetadata$groups <- list()


    for (group in seq_along(gofPlotConfiguration$Groups)) {
      gofPlotGroup <- gofPlotConfiguration$Groups[[group]]
      caption <- gofPlotGroup$Caption
      symbol <- gofPlotConfiguration$Groups[[group]]$Symbol
      outputMappings <- gofPlotGroup$OutputMappings

      plotGOFMetadata$groups[[caption]] <- list()
      plotGOFMetadata$groups[[caption]]$outputMappings <- list()
      plotGOFMetadata$groups[[caption]]$symbol <- symbol


      for (omap in seq_along(outputMappings)) {

        outputMapping <- outputMappings[[omap]]

        projectName <- outputMapping$Project
        simulationName <- outputMapping$Simulation
        outputPath <- outputMapping$Output
        color <- outputMapping$Color

        observedDataPathInSimulation <- outputMapping$Output
        observedDataSet <- outputMapping$ObservedData
        observedDataSetFilePath <- configurationPlan$observedDataSets[configurationPlan$observedDataSets$id == outputMapping$ObservedData, ]$path

        simulationFile <- configurationPlan$getSimulationPath(
          project = projectName,
          simulation = simulationName
        )

        simulationResultsFile <- configurationPlan$getSimulationResultsPath(
          project = projectName,
          simulation = simulationName
        )

        simulation <- loadSimulation(simulationFile,loadFromCache = TRUE)
        simulationResults <- importResultsFromCSV(simulation = simulation, filePaths = simulationResultsFile)

        outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
          Output$new(output$path)
        })
        names(outputs) <- lapply(simulation$outputSelections$allOutputs, function(output) {
          output$path
        })
        output <- outputs[[outputPath]]
        molWeight <- simulation$molWeightFor(outputPath)
        simulationDimension <- getQuantity(path = outputPath,container = simulation)$dimension



        #Setup simulations dataframe
        simulatedDataStandardized <- data.frame(Time = simulationResults$timeValues,
                                                Concentration = simulationResults$getValuesByPath(path = outputPath,individualIds = 0))

        #Setup observations dataframe
        observedDataFileData <- read.csv(file.path(inputFolder, observedDataSetFilePath),check.names = FALSE ,fileEncoding = "UTF-8-BOM")
        observedDataFileMetaData <- parseObservationsDataFrame(observedDataFileData)
        #If simulation in c("Amount","'"Concentration (molar)") and observations in c("Mass","'"Concentration (mass)"), convert observations to c("Amount","'"Concentration (molar)")
        if (simulationDimension %in% c(ospDimensions$Amount,ospDimensions$`Concentration (molar)`)){
          observationsDimension <- massMoleConversion(ospsuite::getDimensionForUnit(observedDataFileMetaData$output$unit))
        }
        #Verify that simulations and observations have same dimensions
        validateIsIncluded(values = observationsDimension,parentValues = simulationDimension,nullAllowed = FALSE)

        observedDataStandardized <- observedDataFileData[,c(1,2)]
        names(observedDataStandardized) <-c("Time","Concentration")
        observedDataStandardized$Time <- ospsuite::toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                                              values = observedDataStandardized$Time,
                                                              unit = observedDataFileMetaData$time$unit)
        observedDataStandardized$Concentration <- ospsuite::toBaseUnit(quantityOrDimension = observationsDimension,
                                                                       values = observedDataStandardized$Concentration,
                                                                       unit = observedDataFileMetaData$output$unit,
                                                                       molWeight = molWeight)


        commonTimePoints <- intersect(observedDataStandardized$Time,simulatedDataStandardized$Time)



        #Setup dataframe of GOF data
        gofData <- data.frame(time = commonTimePoints,
                              observed  = observedDataStandardized$Concentration[observedDataStandardized$Time %in%  commonTimePoints ] ,
                              simulated  = simulatedDataStandardized$Concentration[simulatedDataStandardized$Time %in%  commonTimePoints ] ,
                              group = caption,
                              outputMapping = outputPath)


        plotGOFDataframe <- rbind.data.frame(plotGOFDataframe,gofData)

        plotGOFMetadata$groups[[caption]]$outputMappings[[outputPath]] <- list(molWeight = simulation$molWeightFor(outputPath),
                                                                               color = color,
                                                                               project = projectName,
                                                                               simulation = simulationName)

      }
    }
    plotGOFdata[[plt]] <- list(dataframe  = plotGOFDataframe, metadata = plotGOFMetadata)
  }
  return(plotGOFdata)
}






#' @title buildQualificationGOFPredictedVsObserved
#' @description Build dataframe for observation vs prediction
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @return dataframe for plotting goodness of fit of predictedVsObserved type
buildQualificationGOFPredictedVsObserved <- function(dataframe,
                                                     metadata) {

  axesSettings <- metadata$axesSettings[["predictedVsObserved"]]

  xUnit <- axesSettings$X$unit
  xDimension <- massMoleConversion(axesSettings$X$dimension)
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- massMoleConversion(axesSettings$Y$dimension)
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  #function to do obs vs sim
  #predictedVsObserved|residualsOverTime
  gofPlotDataframe <- NULL
  for (grp in unique(dataframe$group)){
    for (omap in unique( dataframe[dataframe$group == grp,]$outputMapping )){
      molWeight <- metadata$groups[[grp]]$outputMappings[[omap]]$molWeight
      xData <-  dataframe[dataframe$group == grp & dataframe$outputMapping == omap,]$observed
      xData <- ospsuite::toUnit(quantityOrDimension = xDimension,
                                values = xData,
                                targetUnit = xUnit,
                                molWeight = molWeight)
      if (xScaling == "Log"){
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      yData <-  dataframe[dataframe$group == grp & dataframe$outputMapping == omap,]$simulated
      yData <- ospsuite::toUnit(quantityOrDimension = yDimension,
                                values = yData,
                                targetUnit = yUnit,
                                molWeight = molWeight)
      if (yScaling == "Log"){
        yData <- log10(yData)
      }
      yData <- replaceInfWithNA(yData)

      df <- data.frame(Observed = xData,
                       Simulated = yData,
                       Group = grp,
                       Output = omap)

      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe,df)
    }
  }
  return(gofPlotDataframe)
}




#' @title buildQualificationGOFResidualsOverTime
#' @description Build dataframe for residuals vs time
#' @param dataframe data.frame
#' @param metadata meta data on `data`
#' @return dataframe for plotting goodness of fit of residuals vs time type
buildQualificationGOFResidualsOverTime <- function(dataframe,
                                                   metadata) {

  axesSettings <- metadata$axesSettings[["residualsOverTime"]]

  xUnit <- axesSettings$X$unit
  xDimension <- massMoleConversion(axesSettings$X$dimension)
  xScaling <- axesSettings$X$scaling
  xGridlines <- axesSettings$X$gridLines

  yUnit <- axesSettings$Y$unit
  yDimension <- massMoleConversion(axesSettings$Y$dimension)
  yScaling <- axesSettings$Y$scaling
  yGridlines <- axesSettings$Y$gridLines

  #function to do obs vs sim
  #predictedVsObserved|residualsOverTime
  gofPlotDataframe <- NULL
  for (grp in unique(dataframe$group)){
    for (omap in unique( dataframe[dataframe$group == grp,]$outputMapping )){

      molWeight <- metadata$groups[[grp]]$outputMappings[[omap]]$molWeight

      xData <-  dataframe[dataframe$group == grp & dataframe$outputMapping == omap,]$time
      xData <- ospsuite::toUnit(quantityOrDimension = xDimension,
                                values = xData,
                                targetUnit = xUnit,
                                molWeight = molWeight)
      if(xScaling == "Log"){
        xData <- log10(xData)
      }
      xData <- replaceInfWithNA(xData)

      simulated <-  dataframe[dataframe$group == grp & dataframe$outputMapping == omap,]$simulated
      observed  <-  dataframe[dataframe$group == grp & dataframe$outputMapping == omap,]$observed
      if (yScaling == "Log"){
        residualValues <-  log10(simulated) - log10(observed)
      } else {
        residualValues <- (simulated - observed)/observed
      }
      yData <- residualValues
      yData <- ospsuite::toUnit(quantityOrDimension = yDimension,
                                values = yData,
                                targetUnit = yUnit,
                                molWeight = molWeight)
      yData <- replaceInfWithNA(yData)

      df <- data.frame(Time = xData,
                       Residuals = yData,
                       Group = grp,
                       Output = omap)
      gofPlotDataframe <- rbind.data.frame(gofPlotDataframe,df)
    }
  }
  return(gofPlotDataframe)
}



#' @title plotQualificationGOFPredictedVsObserved
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFPredictedVsObserved <- function(data) {
  identityMinMax <- c(
    0.8 * min(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE),
    1.2 * max(cbind(data[, "Observed"], data[, "Simulated"]), na.rm = TRUE)
  )

  identityLine <- data.frame(
    "Observed" = identityMinMax,
    "Simulated" = identityMinMax,
    "Legend" = "Line of identity"
  )



  qualificationGOFPredictedVsObservedPlot <- tlf::addLine(
    data = identityLine,
    dataMapping = tlf::XYGDataMapping$new(x = "Observed", y = "Simulated", linetype = "Legend")
  )

  obsVsPredDataMapping <- tlf::XYGDataMapping$new(
    x = "Observed",
    y = "Simulated",
    shape = "Group",
    color = "Output"
  )

  qualificationGOFPredictedVsObservedPlot <- tlf::addScatter(
    data = data,
    dataMapping = obsVsPredDataMapping,
    plotObject = qualificationGOFPredictedVsObservedPlot
  )
  qualificationGOFPredictedVsObservedPlot <- tlf::setLegendPosition(plotObject = qualificationGOFPredictedVsObservedPlot,
                                                                    position = reDefaultLegendPosition)

  return(qualificationGOFPredictedVsObservedPlot)
}


#' @title plotQualificationGOFResidualsOverTime
#' @description Plot observation vs prediction for qualification workflow
#' @param data data.frame
#' @return ggplot object of residuals over time profile for qualification workflow
#' @import tlf
#' @import ggplot2
plotQualificationGOFResidualsOverTime <- function(data) {

  resVsTimeDataMapping <- tlf::XYGDataMapping$new(
    x = "Time",
    y = "Residuals",
    shape = "Group",
    color = "Output"
  )

  maxRes <- 1.2 * max(abs(data[, resVsTimeDataMapping$y]), na.rm = TRUE)

  qualificationGOFResVsTimePlot <- tlf::initializePlot()

  qualificationGOFResVsTimePlot <- tlf::addScatter(
    data = data,
    dataMapping = resVsTimeDataMapping,
    plotObject = qualificationGOFResVsTimePlot
  )

  qualificationGOFResVsTimePlot <- qualificationGOFResVsTimePlot + ggplot2::geom_hline(
    yintercept = 0,
    size = 1
  )

  qualificationGOFResVsTimePlot <- tlf::setLegendPosition(plotObject = qualificationGOFResVsTimePlot,
                                                          position = reDefaultLegendPosition)
  qualificationGOFResVsTimePlot <- tlf::setYAxis(
    plotObject = qualificationGOFResVsTimePlot,
    limits = c(-maxRes, maxRes)
  )

  return(qualificationGOFResVsTimePlot)
}



#' @title plotQualificationGOFs
#' @description Plot observation vs prediction for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @return list of qualification GOF ggplot objects
plotQualificationGOFs <- function(configurationPlan,
                                  logFolder = getwd(),
                                  settings){
  gofPlotsData <- getQualificationGOFPlotData(configurationPlan)
  gofPlotList <- list()
  gofPlotResults <- list()

  for (plotIndex in seq_along(gofPlotsData)){
    gofPlotList[[plotIndex]] <- list()
    dataframe <- gofPlotsData[[plotIndex]]$dataframe
    metadata <- gofPlotsData[[plotIndex]]$metadata
    for (plotType in metadata$plotTypes){

      plotID <- paste("GOFMergedPlot",plotIndex,plotType, sep = "-")

      plotGOFDataframe <- buildGOFDataFrameFunctions[[plotType]](dataframe,metadata)
      gofPlotList[[plotIndex]][[plotType]] <- plotGOFFunctions[[plotType]]( plotGOFDataframe )

      gofPlotResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = metadata$sectionID,
        plot = gofPlotList[[plotIndex]][[plotType]],
        plotCaption = metadata$title
      )
    }
  }
  return(gofPlotResults)
}

#' Names of fields in configuration plane containing axes settings data for each GOF plot type
gofPlotAxesSettings <-list("predictedVsObserved" = "GOFMergedPlotsPredictedVsObserved",
                           "residualsOverTime" = "GOFMergedPlotsResidualsOverTime")

#' Names of functions for extracting data for each GOF plot type
buildGOFDataFrameFunctions <- list("predictedVsObserved" = buildQualificationGOFPredictedVsObserved,
                                   "residualsOverTime" = buildQualificationGOFResidualsOverTime)

#' Names of functions for plotting GOF plots for each GOF plot type
plotGOFFunctions <- list("predictedVsObserved" = plotQualificationGOFPredictedVsObserved,
                         "residualsOverTime" = plotQualificationGOFResidualsOverTime)

