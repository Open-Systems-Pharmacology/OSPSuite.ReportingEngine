#' @title plotQualificationPKRatio
#' @description Plot PK Ratio plots for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @param settings settings for the task
#' @return list with `plots` and `tables`
#' @export
#' @import tlf
#' @import ospsuite
plotQualificationPKRatio <- function(configurationPlan,
                                     logFolder = getwd(),
                                     settings) {
  pkRatioResults <- list()
  for (pkRatioPlan in configurationPlan$plots$PKRatioPlots) {
    tableID <- paste(length(pkRatioResults) + 1, "pk-ratio-table", sep = "-")
    gmfeID <- paste(length(pkRatioResults) + 2, "pk-ratio-gmfe", sep = "-")

    pkRatioData <- getQualificationPKRatioData(pkRatioPlan, configurationPlan, logFolder)

    pkRatioTable <- getQualificationPKRatioTable(pkRatioData$data, pkRatioData$metaData, settings)
    pkRatioResults[[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = pkRatioPlan$SectionId,
      table = pkRatioTable,
      tableCaption = pkRatioPlan$Title,
      includeTable = isIncluded("Table", pkRatioPlan$Artifacts)
    )

    axesProperties <- getAxesProperties(pkRatioPlan$Axes) %||% settings$axes
    pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)
    pkRatioGMFE <- getQualificationPKRatioGMFE(pkParameterNames, pkRatioData$data, settings)
    pkRatioResults[[gmfeID]] <- saveTaskResults(
      id = gmfeID,
      sectionId = pkRatioPlan$SectionId,
      table = pkRatioGMFE,
      tableCaption = paste0("GMFE for ", pkRatioPlan$Title),
      includeTable = isIncluded("GMFE", pkRatioPlan$Artifacts)
    )
    for (pkParameterName in pkParameterNames) {
      plotID <- paste(length(pkRatioResults) + 1, "pk-ratio-plot", pkParameterName, sep = "-")
      measureID <- paste(length(pkRatioResults) + 2, "pk-ratio-measure", pkParameterName, sep = "-")

      pkRatioPlot <- getQualificationPKRatioPlot(pkParameterName, pkRatioData$data, pkRatioData$metaData, axesProperties)
      pkRatioMeasure <- getQualificationPKRatioMeasure(pkParameterName, pkRatioData$data, pkRatioData$metaData)

      pkRatioResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = pkRatioPlan$SectionId,
        plot = pkRatioPlot,
        plotCaption = pkRatioPlan$Title,
        includePlot = isIncluded("Plot", pkRatioPlan$Artifacts)
      )
      pkRatioResults[[measureID]] <- saveTaskResults(
        id = measureID,
        sectionId = pkRatioPlan$SectionId,
        table = pkRatioMeasure,
        tableCaption = paste0("Measure of ", pkRatioPlan$Title),
        includeTable = isIncluded("Measure", pkRatioPlan$Artifacts)
      )
    }
  }
  return(pkRatioResults)
}

#' @title getQualificationPKRatioGMFE
#' @description Get plot of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @param settings settings for the task
#' @return A data.frame
getQualificationPKRatioGMFE <- function(pkParameterNames, data, settings) {
  gmfe <- sapply(pkParameterNames, FUN = function(pkParameterName) {
    calculateGMFE(data[, paste0("obs", pkParameterName)], data[, paste0("pred", pkParameterName)])
  })
  gmfe <- formatNumerics(
    gmfe,
    digits = settings$digits,
    nsmall = settings$nsmall,
    scientific = settings$scientific
  )
  data.frame(
    Parameter = pkParameterNames,
    GMFE = gmfe
  )
}

#' @title getQualificationPKRatioMeasure
#' @description Get plot of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @return A data.frame
getQualificationPKRatioMeasure <- function(pkParameterName, data, metaData) {
  # Prepare data, dataMapping and plotCOnfiguration to follow tlf nomenclature
  data$Groups <- metaData$caption
  dataMapping <- tlf::PKRatioDataMapping$new(
    x = "age",
    y = paste0("ratio", pkParameterName),
    color = "Groups",
    shape = "Groups"
  )
  pkRatioMeasure <-  tlf::getPKRatioMeasure(data, dataMapping)
  # Export row names for report
  qualificationMeasure <- cbind(row.names(pkRatioMeasure), pkRatioMeasure)
  names(qualificationMeasure) <- c(" ", names(pkRatioMeasure))
  return(qualificationMeasure)
}

#' @title getQualificationPKRatioPlot
#' @description Get plot of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @return A ggplot object
getQualificationPKRatioPlot <- function(pkParameterName, data, metaData, axesProperties) {
  # Prepare data, dataMapping and plotCOnfiguration to follow tlf nomenclature
  data$Groups <- metaData$caption
  dataMapping <- tlf::PKRatioDataMapping$new(
    x = "age",
    y = paste0("ratio", pkParameterName),
    color = "Groups",
    shape = "Groups"
  )
  axesProperties$y$dimension <- metaData[[paste0("ratio", pkParameterName)]]$dimension
  plotConfiguration <- tlf::PKRatioPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  plotConfiguration$points$color <- metaData$color
  plotConfiguration$points$shape <- metaData$shape

  pkRatioPlot <- tlf::plotPKRatio(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration
  )
  pkRatioPlot <- updatePlotAxes(pkRatioPlot, axesProperties)

  return(pkRatioPlot)
}

#' @title getQualificationPKRatioTable
#' @description Get data of pk ratio from field `PKRatioPlots` of configuration plan
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @param settings settings for the task
#' @return A data.frame with correct numeric format
getQualificationPKRatioTable <- function(data, metaData, settings) {
  # Update the digits based on settings
  for (variableName in names(data)) {
    if (!is.numeric(data[, variableName])) {
      next
    }
    data[, variableName] <- formatNumerics(
      data[, variableName],
      digits = settings$digits,
      nsmall = settings$nsmall,
      scientific = settings$scientific
    )
  }

  # Update names of variables
  metaData <- metaData[sapply(metaData, function(metaDataVariable) {
    is.list(metaDataVariable)
  })]
  columnNames <- sapply(metaData, function(metaDataVariable) {
    tlf::getLabelWithUnit(metaDataVariable$dimension, metaDataVariable$unit)
  })
  names(data) <- columnNames
  return(data)
}

#' @title getQualificationPKRatioData
#' @description Get data of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkRatioPlan List providing the PK ratio mapping from field `PKRatioPlots` of configuration plan
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
getQualificationPKRatioData <- function(pkRatioPlan, configurationPlan, logFolder) {
  # Get PK parameters from new or deprecated method
  pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)

  pkRatioData <- data.frame()
  caption <- NULL
  for (group in pkRatioPlan$Groups) {
    for (pkRatioMapping in group$PKRatios) {
      pkRatioResults <- getPKRatioForMapping(pkRatioMapping, pkParameterNames, configurationPlan, logFolder)
      pkRatioData <- rbind.data.frame(
        pkRatioData,
        pkRatioResults$data
      )
    }
    caption <- c(caption, rep(group$Caption %||% NA, length(group$PKRatios)))
  }
  # Capture plot properties in metaData
  pkRatioMetaData <- c(
    pkRatioResults$metaData,
    list(
      caption = caption,
      color = sapply(pkRatioPlan$Groups, FUN = function(group) {
        group$Color %||% "black"
      }),
      shape = sapply(pkRatioPlan$Groups, FUN = function(group) {
        tlfShape(group$Symbol) %||% tlf::Shapes$circle
      })
    )
  )
  return(list(
    data = pkRatioData,
    metaData = pkRatioMetaData
  ))
}


#' @title getPKRatioForMapping
#' @description Get data of pk ratio from field `PKRatios` of configuration plan
#' @param pkRatioMapping List providing the PK ratio mapping from field `PKRatios` of configuration plan
#' @param pkParameterNames Names of PK Parameters as defined by users
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
getPKRatioForMapping <- function(pkRatioMapping, pkParameterNames, configurationPlan, logFolder) {
  # Load required inputs
  simulation <- ospsuite::loadSimulation(
    configurationPlan$getSimulationPath(project = pkRatioMapping$Project, simulation = pkRatioMapping$Simulation)
  )
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = configurationPlan$getPKAnalysisResultsPath(project = pkRatioMapping$Project, simulation = pkRatioMapping$Simulation),
    simulation = simulation
  )
  observedData <- readObservedDataFile(configurationPlan$getObservedDataPath(pkRatioMapping$ObservedData))
  selectedRow <- which(observedData[, reEnv$pkRatio$dictionary$id] %in% pkRatioMapping$ObservedDataRecordId)
  if (!isOfLength(selectedRow, 1)) {
    logWorkflow(
      message = paste0("In PK Ratio Plots, ", length(selectedRow), " data record(s) found for ObservedDataRecordId '", pkRatioMapping$ObservedDataRecordId, "'"),
      pathFolder = logFolder,
      logTypes = c(LogTypes$Error, LogTypes$Debug)
    )
    return()
  }

  metaData <- list()
  data <- data.frame()
  for (pkParameterName in pkParameterNames) {
    # TODO: we may need dictionaries for catching the appropriate PK parameter
    # in observed data and pk analysis (e.g. AUC_tEnd)
    pkParameter <- pkAnalyses$pKParameterFor(quantityPath = pkRatioMapping$Output, pkParameter = pkDictionaryQualificationOSP[[pkParameterName]])

    pkParameterObservedValue <- as.numeric(
      observedData[selectedRow, paste(pkParameterName, reEnv$pkRatio$dictionary$parameterColumn, sep = " ")]
    )

    pkParameterObservedUnit <- tolower(as.character(
      observedData[selectedRow, paste(pkParameterName, reEnv$pkRatio$dictionary$unitColumn, sep = " ")]
    ))
    pkParameterObservedValue <- ospsuite::toUnit(
      quantityOrDimension = pkParameter$dimension,
      values = pkParameterObservedValue,
      targetUnit = pkParameter$unit,
      sourceUnit = pkParameterObservedUnit,
      molWeight = simulation$molWeightFor(pkRatioMapping$Output)
    )
    # Values
    data[1, paste0("obs", pkParameterName)] <- pkParameterObservedValue
    data[1, paste0("pred", pkParameterName)] <- pkParameter$values
    data[1, paste0("ratio", pkParameterName)] <- pkParameter$values / pkParameterObservedValue
    # MetaData for tables and plot labels
    metaData[[paste0("obs", pkParameterName)]] <- list(
      dimension = paste(reEnv$pkRatio$dictionary$prefixObserved, pkParameterName, sep = " "),
      unit = pkParameter$unit
    )
    metaData[[paste0("pred", pkParameterName)]] <- list(
      dimension = paste(reEnv$pkRatio$dictionary$prefixSimulated, pkParameterName, sep = " "),
      unit = pkParameter$unit
    )
    metaData[[paste0("ratio", pkParameterName)]] <- list(
      dimension = paste(reEnv$pkRatio$dictionary$prefixRatio, pkParameterName, reEnv$pkRatio$dictionary$suffixRatio, sep = " "),
      unit = ""
    )
  }

  # Complete table with study, age and weight
  age <- ospsuite::getParameter(ospsuite::StandardPath$Age, simulation)
  weight <- ospsuite::getParameter(ospsuite::StandardPath$Weight, simulation)
  # Concatenate all the results
  data <- cbind.data.frame(
    study = observedData[selectedRow, reEnv$pkRatio$dictionary$study],
    age = age$value,
    weight = weight$value,
    data
  )
  metaData <- c(
    list(
      study = list(dimension = "Study ID", unit = ""),
      age = list(dimension = "Age", unit = age$displayUnit),
      weight = list(dimension = "Body Weight", unit = weight$displayUnit)
    ),
    metaData
  )

  return(list(
    data = data,
    metaData = metaData
  ))
}
