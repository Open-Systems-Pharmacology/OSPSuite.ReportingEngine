#' @title plotQualificationPKRatio
#' @description Plot PK Ratio plots for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param logFolder folder where the logs are saved
#' @param settings settings for the task
#' @return list with `plots` and `tables`
#' @import tlf
#' @import ospsuite
#' @import ospsuite.utils
#' @keywords internal
plotQualificationPKRatio <- function(configurationPlan,
                                     logFolder = getwd(),
                                     settings) {
  pkRatioResults <- list()
  for (pkRatioPlan in configurationPlan$plots$PKRatioPlots) {
    # If field artifacts is null, output them all
    pkRatioPlan$Artifacts <- pkRatioPlan$Artifacts %||% c("Table", "Plot", "Measure", "GMFE")
    pkRatioData <- getQualificationPKRatioData(pkRatioPlan, configurationPlan, logFolder, settings)
    axesProperties <- getAxesProperties(pkRatioPlan$Axes) %||% settings$axes
    pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)

    for (pkParameterName in pkParameterNames) {
      #----- Plot artifact -----#
      plotID <- paste(length(pkRatioResults) + 1, "pk-ratio-plot", pkParameterName, sep = "-")
      pkRatioPlot <- getQualificationPKRatioPlot(pkParameterName, pkRatioData$data, pkRatioData$metaData, axesProperties)
      pkRatioResults[[plotID]] <- saveTaskResults(
        id = plotID,
        sectionId = pkRatioPlan$SectionId,
        plot = pkRatioPlot,
        plotCaption = pkRatioPlan$Title,
        includePlot = isIncluded("Plot", pkRatioPlan$Artifacts)
      )
      #----- Measure artifact -----#
      measureID <- paste(length(pkRatioResults) + 2, "pk-ratio-measure", pkParameterName, sep = "-")
      pkRatioMeasure <- getQualificationPKRatioMeasure(pkParameterName, pkRatioData$data, pkRatioData$metaData)
      pkRatioResults[[measureID]] <- saveTaskResults(
        id = measureID,
        sectionId = pkRatioPlan$SectionId,
        table = pkRatioMeasure,
        tableCaption = paste0("Measure of ", pkRatioPlan$Title),
        includeTable = isIncluded("Measure", pkRatioPlan$Artifacts)
      )
    }
    #----- GMFE artifact -----#
    gmfeID <- paste(length(pkRatioResults) + 2, "pk-ratio-gmfe", sep = "-")
    pkRatioGMFE <- getQualificationPKRatioGMFE(pkParameterNames, pkRatioData$data)
    pkRatioResults[[gmfeID]] <- saveTaskResults(
      id = gmfeID,
      sectionId = pkRatioPlan$SectionId,
      table = pkRatioGMFE,
      tableCaption = paste0("GMFE for ", pkRatioPlan$Title),
      includeTable = isIncluded("GMFE", pkRatioPlan$Artifacts)
    )
    #----- Table artifact -----#
    tableID <- paste(length(pkRatioResults) + 1, "pk-ratio-table", sep = "-")
    pkRatioTable <- getQualificationPKRatioTable(pkRatioData$data, pkRatioData$metaData)
    pkRatioResults[[tableID]] <- saveTaskResults(
      id = tableID,
      sectionId = pkRatioPlan$SectionId,
      table = pkRatioTable,
      tableCaption = pkRatioPlan$Title,
      includeTable = isIncluded("Table", pkRatioPlan$Artifacts)
    )
  }
  return(pkRatioResults)
}

#' @title getQualificationPKRatioGMFE
#' @description Get Geometric Mean Fold Error for PK ratio plots
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @return A data.frame
#' @keywords internal
getQualificationPKRatioGMFE <- function(pkParameterNames, data) {
  gmfe <- sapply(pkParameterNames, FUN = function(pkParameterName) {
    calculateGMFE(data[, paste0("obs", pkParameterName)], data[, paste0("pred", pkParameterName)])
  })

  return(
    data.frame(
      Parameter = pkParameterNames,
      GMFE = gmfe
    )
  )
}

#' @title getQualificationPKRatioMeasure
#' @description Get qualification measure of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @return A data.frame
#' @keywords internal
getQualificationPKRatioMeasure <- function(pkParameterName, data, metaData) {
  # TODO: use tlf::getPKRatioMeasure once updated on tlf
  ratios <- data[, paste0("ratio", pkParameterName)]
  ratios <- ratios[!is.na(ratios)]

  qualificationMeasure <- data.frame(
    " " = c("Points total", "Points within 1.5 fold", "Points within 2 fold"),
    "Number" = c(
      length(ratios),
      measureValuesBetween(ratios, 1 / 1.5, 1.5, method = "count"),
      measureValuesBetween(ratios, 1 / 2, 2, method = "count")
    ),
    "Ratio [%]" = c(
      NA,
      measureValuesBetween(ratios, 1 / 1.5, 1.5, method = "percent"),
      measureValuesBetween(ratios, 1 / 2, 2, method = "percent")
    ),
    check.names = FALSE
  )
  return(qualificationMeasure)
}

#' @title getQualificationPKRatioPlot
#' @description Get plot of pk ratio from field `PKRatioPlots` of configuration plan
#' @param pkParameterName Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @return A ggplot object
#' @keywords internal
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
  plotConfiguration$xAxis$limits <- c(axesProperties$x$min, axesProperties$x$max) %||% autoAxesLimits(data[, "age"])
  plotConfiguration$yAxis$limits <- c(axesProperties$x$min, axesProperties$x$max) %||% autoAxesLimits(c(0.5, 2, data[, paste0("ratio", pkParameterName)]))

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
#' @return A data.frame with correct numeric format
#' @keywords internal
getQualificationPKRatioTable <- function(data, metaData) {
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
#' @param settings settings for the task
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
#' @keywords internal
getQualificationPKRatioData <- function(pkRatioPlan, configurationPlan, logFolder, settings) {
  # Get PK parameters from new or deprecated method
  pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)

  pkRatioData <- data.frame()
  caption <- NULL
  for (group in pkRatioPlan$Groups) {
    for (pkRatioMapping in group$PKRatios) {
      pkRatioResults <- getPKRatioForMapping(pkRatioMapping, pkParameterNames, configurationPlan, logFolder, settings)
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
#' @param settings settings for the task
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
#' @import ospsuite.utils
#' @keywords internal
getPKRatioForMapping <- function(pkRatioMapping, pkParameterNames, configurationPlan, logFolder, settings) {
  # Load required inputs
  simulation <- ospsuite::loadSimulation(
    configurationPlan$getSimulationPath(project = pkRatioMapping$Project, simulation = pkRatioMapping$Simulation),
    loadFromCache = TRUE
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
    pkParameter <- pkAnalyses$pKParameterFor(quantityPath = pkRatioMapping$Output, pkParameter = pkDictionaryQualificationOSP[[pkParameterName]])
    pkParameterObservedValue <- as.numeric(
      observedData[selectedRow, paste(pkParameterName, reEnv$pkRatio$dictionary$parameterColumn, sep = " ")]
    )

    pkParameterObservedUnit <- tolower(as.character(
      observedData[selectedRow, paste(pkParameterName, reEnv$pkRatio$dictionary$unitColumn, sep = " ")]
    ))
    pkParameterSimulatedValue <- ospsuite::toUnit(
      quantityOrDimension = pkParameter$dimension,
      values = pkParameter$values,
      targetUnit = settings$units[[pkParameterName]],
      molWeight = simulation$molWeightFor(pkRatioMapping$Output)
    )
    pkParameterObservedValue <- ospsuite::toUnit(
      quantityOrDimension = pkParameter$dimension,
      values = pkParameterObservedValue,
      targetUnit = settings$units[[pkParameterName]],
      sourceUnit = pkParameterObservedUnit,
      molWeight = simulation$molWeightFor(pkRatioMapping$Output)
    )
    # Values
    data[1, paste0("pred", pkParameterName)] <- pkParameterSimulatedValue
    data[1, paste0("obs", pkParameterName)] <- pkParameterObservedValue
    data[1, paste0("ratio", pkParameterName)] <- pkParameterSimulatedValue / pkParameterObservedValue
    # MetaData for tables and plot labels
    metaData[[paste0("pred", pkParameterName)]] <- list(
      dimension = paste(reEnv$pkRatio$dictionary$prefixSimulated, pkParameterName, sep = " "),
      unit = settings$units[[pkParameterName]]
    )
    metaData[[paste0("obs", pkParameterName)]] <- list(
      dimension = paste(reEnv$pkRatio$dictionary$prefixObserved, pkParameterName, sep = " "),
      unit = settings$units[[pkParameterName]]
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


#' @title isBetween
#' @description Assess if `x` is between `left` and `right` bounds.
#' Shortcut for `x >= left & x <= right` if `strict=FALSE` (default).
#' Shortcut for `x > left & x < right` if `strict=TRUE`.
#' @param x Numeric values to assess
#' @param left Numeric value(s) used as lower bound
#' @param right Numeric value(s) used as upper bound
#' @param strict Logical value defining if `x` is strictly between `left` and `right`.
#' Default value is `FALSE`.
#' @return Logical values
#' @export
#' @examples
#' isBetween(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[isBetween(x, -1, 1)]
#'
#' isBetween(x, cos(x) + 1, cos(x) - 1)
isBetween <- function(x, left, right, strict = FALSE) {
  if (strict) {
    return(x > left & x < right)
  }
  return(x >= left & x <= right)
}

#' @title measureValuesBetween
#' @description Measure the values of `x` between `left` and `right` bounds according to `method`.
#' @param x Numeric values to assess
#' @param left Numeric value(s) used as lower bound
#' @param right Numeric value(s) used as upper bound
#' @param method One of the following methods `"count"`, `"ratio"`, and `"percent"`.
#' @param strict Logical value defining if `x` is strictly between `left` and `right`.
#' Default value is `FALSE`.
#' @return Measure of `x` values between `left` and `right` bounds
#' @export
#' @import ospsuite.utils
#' @examples
#' measureValuesBetween(1:12, 7, 9)
#' measureValuesBetween(1:12, 7, 9, method = "percent")
#'
#' x <- rnorm(1e2)
#' measureValuesBetween(x, -1, 1)
#' measureValuesBetween(x, -1, 1, method = "ratio")
#'
#' measureValuesBetween(x, cos(x) + 1, cos(x) - 1)
measureValuesBetween <- function(x, left, right, method = "count", strict = FALSE) {
  # Remove NA values from counting
  if (isOfLength(left, 1)) {
    left <- rep(left, length(x))
  }
  if (isOfLength(right, 1)) {
    right <- rep(right, length(x))
  }
  naRows <- (is.na(x) | is.na(left) | is.na(right))
  measure <- switch(
    method,
    "count" = sum(isBetween(x[!naRows], left[!naRows], right[!naRows], strict)),
    "ratio" = sum(isBetween(x[!naRows], left[!naRows], right[!naRows], strict)) / length(x[!naRows]),
    "percent" = 100 * sum(isBetween(x[!naRows], left[!naRows], right[!naRows], strict)) / length(x[!naRows]),
  )
  return(measure)
}
