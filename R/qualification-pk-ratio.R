#' @title plotQualificationPKRatio
#' @description Plot PK Ratio plots for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param settings settings for the task
#' @return list with `plots` and `tables`
#' @import tlf
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
plotQualificationPKRatio <- function(configurationPlan, settings) {
  pkRatioResults <- list()
  for (pkRatioPlan in configurationPlan$plots$PKRatioPlots) {
    qualificationCatch(
      {
        # If field artifacts is null, output them all
        pkRatioPlan$Artifacts <- pkRatioPlan$Artifacts %||% c("Table", "Plot", "Measure", "GMFE")
        pkRatioData <- getQualificationPKRatioData(pkRatioPlan, configurationPlan, settings)
        axesProperties <- getAxesProperties(pkRatioPlan$Axes) %||% settings$axes
        pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)

        for (pkParameterName in pkParameterNames) {
          #----- Plot artifact -----#
          plotID <- defaultFileNames$resultID(length(pkRatioResults) + 1, "pk_ratio_plot", pkParameterName)
          pkRatioPlot <- getQualificationPKRatioPlot(pkParameterName, pkRatioData$data, pkRatioData$metaData, axesProperties, pkRatioPlan[["PlotSettings"]])
          pkRatioResults[[plotID]] <- saveTaskResults(
            id = plotID,
            sectionId = pkRatioPlan$SectionReference %||% pkRatioPlan$SectionId,
            plot = pkRatioPlot,
            plotCaption = pkRatioPlan$Title,
            includePlot = isIncluded("Plot", pkRatioPlan$Artifacts)
          )
          #----- Measure artifact -----#
          measureID <- defaultFileNames$resultID(length(pkRatioResults) + 1, "pk_ratio_measure", pkParameterName)
          pkRatioMeasure <- getQualificationPKRatioMeasure(pkParameterName, pkRatioData$data, pkRatioData$metaData)
          pkRatioResults[[measureID]] <- saveTaskResults(
            id = measureID,
            sectionId = pkRatioPlan$SectionReference %||% pkRatioPlan$SectionId,
            table = pkRatioMeasure,
            tableCaption = paste0("Measure of ", pkRatioPlan$Title),
            includeTable = isIncluded("Measure", pkRatioPlan$Artifacts)
          )
        }
        #----- GMFE artifact -----#
        gmfeID <- defaultFileNames$resultID(length(pkRatioResults) + 1, "pk_ratio_gmfe")
        pkRatioGMFE <- getQualificationPKRatioGMFE(pkParameterNames, pkRatioData$data)
        pkRatioResults[[gmfeID]] <- saveTaskResults(
          id = gmfeID,
          sectionId = pkRatioPlan$SectionReference %||% pkRatioPlan$SectionId,
          table = pkRatioGMFE,
          tableCaption = paste0("GMFE for ", pkRatioPlan$Title),
          includeTable = isIncluded("GMFE", pkRatioPlan$Artifacts)
        )
        #----- Table artifact -----#
        tableID <- defaultFileNames$resultID(length(pkRatioResults) + 1, "pk_ratio_table", pkParameterName)
        pkRatioTable <- getQualificationPKRatioTable(pkRatioData$data, pkRatioData$metaData)
        pkRatioResults[[tableID]] <- saveTaskResults(
          id = tableID,
          sectionId = pkRatioPlan$SectionReference %||% pkRatioPlan$SectionId,
          table = pkRatioTable,
          tableCaption = pkRatioPlan$Title,
          includeTable = isIncluded("Table", pkRatioPlan$Artifacts)
        )
      },
      configurationPlanField = pkRatioPlan
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
#' @param plotProperties list of plot properties defined in field `Plot` of PKRatio configuration plan
#' @return A ggplot object
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getQualificationPKRatioPlot <- function(pkParameterName, data, metaData, axesProperties, plotProperties) {
  # Prepare data, dataMapping and plotCOnfiguration to follow tlf nomenclature
  data$Groups <- metaData$caption
  dataMapping <- tlf::PKRatioDataMapping$new(
    x = "age",
    y = paste0("ratio", pkParameterName),
    color = "Groups",
    shape = "Groups"
  )
  axesProperties$y$dimension <- metaData[[paste0("ratio", pkParameterName)]]$dimension

  plotConfiguration <- getPlotConfigurationFromPlan(plotProperties, plotType = "PKRatio")
  plotConfiguration$points$color <- metaData$color
  plotConfiguration$points$shape <- metaData$shape
  plotConfiguration$xAxis$limits <- c(axesProperties$x$min, axesProperties$x$max) %||% autoAxesLimits(data[, "age"])
  plotConfiguration$yAxis$limits <- c(axesProperties$y$min, axesProperties$y$max) %||% autoAxesLimits(c(0.5, 2, data[, paste0("ratio", pkParameterName)]))

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
#' @param settings settings for the task
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getQualificationPKRatioData <- function(pkRatioPlan, configurationPlan, settings) {
  # Get PK parameters from new or deprecated method
  pkParameterNames <- pkRatioPlan$PKParameters %||% ospsuite::toPathArray(pkRatioPlan$PKParameter)

  pkRatioData <- data.frame()
  caption <- NULL
  for (group in pkRatioPlan$Groups) {
    for (pkRatioMapping in group$PKRatios) {
      pkRatioResults <- getPKRatioForMapping(pkRatioMapping, pkParameterNames, configurationPlan, settings)
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
        group$Color %||% reEnv$theme$plotConfigurations$plotPKRatio$points$color
      }),
      shape = sapply(pkRatioPlan$Groups, FUN = function(group) {
        tlfShape(group$Symbol %||% reEnv$theme$plotConfigurations$plotPKRatio$points$shape)
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
#' @param settings settings for the task
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
#' @keywords internal
getPKRatioForMapping <- function(pkRatioMapping, pkParameterNames, configurationPlan, settings) {
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
  # Warn if record ID not found and go to next PK Ratio Mapping
  if (!checkPKRatioObservedRecord(selectedRow, pkRatioMapping$ObservedDataRecordId)) {
    return()
  }

  metaData <- list()
  data <- data.frame()
  for (pkParameterName in pkParameterNames) {
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

    # Get PK Parameter observed and simulated values
    # Warn if observed data is not found and display NA in case there is a simulated value
    parameterColumn <- paste(pkParameterName, reEnv$pkRatio$dictionary$parameterColumn, sep = " ")
    checkPKRatioObservedVariable(parameterColumn, observedData)
    pkParameterObservedValue <- as.numeric(observedData[selectedRow, parameterColumn] %||% NA)

    pkParameter <- pkAnalyses$pKParameterFor(
      quantityPath = pkRatioMapping$Output,
      pkParameter = pkDictionaryQualificationOSP[[pkParameterName]]
    )

    # Warn if PK parameter simulated value is not found,
    # Still display observed data
    if (!checkPKParameterExists(pkParameter, pkParameterName, pkRatioMapping)) {
      data[1, paste0("pred", pkParameterName)] <- NA
      data[1, paste0("obs", pkParameterName)] <- pkParameterObservedValue
      data[1, paste0("ratio", pkParameterName)] <- NA
      next
    }

    # Convert simulated to display unit
    pkParameterSimulatedValue <- ospsuite::toUnit(
      quantityOrDimension = pkParameter$dimension,
      values = pkParameter$values,
      targetUnit = settings$units[[pkParameterName]],
      molWeight = simulation$molWeightFor(pkRatioMapping$Output)
    )

    # Warn if unit is not found and assumes unit is display unit
    unitColumn <- paste(pkParameterName, reEnv$pkRatio$dictionary$unitColumn, sep = " ")
    if (checkPKRatioObservedVariable(unitColumn, observedData)) {
      pkParameterObservedUnit <- observedData[selectedRow, unitColumn]
      pkParameterObservedValue <- ospsuite::toUnit(
        quantityOrDimension = pkParameter$dimension,
        values = pkParameterObservedValue,
        targetUnit = settings$units[[pkParameterName]],
        sourceUnit = pkParameterObservedUnit,
        molWeight = simulation$molWeightFor(pkRatioMapping$Output)
      )
    }

    # Values and ratio
    data[1, paste0("pred", pkParameterName)] <- pkParameterSimulatedValue
    data[1, paste0("obs", pkParameterName)] <- pkParameterObservedValue
    data[1, paste0("ratio", pkParameterName)] <- pkParameterSimulatedValue / pkParameterObservedValue
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


#' @title measureValuesBetween
#' @description Measure the values of `x` between `left` and `right` bounds according to `method`.
#' @param x Numeric values to assess
#' @param left Numeric value(s) used as lower bound
#' @param right Numeric value(s) used as upper bound
#' @param method One of the following methods `"count"`, `"ratio"`, and `"percent"`.
#' @param strict Logical value defining if `x` is strictly between `left` and `right`.
#' Default value is `FALSE`.
#' @return Measure of `x` values between `left` and `right` bounds
#' @import tlf
#' @export
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
  measure <- switch(method,
    "count" = sum(tlf::isBetween(x[!naRows], left[!naRows], right[!naRows], strict)),
    "ratio" = sum(tlf::isBetween(x[!naRows], left[!naRows], right[!naRows], strict)) / length(x[!naRows]),
    "percent" = 100 * sum(tlf::isBetween(x[!naRows], left[!naRows], right[!naRows], strict)) / length(x[!naRows]),
  )
  return(measure)
}
