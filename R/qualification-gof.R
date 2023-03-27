#' @title plotQualificationGOFs
#' @description Plot observation vs prediction for qualification workflow
#' @param configurationPlan A `ConfigurationPlan` object
#' @param settings settings for the task
#' @return list of qualification GOF ggplot objects
#' @importFrom ospsuite.utils %||%
#' @keywords internal
plotQualificationGOFs <- function(configurationPlan, settings) {
  gofResults <- list()
  for (gofPlan in configurationPlan$plots$GOFMergedPlots) {
    qualificationCatch(
      {
        # If field artifacts is null, output them all
        gofPlan$Artifacts <- gofPlan$Artifacts %||% c("Plot", "GMFE")
        gofAxesUnits <- getGOFAxesUnits(gofPlan, settings)
        gofData <- getQualificationGOFData(gofPlan, configurationPlan, gofAxesUnits)

        # GMFE
        gmfeID <- defaultFileNames$resultID(length(gofResults) + 1, "gof_gmfe")
        gofGMFE <- getQualificationGOFGMFE(gofData$data)
        gofResults[[gmfeID]] <- saveTaskResults(
          id = gmfeID,
          sectionId = gofPlan$SectionReference %||% gofPlan$SectionId,
          table = gofGMFE,
          tableCaption = paste0("GMFE for ", gofPlan$Title),
          includeTable = isIncluded("GMFE", gofPlan$Artifacts)
        )

        # GOF plots
        plotTypes <- gofPlan$PlotTypes %||% ospsuite::toPathArray(gofPlan$PlotType)
        for (plotType in plotTypes) {
          plotID <- defaultFileNames$resultID(length(gofResults) + 1, "gof_plot", plotType)
          axesProperties <- getAxesProperties(gofPlan$Axes[[plotType]]) %||% settings[[plotType]]$axes

          gofPlot <- getQualificationGOFPlot(plotType, gofData$data, gofData$metaData, axesProperties, gofPlan[["PlotSettings"]])
          gofResults[[plotID]] <- saveTaskResults(
            id = plotID,
            sectionId = gofPlan$SectionReference %||% gofPlan$SectionId,
            plot = gofPlot,
            plotCaption = gofPlan$Title,
            includePlot = isIncluded("Plot", gofPlan$Artifacts)
          )
        }
      },
      configurationPlanField = gofPlan
    )
  }
  return(gofResults)
}

#' @title getQualificationGOFData
#' @description Get data of goodness of fit from field `GOFMergedPlots` of configuration plan
#' @param gofPlan List providing the mapping of observed and simulated data
#' @param configurationPlan A `ConfigurationPlan` object
#' @param axesUnits list of axes properties obtained from `getGOFAxesUnits`
#' @return list with `data` and `metaData`
#' @import tlf
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getQualificationGOFData <- function(gofPlan, configurationPlan, axesUnits) {
  gofData <- data.frame()
  groupCaptions <- NULL
  groupShapes <- NULL
  groupColors <- NULL
  # groupColorsIndex aims at providing a unique number for the each new legend entry
  # and ensuring the right color is associated to the right legend entry
  groupColorsIndex <- 1
  for (group in gofPlan$Groups) {
    for (outputMapping in group$OutputMappings) {
      gofResults <- getGOFDataForMapping(outputMapping, configurationPlan, axesUnits)
      # Variable associated to shapes
      gofResults$Groups <- group$Caption %||% NA
      # Variable associated to colors
      gofResults$Legend <- groupColorsIndex
      groupColorsIndex <- groupColorsIndex + 1
      gofData <- rbind.data.frame(gofData, gofResults)
      # Use default "black" color to prevent crash if undefined
      groupColors <- c(groupColors, outputMapping$Color %||% group$Color %||% "black")
    }
    # Use default "circle" shape to prevent crash if undefined
    groupShapes <- c(groupShapes, tlfShape(group$Symbol) %||% tlf::Shapes$circle)
    groupCaptions <- c(groupCaptions, group$Caption %||% "")
  }
  # Capture plot properties in metaData
  gofMetaData <- list(
    caption = groupCaptions,
    color = groupColors,
    shape = groupShapes
  )
  return(list(
    data = gofData,
    metaData = gofMetaData
  ))
}

#' @title getGOFDataForMapping
#' @description Get data of goodness of fit from field `GOFMergedPlots` of configuration plan
#' @param outputMapping list of mapping elements from `OutputMappings` field in configuration plan
#' @param configurationPlan A `ConfigurationPlan` object
#' @param axesUnits list of axes properties obtained from `getGOFAxesUnits`
#' @return A data.frame as obtained by `getResiduals` whose values are in base unit
#' @import ospsuite
#' @keywords internal
getGOFDataForMapping <- function(outputMapping, configurationPlan, axesUnits) {
  # Get simulation output
  simulationFile <- configurationPlan$getSimulationPath(
    project = outputMapping$Project,
    simulation = outputMapping$Simulation
  )
  simulationResultsFile <- configurationPlan$getSimulationResultsPath(
    project = outputMapping$Project,
    simulation = outputMapping$Simulation
  )
  simulation <- ospsuite::loadSimulation(simulationFile, loadFromCache = TRUE)
  simulationResults <- ospsuite::importResultsFromCSV(simulation, simulationResultsFile)
  # Get and convert output path values into display unit
  simulationQuantity <- ospsuite::getQuantity(outputMapping$Output, simulation)
  simulationPathResults <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = simulationQuantity)
  molWeight <- simulation$molWeightFor(outputMapping$Output)
  simulatedTime <- ospsuite::toUnit(
    "Time",
    simulationPathResults$data[, "Time"],
    axesUnits$time
  )
  simulatedValues <- ospsuite::toUnit(
    simulationQuantity,
    simulationPathResults$data[, outputMapping$Output],
    axesUnits$simulated,
    molWeight = molWeight
  )

  # Loop on each observed dataset in OutputMappings
  gofData <- data.frame()
  for (observedDataSet in outputMapping$ObservedData) {
    observedResults <- getObservedDataFromConfigurationPlan(observedDataSet, configurationPlan)
    observedTime <- ospsuite::toUnit(
      quantityOrDimension = "Time",
      values = as.numeric(observedResults$data[, 1]),
      targetUnit = axesUnits$time,
      sourceUnit = observedResults$metaData$time$unit
    )
    observedValues <- ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(observedResults$metaData$output$unit),
      values = observedResults$data[, 2],
      targetUnit = axesUnits$observed,
      sourceUnit = tolower(observedResults$metaData$output$unit),
      molWeight = molWeight
    )

    # Re-use nomenclature and functions from utilities-goodness-of-fit
    observedData <- data.frame(
      Time = observedTime,
      Concentration = observedValues,
      Path = observedDataSet
    )
    simulatedData <- data.frame(
      Time = simulatedTime,
      Concentration = simulatedValues,
      Legend = outputMapping$Simulation,
      ResidualsLegend = outputMapping$Simulation
    )
    # Currently residuals are only calculated assuming Logarithmic formula
    gofData <- rbind.data.frame(
      gofData,
      getResiduals(observedData, simulatedData)
    )
  }
  return(gofData)
}


#' @title getQualificationGOFPlot
#' @description Get goodness of fit plot
#' @param plotType Name of PK Parameter as defined by users
#' @param data data.frame with PK Ratios
#' @param metaData metaData with units and dimension for labeling the table header
#' @param axesProperties list of axes properties obtained from `getAxesProperties`
#' @param plotProperties list of plot properties defined in field `Plot` of GOFMerged configuration plan
#' @return A ggplot object
#' @import tlf
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getQualificationGOFPlot <- function(plotType, data, metaData, axesProperties, plotProperties) {
  # Axes labels
  axesProperties$y$dimension <- switch(plotType,
    "predictedVsObserved" = paste0("Simulated ", displayDimension(axesProperties$y$dimension)),
    "residualsOverTime" = "Residuals\nlog(Observed)-log(Simulated)"
  )
  axesProperties$x$dimension <- switch(plotType,
    "predictedVsObserved" = paste0("Observed ", displayDimension(axesProperties$x$dimension)),
    "residualsOverTime" = displayDimension(axesProperties$x$dimension)
  )
  dataMapping <- switch(plotType,
    "predictedVsObserved" = tlf::ObsVsPredDataMapping$new(
      x = "Observed",
      y = "Simulated",
      color = "Legend",
      shape = "Groups"
    ),
    "residualsOverTime" = tlf::ResVsPredDataMapping$new(
      x = "Time",
      y = "Residuals",
      color = "Legend",
      shape = "Groups"
    )
  )
  plotConfiguration <- getPlotConfigurationFromPlan(
    plotProperties,
    plotType = switch(plotType,
      "predictedVsObserved" = "ObsVsPred",
      "residualsOverTime" = "ResVsPred"
    )
  )
  # Use time tick algorithm for residualsOverTime plots
  if (isIncluded(plotType, "residualsOverTime")) {
    axesProperties$x <- c(
      axesProperties$x,
      getTimeTicksFromUnit(axesProperties$x$unit, data$Time)
    )
  }

  # Update shapes and colors from config plan
  plotConfiguration$points$color <- metaData$color
  # Ensure order of shapes matches order of caption
  # so that the correct shape is associated to its caption
  plotConfiguration$points$shape <- metaData$shape[order(metaData$caption)]

  positiveRows <- (data[, "Observed"] > 0) & (data[, "Simulated"] > 0)
  dataForLimit <- c(data[positiveRows, "Observed"], data[positiveRows, "Simulated"])

  plotConfiguration$xAxis$limits <- c(axesProperties$x$min, axesProperties$x$max) %||%
    autoAxesLimits(switch(plotType,
      "predictedVsObserved" = dataForLimit,
      "residualsOverTime" = data[positiveRows, "Time"]
    ))
  plotConfiguration$yAxis$limits <- c(axesProperties$x$min, axesProperties$x$max) %||%
    autoAxesLimits(switch(plotType,
      "predictedVsObserved" = dataForLimit,
      "residualsOverTime" = c(0, data[positiveRows, "Residuals"])
    ))

  gofPlot <- switch(plotType,
    "predictedVsObserved" = tlf::plotObsVsPred(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping,
      plotConfiguration = plotConfiguration,
      # Add identity line to log-log plot
      foldDistance = 0
    ),
    "residualsOverTime" = tlf::plotResVsPred(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping,
      plotConfiguration = plotConfiguration
    )
  )
  gofPlot <- updatePlotAxes(gofPlot, axesProperties)
  # Remove legend from colors
  gofPlot <- gofPlot + ggplot2::guides(color = "none")
  return(gofPlot)
}

#' @title getQualificationGOFGMFE
#' @description Get Geometric Mean Fold Error for GOF plots
#' @param data data.frame with residuals values
#' @return A data.frame
#' @keywords internal
getQualificationGOFGMFE <- function(data) {
  gmfe <- data.frame()
  # Ensure class of Groups
  groupNames <- levels(as.factor(data$Groups))
  for (groupName in groupNames) {
    selectedRows <- data$Groups %in% groupName
    gmfe <- rbind.data.frame(
      gmfe,
      data.frame(
        Group = groupName,
        GMFE = calculateGMFE(data[selectedRows, "Observed"], data[selectedRows, "Simulated"])
      )
    )
  }
  if (!isOfLength(groupNames, 1)) {
    gmfe <- rbind.data.frame(
      gmfe,
      data.frame(
        Group = "All",
        GMFE = calculateGMFE(data[, "Observed"], data[, "Simulated"])
      )
    )
  }
  return(gmfe)
}

#' @title getGOFAxesUnits
#' @description Get list of units from axes properties and settings
#' @param gofPlan List providing the configuration of the goodness of fit results
#' @param settings settings for the task
#' @return A list of units for goodness of fit results
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getGOFAxesUnits <- function(gofPlan, settings) {
  predictedVsObservedAxesProperties <- getAxesProperties(gofPlan$Axes$PredictedVsObserved) %||% settings$predictedVsObserved$axes
  residualsOverTimeAxesProperties <- getAxesProperties(gofPlan$Axes$ResidualsOverTime) %||% settings$residualsOverTime$axes
  return(
    list(
      time = residualsOverTimeAxesProperties$x$unit,
      observed = predictedVsObservedAxesProperties$x$unit,
      simulated = predictedVsObservedAxesProperties$y$unit,
      residuals = residualsOverTimeAxesProperties$y$unit
    )
  )
}
