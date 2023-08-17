#' @title plotMeanMassBalance
#' @description Plot mass balance diagnostics time profiles, cumulative time profiles,
#' normalized time profiles and cumulative normalized time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings Options to be defined
#' (e.g. how to group the plot results, or which molecule to exclude)
#' @return list of `ggplot` objects
#' @import tlf
#' @import ospsuite
#' @import utils
#' @import ospsuite.utils
#' @keywords internal
plotMeanMassBalance <- function(structureSet, settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)
  outputPaths <- getOutputPathsInSimulationSet(structureSet$simulationSet)
  outputArray <- unique(ospsuite::toPathArray(outputPaths))
  
  # Get all the relevant compounds for mass balance
  # as intersect between all compounds and output paths
  allAppliedCompoundNames <- simulation$allXenobioticFloatingMoleculeNames()
  compoundsInOutputPaths <- sapply(
    allAppliedCompoundNames, 
    function(compoundName){isIncluded(compoundName, outputArray)}
    )
  relevantAppliedCompoundNames <- allAppliedCompoundNames[compoundsInOutputPaths]
  # Get drug mass for each application to perform the drugmass normalized plot
  applications <- NULL
  for(compoundName in relevantAppliedCompoundNames){
    applications <- c(applications, simulation$allApplicationsFor(compoundName))
  }
  # Create data.frame of cumulative applied drug mass from applications
  # which accounts for infusion time if not bolus nor oral
  applicationResults <- getApplicationResults(applications)
  
  # Get all the molecule paths (with dimension=amount) for the compounds and their metabolites
  allCompoundNames <- setdiff(
    c(simulation$allFloatingMoleculeNames(), simulation$allStationaryMoleculeNames()),
    simulation$allEndogenousStationaryMoleculeNames()
  )
  organism <- ospsuite::getContainer("Organism", simulation)
  molecules <- ospsuite::getAllMoleculesMatching(paste0("**|", allCompoundNames), organism)
  # Clear concentration output in case any concentrations are still included
  ospsuite::clearOutputs(simulation)
  for (molecule in molecules) {
    addOutputs(quantitiesOrPaths = molecule, simulation = simulation)
  }

  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutput <- ospsuite::getOutputValues(
    simulationResults = simulationResults,
    quantitiesOrPaths = simulationResults$allQuantityPaths
  )

  # Create a data.frame with full string path and separates the last 3 elements of paths
  # to perform filtering and grouping of for the final plot
  pathsArray <- NULL
  for (path in simulationResults$allQuantityPaths) {
    endOfPath <- utils::tail(ospsuite::toPathArray(path), 3)
    pathsArray <- rbind.data.frame(
      pathsArray,
      data.frame(
        "parentCompartmentName" = endOfPath[1],
        "subCompartmentName" = endOfPath[2],
        "compoundName" = endOfPath[3],
        "path" = path,
        stringsAsFactors = FALSE
      )
    )
  }

  # In the Matlab version, there are multiple groupings performed at that point:
  # 1) Saliva is excluded from the mass balance calculation
  # 2) Lumen is a parent compartment that regroups many sub-compartments but Feces
  # 3) All the rest of the sub-compartments are used normally

  # 1) Exclude Saliva
  salivaExclusionCondition <- !pathsArray[, "parentCompartmentName"] %in% "Saliva"
  pathsArray <- pathsArray[salivaExclusionCondition, ]

  # 2) Regroup the lumen sub-compartment but Feces
  lumenGroupingCondition <- pathsArray[, "parentCompartmentName"] %in% "Lumen" & !pathsArray[, "subCompartmentName"] %in% "Feces"
  pathsArray[lumenGroupingCondition, "subCompartmentName"] <- "Lumen"

  # Factors could not be used for steps 1 and 2 but are needed in the next step
  pathsArray <- as.data.frame(lapply(pathsArray, function(pathsArrayColumn) {
    as.factor(pathsArrayColumn)
  }))

  # Aggregate Data by compound and compartment
  simulationResultsOutputByGroup <- NULL
  for (compoundLevel in levels(pathsArray[, "compoundName"])) {
    for (compartmentLevel in levels(pathsArray[, "subCompartmentName"])) {
      selectedPaths <- pathsArray[, "compoundName"] %in% compoundLevel & pathsArray[, "subCompartmentName"] %in% compartmentLevel
      # as.character() ensures that levels filtered out won't still be added to the aggregation
      selectedPathNames <- as.character(pathsArray[selectedPaths, "path"])

      # cbind(..., 0) prevents rowSums to crash if only one column is filtered
      aggregatedData <- rowSums(cbind.data.frame(
        simulationResultsOutput$data[, selectedPathNames, drop = FALSE],
        data.frame(dummyVariable = 0)
      ))

      if (max(aggregatedData) > 0) {
        # Expand drugMass in the time grid of the simulated data using function approx
        # Output of approx is a data.frame with x and y variables
        drugMass <- approx(
          x = applicationResults$time,
          y = applicationResults$drugMass,
          xout = simulationResultsOutput$data[, "Time"],
          method = "constant",
          rule = 2
        )$y

        simulationResultsOutputByGroup <- rbind.data.frame(
          simulationResultsOutputByGroup,
          cbind.data.frame(
            "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
            "Amount" = aggregatedData,
            "NormalizedAmount" = aggregatedData / drugMass,
            "Legend" = paste0(compoundLevel, " - ", compartmentLevel)
          )
        )
      }
    }
  }

  metaDataOutputByGroup <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Amount" = list(
      dimension = molecule$dimension,
      unit = molecule$unit
    ),
    "NormalizedAmount" = list(
      dimension = molecule$dimension,
      unit = "fraction of drugmass"
    )
  )

  # Get mass balance results
  massBalanceResults <- list()
  timeTicks <- getTimeTicksFromUnit(
    structureSet$simulationSet$timeUnit,
    timeValues = simulationResultsOutputByGroup$Time
  )
  # Time profile
  timeProfileID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  timeProfilePlot <- tlf::plotSimulatedTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = getMassBalanceDataMapping("timeProfile"),
    plotConfiguration = getMassBalancePlotConfiguration(
      plotType = "timeProfile",
      data = simulationResultsOutputByGroup,
      metaData = metaDataOutputByGroup,
      settings = settings
    )
  )
  massBalanceResults[[timeProfileID]] <- saveTaskResults(
    id = timeProfileID,
    plot = timeProfilePlot,
    plotCaption = captions$massBalance$timeProfile()
  )

  # Cumulative time profile
  cumulativeTimeProfileID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  cumulativeTimeProfilePlot <- tlf::plotCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = getMassBalanceDataMapping("cumulativeTimeProfile"),
    plotConfiguration = getMassBalancePlotConfiguration(
      plotType = "cumulativeTimeProfile",
      data = simulationResultsOutputByGroup,
      metaData = metaDataOutputByGroup,
      settings = settings
    )
  )

  massBalanceResults[[cumulativeTimeProfileID]] <- saveTaskResults(
    id = cumulativeTimeProfileID,
    plot = cumulativeTimeProfilePlot,
    plotCaption = captions$massBalance$cumulativeTimeProfile()
  )

  # Normalized time profile
  normalizedTimeProfileID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  normalizedTimeProfilePlot <- tlf::plotSimulatedTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = getMassBalanceDataMapping("normalizedTimeProfile"),
    plotConfiguration = getMassBalancePlotConfiguration(
      plotType = "normalizedTimeProfile",
      data = simulationResultsOutputByGroup,
      metaData = metaDataOutputByGroup,
      settings = settings
    )
  )
  massBalanceResults[[normalizedTimeProfileID]] <- saveTaskResults(
    id = normalizedTimeProfileID,
    plot = normalizedTimeProfilePlot,
    plotCaption = captions$massBalance$normalizedTimeProfile()
  )

  # Normalized cumulative time profile
  normalizedCumulativeTimeProfileID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  normalizedCumulativeTimeProfilePlot <- tlf::plotCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = getMassBalanceDataMapping("normalizedCumulativeTimeProfile"),
    plotConfiguration = getMassBalancePlotConfiguration(
      plotType = "normalizedCumulativeTimeProfile",
      data = simulationResultsOutputByGroup,
      metaData = metaDataOutputByGroup,
      settings = settings
    )
  )
  massBalanceResults[[normalizedCumulativeTimeProfileID]] <- saveTaskResults(
    id = normalizedCumulativeTimeProfileID,
    plot = normalizedCumulativeTimeProfilePlot,
    plotCaption = captions$massBalance$normalizedCumulativeTimeProfile()
  )

  # Pie Chart
  pieChartID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  # Select latest time point
  selectedRows <- which(simulationResultsOutputByGroup$Time == max(simulationResultsOutputByGroup$Time))
  pieChartData <- simulationResultsOutputByGroup[selectedRows, ]
  # Update legend to include normalized amount as percent
  pieChartData$LegendWithPercent <- paste(
    pieChartData$Legend,
    " (", round(100 * pieChartData$NormalizedAmount, digits = 1), "%)",
    sep = ""
  )
  # Ensure that the colors and legend match previous mass balance plots by re-ordering Legend
  pieChartData$LegendWithPercent <- reorder(
    pieChartData$LegendWithPercent,
    as.numeric(factor(pieChartData$Legend))
  )
  pieChartPlot <- tlf::plotPieChart(
    data = pieChartData,
    metaData = metaDataOutputByGroup,
    dataMapping = getMassBalanceDataMapping("pieChart"),
    plotConfiguration = settings$plotConfigurations[["pieChart"]]
  )

  # Get time caption text for report
  timeCaption <- formatNumerics(
    max(simulationResultsOutputByGroup$Time),
    digits = settings$digits,
    scientific = settings$scientific
  )
  massBalanceResults[[pieChartID]] <- saveTaskResults(
    id = pieChartID,
    plot = pieChartPlot,
    plotCaption = captions$massBalance$pieChart(timeCaption, metaDataOutputByGroup$Time$unit)
  )

  # Table of mass balance time profiles
  # saved but not included into report
  tableID <- defaultFileNames$resultID(
    length(massBalanceResults) + 1,
    "mass_balance",
    structureSet$simulationSet$simulationSetName
  )
  massBalanceResults[[tableID]] <- saveTaskResults(
    id = tableID,
    table = simulationResultsOutputByGroup,
    includeTable = FALSE
  )
  return(massBalanceResults)
}


#' @title getMassBalancePlotConfiguration
#' @description Get mass balance time profile plot configuration
#' @param plotType One of the 5 plot types displayed by mass balance task
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param settings User-defined options
#' @return A `tlf` `PlotConfiguration` object
#' @import tlf
#' @keywords internal
getMassBalancePlotConfiguration <- function(plotType, data, metaData, settings) {
  dataMapping <- getMassBalanceDataMapping(plotType)
  plotConfiguration <- settings$plotConfigurations[[plotType]] %||%
    switch(plotType,
      "timeProfile" = tlf::TimeProfilePlotConfiguration$new(
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      ),
      "normalizedTimeProfile" = tlf::TimeProfilePlotConfiguration$new(
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      ),
      "cumulativeTimeProfile" = tlf::CumulativeTimeProfilePlotConfiguration$new(
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      ),
      "normalizedCumulativeTimeProfile" = tlf::CumulativeTimeProfilePlotConfiguration$new(
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      )
    )
  # Update time ticks
  plotConfiguration <- updatePlotConfigurationTimeTicks(data, metaData, dataMapping, plotConfiguration)
  return(plotConfiguration)
}

#' @title getMassBalanceDataMapping
#' @description Get mass balance data mapping
#' @param plotType One of the 5 plot types displayed by mass balance task
#' @return A `tlf` `DataMapping` object
#' @import tlf
#' @keywords internal
getMassBalanceDataMapping <- function(plotType) {
  dataMapping <- switch(plotType,
    "timeProfile" = tlf::TimeProfileDataMapping$new(
      x = "Time",
      y = "Amount",
      color = "Legend"
    ),
    "normalizedTimeProfile" = tlf::TimeProfileDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      color = "Legend"
    ),
    "cumulativeTimeProfile" = tlf::CumulativeTimeProfileDataMapping$new(
      x = "Time",
      y = "Amount",
      fill = "Legend"
    ),
    "normalizedCumulativeTimeProfile" = tlf::CumulativeTimeProfileDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      fill = "Legend"
    ),
    "pieChart" = tlf::PieChartDataMapping$new(
      x = "NormalizedAmount",
      fill = "LegendWithPercent"
    )
  )
  return(dataMapping)
}


#' @title getApplicationResults
#' @description Get a data.frame of application results corresponding to
#' cumulative drug mass as a function of time.
#' Accounts for infusion cases in which infusion time is not `NULL` by including intermediate time steps
#' @param applications list of `Application` objects queried by the method `simulation$allApplicationsFor()`
#' @return A data.frame that includes time and drug mass as variables
#' @import ospsuite
#' @import ospsuite.utils
#' @keywords internal
getApplicationResults <- function(applications, infusionSteps = 20) {
  applicationResults <- data.frame()
  for (application in applications) {
    # No Infusion
    if (is.null(application$infusionTime$value)) {
      applicationResults <- rbind.data.frame(
        applicationResults,
        data.frame(
          time = application$startTime$value,
          drugMass = application$drugMass$value
        )
      )
      next
    }
    # Infusion, need to discretize in multiple time steps
    applicationResults <- rbind.data.frame(
      applicationResults,
      data.frame(
        time = seq(
          application$startTime$value,
          application$startTime$value + application$infusionTime$value,
          length.out = infusionSteps
        ),
        drugMass = application$drugMass$value / infusionSteps
      )
    )
  }
  # Normalization uses total drug mass
  applicationResults$drugMass <- cumsum(applicationResults$drugMass)
  return(applicationResults)
}
