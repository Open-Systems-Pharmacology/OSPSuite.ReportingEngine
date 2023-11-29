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
#' @importFrom stats approx
#' @keywords internal
plotMeanMassBalance <- function(structureSet, settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)
  allMoleculePaths <- ospsuite::getAllMoleculePathsIn(simulation)
  ospsuite::clearOutputs(simulation)
  addOutputs(quantitiesOrPaths = allMoleculePaths, simulation = simulation)
  simulationResults <- ospsuite::runSimulation(simulation)

  allAppliedCompoundNames <- simulation$allXenobioticFloatingMoleculeNames()
  # Default configuration behavior integrates all xenobiotic molecules in mass balance
  # Note that a list of lists is expected from the json file,
  # thus the same format is used for default configuration
  massBalanceSettings <- list(list(
    Name = paste("Mass Balance for", paste(allAppliedCompoundNames, collapse = ", ")),
    Molecules = allAppliedCompoundNames
  ))
  massBalanceSettings <- structureSet$simulationSet$massBalanceSettings %||% massBalanceSettings

  massBalanceResults <- list()
  for (plotSettings in massBalanceSettings) {
    compoundNames <- as.character(plotSettings$Molecules)
    # Sub section using settings name
    if (!isEmpty(plotSettings$Name)) {
      sectionId <- defaultFileNames$resultID(length(massBalanceResults) + 1, "mass_balance")
      massBalanceResults[[sectionId]] <- saveTaskResults(
        id = sectionId,
        textChunk = paste("###", plotSettings$Name, anchor(sectionId)),
        includeTextChunk = TRUE
      )
    }
    massBalanceData <- getMassBalanceData(
      groupings = plotSettings$Groupings,
      compoundNames = compoundNames,
      simulation = simulation,
      simulationResults = simulationResults
    )

    # Get applications for drug mass normalization, only the applied compound name should be used
    appliedCompoundName <- intersect(compoundNames, allAppliedCompoundNames)
    validateIsOfLength(appliedCompoundName, 1)
    applications <- simulation$allApplicationsFor(appliedCompoundName)
    # Application results formatted as a data.frame with time and cumulative drugMass
    applicationResults <- getApplicationResults(applications)

    # Format to appropriate units
    massBalanceData$Time <- ospsuite::toUnit(
      "Time",
      massBalanceData$Time,
      structureSet$simulationSet$timeUnit
    )
    timeTicks <- getTimeTicksFromUnit(
      structureSet$simulationSet$timeUnit,
      timeValues = massBalanceData$Time
    )

    metaData <- list(
      "Time" = list(
        dimension = "Time",
        unit = structureSet$simulationSet$timeUnit
      ),
      "Amount" = list(
        dimension = applications[[1]]$drugMass$dimension,
        unit = applications[[1]]$drugMass$unit
      ),
      "NormalizedAmount" = list(
        dimension = applications[[1]]$drugMass$dimension,
        unit = "fraction of drugmass"
      )
    )

    # The function approx aims at intra- and extrapolating the total drug mass data
    # at the same time points as the mass balance data
    massBalanceData$DrugMass <- approx(
      x = ospsuite::toUnit(
        "Time",
        applicationResults$time,
        structureSet$simulationSet$timeUnit
      ),
      y = applicationResults$totalDrugMass,
      xout = massBalanceData$Time,
      method = "constant",
      # argument rule defines how interpolation is to take place outside the interval [min(x), max(x)]
      # 1: predicts NA outside of range of x
      # 2: predicts the y value at the closest x data extreme
      rule = 2
      # since approx outputs a data.frame of x and y variables, only extract y values
    )$y
    massBalanceData <- massBalanceData %>% mutate(NormalizedAmount = Amount / DrugMass)

    #------ Get mass balance plots ------
    # Time profile
    timeProfileID <- defaultFileNames$resultID(
      length(massBalanceResults) + 1,
      "mass_balance",
      structureSet$simulationSet$simulationSetName
    )
    timeProfilePlot <- tlf::plotSimulatedTimeProfile(
      data = massBalanceData,
      metaData = metaData,
      dataMapping = getMassBalanceDataMapping("timeProfile"),
      plotConfiguration = getMassBalancePlotConfiguration(
        plotType = "timeProfile",
        data = massBalanceData,
        metaData = metaData,
        settings = settings
      )
    )
    massBalanceResults[[timeProfileID]] <- saveTaskResults(
      id = timeProfileID,
      plot = timeProfilePlot,
      plotCaption = captions$massBalance$timeProfile(compoundNames)
    )

    # Cumulative time profile
    cumulativeTimeProfileID <- defaultFileNames$resultID(
      length(massBalanceResults) + 1,
      "mass_balance",
      structureSet$simulationSet$simulationSetName
    )
    cumulativeTimeProfilePlot <- tlf::plotCumulativeTimeProfile(
      data = massBalanceData,
      metaData = metaData,
      dataMapping = getMassBalanceDataMapping("cumulativeTimeProfile"),
      plotConfiguration = getMassBalancePlotConfiguration(
        plotType = "cumulativeTimeProfile",
        data = massBalanceData,
        metaData = metaData,
        settings = settings
      )
    )
    massBalanceResults[[cumulativeTimeProfileID]] <- saveTaskResults(
      id = cumulativeTimeProfileID,
      plot = cumulativeTimeProfilePlot,
      plotCaption = captions$massBalance$cumulativeTimeProfile(compoundNames)
    )

    # Normalized time profile
    normalizedTimeProfileID <- defaultFileNames$resultID(
      length(massBalanceResults) + 1,
      "mass_balance",
      structureSet$simulationSet$simulationSetName
    )
    normalizedTimeProfilePlot <- tlf::plotSimulatedTimeProfile(
      data = massBalanceData,
      metaData = metaData,
      dataMapping = getMassBalanceDataMapping("normalizedTimeProfile"),
      plotConfiguration = getMassBalancePlotConfiguration(
        plotType = "normalizedTimeProfile",
        data = massBalanceData,
        metaData = metaData,
        settings = settings
      )
    )
    massBalanceResults[[normalizedTimeProfileID]] <- saveTaskResults(
      id = normalizedTimeProfileID,
      plot = normalizedTimeProfilePlot,
      plotCaption = captions$massBalance$normalizedTimeProfile(compoundNames)
    )

    # Normalized cumulative time profile
    normalizedCumulativeTimeProfileID <- defaultFileNames$resultID(
      length(massBalanceResults) + 1,
      "mass_balance",
      structureSet$simulationSet$simulationSetName
    )
    normalizedCumulativeTimeProfilePlot <- tlf::plotCumulativeTimeProfile(
      data = massBalanceData,
      metaData = metaData,
      dataMapping = getMassBalanceDataMapping("normalizedCumulativeTimeProfile"),
      plotConfiguration = getMassBalancePlotConfiguration(
        plotType = "normalizedCumulativeTimeProfile",
        data = massBalanceData,
        metaData = metaData,
        settings = settings
      )
    )
    massBalanceResults[[normalizedCumulativeTimeProfileID]] <- saveTaskResults(
      id = normalizedCumulativeTimeProfileID,
      plot = normalizedCumulativeTimeProfilePlot,
      plotCaption = captions$massBalance$normalizedCumulativeTimeProfile(compoundNames)
    )

    # Pie Chart
    pieChartID <- defaultFileNames$resultID(
      length(massBalanceResults) + 1,
      "mass_balance",
      structureSet$simulationSet$simulationSetName
    )

    pieChartData <- massBalanceData %>%
      filter(Time == max(Time)) %>%
      mutate(LegendWithPercent = paste(
        Legend, " (", round(100 * NormalizedAmount, digits = 1), "%)",
        sep = ""
      ))
    # Ensure that the colors and legend match previous mass balance plots by re-ordering Legend
    pieChartData$LegendWithPercent <- reorder(
      pieChartData$LegendWithPercent,
      as.numeric(factor(pieChartData$Legend))
    )
    pieChartPlot <- tlf::plotPieChart(
      data = pieChartData,
      metaData = metaData,
      dataMapping = getMassBalanceDataMapping("pieChart"),
      plotConfiguration = settings$plotConfigurations[["pieChart"]]
    )

    # Get time caption text for report
    timeCaption <- formatNumerics(
      max(massBalanceData$Time),
      digits = settings$digits,
      scientific = settings$scientific
    )
    massBalanceResults[[pieChartID]] <- saveTaskResults(
      id = pieChartID,
      plot = pieChartPlot,
      plotCaption = captions$massBalance$pieChart(
        timeCaption,
        metaData$Time$unit,
        compoundNames
      )
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
      table = massBalanceData,
      includeTable = FALSE
    )
  }
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
#' total drug mass as a function of time.
#' @param applications
#' list of `Application` objects queried by the method `simulation$allApplicationsFor()`
#' @return A data.frame that includes `time`, `drugMass` and `totalDrugMass` as variables
#' @import ospsuite
#' @import ospsuite.utils
#' @import dplyr
#' @keywords internal
getApplicationResults <- function(applications) {
  applicationResults <- data.frame()
  for (application in applications) {
    applicationResults <- rbind.data.frame(
      applicationResults,
      data.frame(
        time = application$startTime$value,
        drugMass = application$drugMass$value
      )
    )
  }
  # Total drug mass is cumulative sum of all the applied drug mass
  applicationResults <- applicationResults %>%
    mutate(totalDrugMass = cumsum(drugMass))
  return(applicationResults)
}

#' @title getMassBalanceData
#' @description Get mass balance data for a set of compounds
#' @param groupings A list of grouping lists that define naming and inclusion/exclusion criteria
#' @param compoundNames A vector of compound names
#' @param simulation A `Simulation` object
#' @param simulationResults A `SimulationResults` object
#' @return A data.frame that includes `Time`, `Amount` and `Legend` as variables
#' @import ospsuite
#' @import ospsuite.utils
#' @keywords internal
getMassBalanceData <- function(groupings, compoundNames, simulation, simulationResults) {
  # If groupings is NULL, use default inclusion/exclusion and grouping
  groupings <- groupings %||% defaultMassBalanceGroupings(compoundNames)

  massBalanceData <- data.frame()
  previouslyIncludedMoleculePaths <- NULL
  for (group in groupings) {
    includedMolecules <- ospsuite::getAllMoleculesMatching(group$Include, simulation)
    includedMoleculePaths <- sapply(includedMolecules, function(molecule) {
      molecule$path
    })
    # Exclusion criteria
    excludedMoleculePaths <- NULL
    # If ExcludePreviousGroupings is TRUE, exclude molecules from previous groupings
    if (group$ExcludePreviousGroupings %||% TRUE) {
      excludedMoleculePaths <- previouslyIncludedMoleculePaths
    }
    if (!is.null(group$Exclude)) {
      excludedMolecules <- ospsuite::getAllMoleculesMatching(group$Exclude, simulation)
      excludedMoleculePaths <- c(
        excludedMoleculePaths,
        sapply(excludedMolecules, function(molecule) {
          molecule$path
        })
      )
    }
    includedMolecules <- includedMolecules[!includedMoleculePaths %in% excludedMoleculePaths]
    includedMoleculePaths <- setdiff(includedMoleculePaths, excludedMoleculePaths)
    if (isEmpty(includedMoleculePaths)) {
      warning(messages$noMoleculePathsIncluded(group$Name), call. = FALSE)
      next
    }
    validateMoleculesFromCompounds(includedMolecules, compoundNames)
    checkMoleculesAlreadyIncluded(includedMoleculePaths, previouslyIncludedMoleculePaths)

    previouslyIncludedMoleculePaths <- c(
      previouslyIncludedMoleculePaths,
      includedMoleculePaths
    )

    # For included molecule paths, get their amount vs time and sum them
    moleculeAmounts <- sapply(
      includedMoleculePaths,
      function(moleculePath) {
        simulationResults$getValuesByPath(moleculePath, individualIds = simulationResults$allIndividualIds)
      }
    )
    moleculeAmounts <- rowSums(as.data.frame(moleculeAmounts))

    massBalanceData <- rbind.data.frame(
      massBalanceData,
      data.frame(
        Time = simulationResults$timeValues,
        Amount = moleculeAmounts,
        Legend = group$Name,
        stringsAsFactors = FALSE
      )
    )
  }
  return(massBalanceData)
}

#' @title defaultMassBalanceGrouping
#' @description Get mass balance default inclusion/exclusion defined as groupings list
#' Default groups are
#' \itemize{
#' \item Plasma
#' \item BloodCells
#' \item Interstitial
#' \item Intracellular
#' \item Endosome
#' \item Gallbladder
#' \item Urine
#' \item Feces
#' \item Rest (includes everything else but Saliva compartments)
#' }
#' @param compoundNames Names of simulation molecules
#' @import ospsuite
#' @keywords internal
defaultMassBalanceGroupings <- function(compoundNames) {
  groupNames <- c(
    "Plasma",
    "BloodCells",
    "Interstitial",
    "Intracellular",
    "Endosome",
    "Gallbladder",
    "Urine",
    "Feces"
  )

  defaultGroupings <- c(
    # Lumen Compartment excluding Feces
    list(list(
        Name = paste(paste(compoundNames, collapse = ", "), "Lumen", sep = " - "),
        Include = paste0("Organism|Lumen|*|", compoundNames),
        Exclude = paste0("Organism|Lumen|Feces|", compoundNames)
      )),
    lapply(
      groupNames,
      function(groupName) {
        list(
          Name = paste(paste(compoundNames, collapse = ", "), groupName, sep = " - "),
          Include = paste0("Organism|**|", groupName, "|", compoundNames)
        )
      }
    ),
    # Rest Compartment
    list(list(
        Name = paste("Rest of", paste(compoundNames, collapse = ", ")),
        # Since default option excludes previously included paths,
        # we can include all the paths but saliva here
        Include = paste0("Organism|**|", compoundNames),
        # Exclude saliva from rest
        Exclude = paste0("Organism|Saliva|**|", compoundNames)
      ))
  )
  return(defaultGroupings)
}
