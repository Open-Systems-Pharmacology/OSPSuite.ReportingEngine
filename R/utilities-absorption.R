#' @title plotMeanAbsorption
#' @description Plot absorption diagnostics time profile
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings Options to be defined
#' (e.g. plotConfigurations list of `PlotConfiguration` objects)
#' @return list of `ggplot` objects
#' @import tlf
#' @import ospsuite
#' @import utils
#' @keywords internal
plotMeanAbsorption <- function(structureSet,
                               logFolder = getwd(),
                               settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  # Get drug mass to perform the drugmass normalized plot
  applications <- ospsuite::getContainer("Applications", simulation)
  appliedMoleculePaths <- ospsuite::getAllMoleculePathsIn(applications)

  appliedMolecules <- ospsuite::getAllMoleculesMatching(appliedMoleculePaths, simulation)

  resultsByCompound <- list()
  absorptionPlots <- list()
  absorptionCaptions <- list()
  absorptionTables <- list()
  for (compound in appliedMolecules) {
    fractionAbsorbedInVenousBloodPath <- paste0("Organism|VenousBlood|*|", compound$name)
    fractionAbsorbedInPortalVeinPath <- paste0("Organism|PortalVein|*|", compound$name)

    quantitiesInVenousBlood <- ospsuite::getAllQuantitiesMatching(fractionAbsorbedInVenousBloodPath, simulation)
    quantitiesInPortalVein <- ospsuite::getAllQuantitiesMatching(fractionAbsorbedInPortalVeinPath, simulation)

    resultsByCompound[[compound$name]] <- list(
      "compoundName" = compound$name,
      "drugMass" = compound$value,
      "fractionDissolvedPath" = paste0("Organism|Lumen|", compound$name, "|Fraction dissolved"),
      "fractionAbsorbedInMucosaPath" = paste0("Organism|Lumen|", compound$name, "|Fraction of oral drug mass absorbed into mucosa"),
      "fractionExcretedPath" = paste0("Organism|Lumen|Feces|", compound$name, "|Fraction excreted to feces"),
      "fractionAbsorbedInVenousBloodPaths" = sapply(quantitiesInVenousBlood, function(quantity) {
        quantity$path
      }),
      "fractionAbsorbedInPortalVeinPaths" = sapply(quantitiesInPortalVein, function(quantity) {
        quantity$path
      }),
      "timeProfileData" = NULL,
      "timeProfileMetaData" = NULL
    )
  }
  compoundNames <- names(resultsByCompound)

  # Matlab version was setting the relative value of these 2 parameters to 1
  # It is supposed to mean that the value is equal to reference value of the parameter
  # Not sure if I need to use scaleParameterValues method ?
  lungBloodFlowParameter <- ospsuite::getParameter("Organism|Lung|Blood flow rate", simulation)
  portalVeinBloodFlowParameter <- ospsuite::getParameter("Organism|PortalVein|Blood flow rate", simulation)

  # Add paths of
  # 1) Fraction dissolved
  # 2) Fraction absorbed in mucosa
  # 3) Fraction excreted
  # 4) Fraction absorbed in venous blood with lungBloodFlowParameter set to 0
  # 5) Fraction absorbed in portal vein with portalVeinBloodFlowParameter set to 0

  allFractionDissolvedPaths <- paste0("Organism|Lumen|", compoundNames, "|Fraction dissolved")
  allFractionAbsorbedInMucosaPaths <- paste0("Organism|Lumen|", compoundNames, "|Fraction of oral drug mass absorbed into mucosa")
  allFractionExcretedPaths <- paste0("Organism|Lumen|Feces|", compoundNames, "|Fraction excreted to feces")
  allFractionAbsorbedInVenousBloodPaths <- paste0("Organism|VenousBlood|*|", compoundNames)
  allFractionAbsorbedInPortalVeinPaths <- paste0("Organism|PortalVein|*|", compoundNames)

  # Get all the quantities with paths of fractions dissolved, absorbed and excreted
  quantitiesToSimulate <- ospsuite::getAllQuantitiesMatching(
    c(
      allFractionDissolvedPaths,
      allFractionAbsorbedInMucosaPaths,
      allFractionExcretedPaths,
      allFractionAbsorbedInVenousBloodPaths,
      allFractionAbsorbedInPortalVeinPaths
    ),
    simulation
  )

  # quantitiesPaths <- sapply(quantities, function(quantity){quantity$path})

  # Clear concentration output in case any concentrations are still included
  ospsuite::clearOutputs(simulation)
  for (quantity in quantitiesToSimulate) {
    addOutputs(quantitiesOrPaths = quantity, simulation = simulation)
  }

  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutput <- ospsuite::getOutputValues(
    simulationResults = simulationResults,
    quantitiesOrPaths = simulationResults$allQuantityPaths
  )

  # 4) Fraction absorbed in venous blood
  ospsuite::setParameterValues(lungBloodFlowParameter, 0)
  simulationResultsNoLungBloodFlow <- ospsuite::runSimulation(simulation)
  simulationResultsOutputNoLungBloodFlow <- ospsuite::getOutputValues(
    simulationResults = simulationResultsNoLungBloodFlow,
    quantitiesOrPaths = simulationResultsNoLungBloodFlow$allQuantityPaths
  )

  # 5) Fraction absorbed in venous blood
  ospsuite::setParameterValues(portalVeinBloodFlowParameter, 0)
  simulationResultsNoPortalVeinBloodFlow <- ospsuite::runSimulation(simulation)
  simulationResultsOutputNoPortalVeinBloodFlow <- ospsuite::getOutputValues(
    simulationResults = simulationResultsNoPortalVeinBloodFlow,
    quantitiesOrPaths = simulationResultsNoPortalVeinBloodFlow$allQuantityPaths
  )

  # Get results by Compound
  for (result in resultsByCompound) {
    # Results by compound as fractions
    fractionAbsorbedInVenousBlood <- rowSums(cbind.data.frame(
      simulationResultsOutputNoLungBloodFlow$data[, result$fractionAbsorbedInVenousBloodPaths, drop = FALSE],
      data.frame(dummyVariable = 0)
    )) / result$drugMass

    fractionAbsorbedInPortalVein <- rowSums(cbind.data.frame(
      simulationResultsOutputNoPortalVeinBloodFlow$data[, result$fractionAbsorbedInPortalVeinPaths, drop = FALSE],
      data.frame(dummyVariable = 0)
    )) / result$drugMass

    result$timeProfileData <- rbind.data.frame(
      data.frame(
        "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
        "Fraction" = simulationResultsOutput$data[, result$fractionDissolvedPath],
        "Legend" = "Fraction dissolved"
      ),
      data.frame(
        "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
        "Fraction" = simulationResultsOutput$data[, result$fractionAbsorbedInMucosaPath],
        "Legend" = "Fraction absorbed to mucosa"
      ),
      data.frame(
        "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
        "Fraction" = fractionAbsorbedInPortalVein,
        "Legend" = "Fraction absorbed to portal vein"
      ),
      data.frame(
        "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
        "Fraction" = fractionAbsorbedInVenousBlood,
        "Legend" = "Fraction absorbed to venous blood"
      ),
      data.frame(
        "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
        "Fraction" = simulationResultsOutput$data[, result$fractionExcretedPath],
        "Legend" = "Fraction excrected to feces"
      )
    )

    result$timeProfileMetaData <- list(
      "Time" = list(
        dimension = "Time",
        unit = structureSet$simulationSet$timeUnit
      ),
      "Fraction" = list(
        dimension = "Fraction of drugmass",
        unit = ""
      )
    )

    absorptionPlots[[result$compoundName]] <- plotAbsorptionTimeProfile(
      data = result$timeProfileData,
      metaData = result$timeProfileMetaData,
      dataMapping = tlf::XYGDataMapping$new(
        x = "Time",
        y = "Fraction",
        color = "Legend"
      ),
      plotConfiguration = settings$plotConfigurations[["absorptionPlot"]]
    )

    absorptionCaptions[[result$compoundName]] <- captions$absorption(result$compoundName)
    absorptionTables[[result$compoundName]] <- data.frame(
      Time = simulationResultsOutput$data[, "Time"],
      `Fraction dissolved` = simulationResultsOutput$data[, result$fractionDissolvedPath],
      `Fraction absorbed to mucosa` = simulationResultsOutput$data[, result$fractionAbsorbedInMucosaPath],
      `Fraction absorbed to portal vein` = fractionAbsorbedInPortalVein,
      `Fraction absorbed to venous blood` = fractionAbsorbedInVenousBlood,
      `Fraction excrected to feces` = simulationResultsOutput$data[, result$fractionExcretedPath],
      check.names = FALSE
    )
  }

  return(list(
    plots = absorptionPlots,
    tables = absorptionTables,
    captions = absorptionCaptions
  ))
}


#' @title plotAbsorptionTimeProfile
#' @description Plot absorption time profile
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
#' @importFrom ospsuite.utils %||%
plotAbsorptionTimeProfile <- function(data,
                                      metaData = NULL,
                                      dataMapping = NULL,
                                      plotConfiguration = NULL) {
  timeVsFractionDataMapping <- dataMapping %||% tlf::XYGDataMapping$new(
    x = "Time",
    y = "Fraction",
    color = "Legend"
  )

  timeVsFractionPlotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = timeVsFractionDataMapping
  )

  timeVsFractionPlot <- tlf::addLine(
    data = data,
    metaData = metaData,
    dataMapping = timeVsFractionDataMapping,
    plotConfiguration = timeVsFractionPlotConfiguration
  )
  return(timeVsFractionPlot)
}
