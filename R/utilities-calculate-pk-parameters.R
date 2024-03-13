#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @return pkAnalysis object
#' @import ospsuite
#' @keywords internal
calculatePKParameters <- function(structureSet, settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation = simulation,
    filePaths = structureSet$simulationResultFileNames
  )

  logDebug(paste0("Simulation results '", structureSet$simulationResultFileNames, "' successfully loaded"))

  pkAnalyses <- calculatePKAnalyses(results = simulationResults)
  logDebug("Calculation of PK parameters complete")
  return(pkAnalyses)
}

#' @title plotMeanPKParameters
#' @description Plot PK parameters table
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the output table/plot
#' @return data.frame with calculated PK parameters for the simulation set
#' @import ospsuite
#' @keywords internal
plotMeanPKParameters <- function(structureSet, settings = NULL) {
  pkParametersData <- NULL
  pkParametersResults <- list()

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$pkAnalysisResultsFileNames)
  pkParametersTable <- loadPKAnalysesFromSet(structureSet = structureSet, to = "data.frame", useCache = TRUE)
  for (output in structureSet$simulationSet$outputs) {
    molWeight <- simulation$molWeightFor(output$path)

    pkParametersData <- rbind.data.frame(
      pkParametersData,
      getMeanPKAnalysesFromOutput(pkParametersTable, output, molWeight)
    )
  }
  pkParametersData$Value <- replaceInfWithNA(pkParametersData$Value)

  tableID <- defaultFileNames$resultID("pk_parameters", structureSet$simulationSet$simulationSetName)
  pkParametersResults[[tableID]] <- saveTaskResults(
    id = tableID,
    table = pkParametersData,
    tableCaption = captions$plotPKParameters$mean(
      structureSet$simulationSet$simulationSetName,
      structureSet$simulationSetDescriptor
    ),
    includeTable = TRUE
  )

  return(pkParametersResults)
}

#' @title getMeanPKAnalysesFromOutput
#' @description Get PK analyses from an `Output` object
#' @param data A data.frame of PK Analyses
#' @param output An `Output` object defining `pkParameters`
#' @param molWeight Molecular weight for converting into PK Parameter `displayUnit`
#' @return A data.frame with `Path`, `Parameter`, `Value` and `Unit` to display in final report
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getMeanPKAnalysesFromOutput <- function(data, output, molWeight = NULL) {
  pkAnalysesFromOutput <- NULL
  validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  for (pkParameter in output$pkParameters) {
    displayName <- pkParameter$displayName
    displayUnit <- pkParameter$displayUnit

    validateIsIncluded(pkParameter$pkParameter, unique(outputData$Parameter))
    selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter
    pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

    # Need to switch back to base unit first if a display unit is provided
    pkParameterValue <- outputData$Value[selectedParameter]

    if (!is.null(displayUnit)) {
      pkParameterValue <- ospsuite::toUnit(
        quantityOrDimension = pkParameterObject$dimension,
        values = as.numeric(outputData$Value[selectedParameter]),
        targetUnit = displayUnit,
        molWeight = molWeight,
        sourceUnit = pkParameterObject$displayUnit
      )
    }

    pkAnalysesFromOutput <- rbind.data.frame(
      pkAnalysesFromOutput,
      data.frame(
        Path = output$displayName,
        Parameter = displayName %||% outputData$Parameter[selectedParameter],
        Value = pkParameterValue,
        Unit = displayUnit %||% outputData$Unit[selectedParameter]
      )
    )
  }
  return(pkAnalysesFromOutput)
}
