#' @title getOutputsFromConfigurationPlan
#' @description Get a list of outputs from simulation and from `ConfigurationPlan`
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A dataframe of project, simulation, output paths and (if applicable) pk parameters and start and end times of interval over which the pk parameter is evaluated
#' @keywords internal
getOutputsFromConfigurationPlan <- function(configurationPlan) {
  outputsTimeProfile <- getTimeProfileOutputsDataframe(configurationPlan)
  outputsComparisonTimeProfile <- getComparisonTimeProfileOutputsDataframe(configurationPlan)
  outputsGOF <- getGOFOutputsDataframe(configurationPlan)
  outputsDDI <- getDDIOutputsDataframe(configurationPlan)
  outputsPKRatio <- getPKRatioOutputsDataframe(configurationPlan)

  outputs <- rbind.data.frame(
    outputsTimeProfile,
    outputsComparisonTimeProfile,
    outputsGOF,
    outputsDDI,
    outputsPKRatio,
    stringsAsFactors = FALSE
  )

  return(outputs[!duplicated(outputs), ])
}

#' @title getTimeProfileOutputsDataframe
#' @description Get a dataframe relating project, simulation and output path for each time profile plot component
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A dataframe containing data for generating time profile plots
#' @keywords internal
getTimeProfileOutputsDataframe <- function(configurationPlan) {
  timeProfileOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$TimeProfile) {
    validateIsIncluded(values = "Plot", parentValues = names(plot), nullAllowed = TRUE)
    # Accounts for paths of both mean and pop time profiles
    # If population, paths is directly QuantityPath and Curves is NULL
    # If mean, paths from QuantityPath is NULL and increases using Curves coming from same simulation
    paths <- plot$Plot$Analysis$Fields[[1]]$QuantityPath
    for (curve in plot$Plot$Curves) {
      if (isObservedData(curve$Y)) {
        next
      }
      paths <- c(paths, ospsuite::toPathString(tail(ospsuite::toPathArray(curve$Y), -1)))
    }
    timeProfileOutputsDataframe <- rbind.data.frame(
      timeProfileOutputsDataframe,
      data.frame(
        project = plot$Project,
        simulation = plot$Simulation,
        outputPath = paths,
        pkParameter = NA,
        startTime = NA,
        endTime = NA,
        stringsAsFactors = FALSE
      ),
      stringsAsFactors = FALSE
    )
  }
  return(timeProfileOutputsDataframe[!duplicated(timeProfileOutputsDataframe), ])
}

#' @title getComparisonTimeProfileOutputsDataframe
#' @description Get a dataframe relating project, simulation and output path for each comparison time profile plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A dataframe containing data for generating time profile plots
#' @keywords internal
getComparisonTimeProfileOutputsDataframe <- function(configurationPlan) {
  comparisonTimeProfileOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$ComparisonTimeProfilePlots) {
    for (outputMapping in plot$OutputMappings) {
      comparisonTimeProfileOutputsDataframe <- rbind.data.frame(
        comparisonTimeProfileOutputsDataframe,
        data.frame(
          project = outputMapping$Project,
          simulation = outputMapping$Simulation,
          outputPath = outputMapping$Output,
          pkParameter = NA,
          startTime = NA,
          endTime = NA,
          stringsAsFactors = FALSE
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  return(comparisonTimeProfileOutputsDataframe[!duplicated(comparisonTimeProfileOutputsDataframe), ])
}

#' @title getGOFOutputsDataframe
#' @description Get a dataframe relating project, simulation and output path for each GOF plot component
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A dataframe containing data for generating GOF plots
#' @keywords internal
getGOFOutputsDataframe <- function(configurationPlan) {
  gofOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$GOFMergedPlots) {
    validateIsIncluded(values = "Groups", parentValues = names(plot), nullAllowed = TRUE)
    for (group in plot$Groups) {
      validateIsIncluded(values = "OutputMappings", parentValues = names(group), nullAllowed = TRUE)
      for (outputMapping in group$OutputMappings) {
        validateIsIncluded(values = "Output", parentValues = names(outputMapping), nullAllowed = TRUE)
        validateIsString(object = outputMapping$Output)
        gofOutputsDataframe <- rbind.data.frame(
          gofOutputsDataframe,
          data.frame(
            project = outputMapping$Project,
            simulation = outputMapping$Simulation,
            outputPath = outputMapping$Output,
            pkParameter = NA,
            startTime = NA,
            endTime = NA,
            stringsAsFactors = FALSE
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  return(gofOutputsDataframe[!duplicated(gofOutputsDataframe), ])
}

#' @title getDDIOutputsDataframe
#' @description Get a dataframe relating project, simulation, output, pk parameter, start time, end time for each DDI plot component
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A list containing data for generating DDI plots
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getDDIOutputsDataframe <- function(configurationPlan) {
  ddiOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$DDIRatioPlots) {
    pkParameters <- plot$PKParameters %||% ospsuite::toPathArray(plot$PKParameter)

    for (group in plot$Groups) {
      for (ddiRatio in group$DDIRatios) {
        outputPath <- ddiRatio$Output
        for (simulationType in c("SimulationControl", "SimulationDDI")) {
          plotComponent <- ddiRatio[[simulationType]]
          startTime <- getTimeFromPlan(plotComponent, "StartTime")
          endTime <- getTimeFromPlan(plotComponent, "EndTime")

          newPKParameterNames <- NULL
          for (pkParameter in pkParameters) {
            checkPKParameterEndTime(pkParameter, endTime)
            # Configuration plan AUC default to AUC_tEnd
            # If end time is Inf, needs to use AUC_inf instead
            useAUCinf <- all(isEmpty(endTime), isIncluded(pkParameter, "AUC"))
            if (useAUCinf) {
              pkParameter <- "AUC_inf"
            }
            newPKParameterNames <- c(
              newPKParameterNames,
              addNewPkParameter(pkParameter, startTime, endTime)
            )
          }

          df <- data.frame(
            project = plotComponent$Project,
            simulation = plotComponent$Simulation,
            outputPath = outputPath,
            pkParameter = newPKParameterNames %||% NA,
            startTime = startTime %||% NA,
            endTime = endTime %||% NA,
            stringsAsFactors = FALSE
          )
          ddiOutputsDataframe <- rbind.data.frame(ddiOutputsDataframe, df, stringsAsFactors = FALSE)
        }
      }
    }
  }
  return(ddiOutputsDataframe[!duplicated(ddiOutputsDataframe), ])
}

#' @title getPKRatioOutputsDataframe
#' @description Get a dataframe relating project, simulation, output path and pk parameter for each PK ratio plot
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A list containing data for generating DDI plots
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getPKRatioOutputsDataframe <- function(configurationPlan) {
  pkRatioOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$PKRatioPlots) {
    pkParameters <- plot$PKParameters %||% ospsuite::toPathArray(plot$PKParameter)

    for (group in plot$Groups) {
      for (plotComponent in group$PKRatios) {
        outputPath <- plotComponent$Output
        startTime <- NULL
        endTime <- NULL
        newPKParameterNames <- NULL
        for (pkParameter in pkParameters) {
          newPKParameterNames <- c(
            newPKParameterNames,
            addNewPkParameter(pkParameter, startTime, endTime)
          )
        }

        df <- data.frame(
          project = plotComponent$Project,
          simulation = plotComponent$Simulation,
          outputPath = outputPath,
          pkParameter = newPKParameterNames %||% NA,
          startTime = startTime %||% NA,
          endTime = endTime %||% NA,
          stringsAsFactors = FALSE
        )

        pkRatioOutputsDataframe <- rbind.data.frame(pkRatioOutputsDataframe, df, stringsAsFactors = FALSE)
      }
    }
  }
  return(pkRatioOutputsDataframe[!duplicated(pkRatioOutputsDataframe), ])
}

#' @title addNewPkParameter
#' @description Create a PK parameter calculated between a start and end time as specified in a qualification `ConfigurationPlan` and return the PK parameter name
#' @param pkParameter the name of the PK parameter from the qualification `ConfigurationPlan`
#' @param startTime the starting time of the interval over which the PK parameter is calculated (from the qualification `ConfigurationPlan`)
#' @param endTime the ending time of the interval over which the PK parameter is calculated (from the qualification `ConfigurationPlan`)
#' @return String `pkParameterName`
#' @keywords internal
addNewPkParameter <- function(pkParameter, startTime, endTime) {
  pkParameterName <- generateDDIPlotPKParameterName(pkParameter, startTime, endTime)

  if (pkParameterName %in% ospsuite::allPKParameterNames()) {
    return(pkParameterName)
  }

  newPKParameter <- ospsuite::addUserDefinedPKParameter(
    name = pkParameterName,
    standardPKParameter = StandardPKParameter[[pkDictionaryQualificationOSP[[pkParameter]]]],
    displayName = pkParameterName
  )
  if (!is.null(startTime)) {
    newPKParameter$startTime <- startTime
  }

  if (!is.null(endTime)) {
    newPKParameter$endTime <- endTime
  }
  return(pkParameterName)
}

#' @title generateDDIPlotPKParameterName
#' @description Generate name for a PK parameter calculated between a start and end time
#' @param pkParameter the name of the PK parameter from the qualification `ConfigurationPlan`
#' @param startTime the starting time of the interval over which the PK parameter is calculated (from the qualification `ConfigurationPlan`)
#' @param endTime the ending time of the interval over which the PK parameter is calculated (from the qualification `ConfigurationPlan`)
#' @return String `pkParameterName`
#' @keywords internal
generateDDIPlotPKParameterName <- function(pkParameter, startTime, endTime) {
  validateIsIncluded(values = pkParameter, parentValues = names(pkDictionaryQualificationOSP))
  standardPKParameter <- pkDictionaryQualificationOSP[[pkParameter]]
  pkParameterName <- standardPKParameter
  pkParameterName <- ifNotNull(startTime, paste0(pkParameterName, "_tStartTime_", startTime), pkParameterName)
  pkParameterName <- ifNotNull(endTime, paste0(pkParameterName, "_tEndTime_", endTime), pkParameterName)
  return(pkParameterName)
}

#' Dictionary for `ospsuite` PK parameters defined by Configuration Plan
#' @keywords internal
pkDictionaryQualificationOSP <- c(
  enum(ospsuite::allPKParameterNames()),
  list(
    AUC = "AUC_tEnd",
    CMAX = "C_max"
  )
)

#' @title getTimeFromPlan
#' @description
#' Get endTime value in base unit from configuration plan field
#' @param configurationPlanField The configuration plan field listing `endTime` value and unit
#' @param fieldName The name of the field to extract the time from (e.g., "EndTime")
#' @return `endTime` value in base unit
#' @keywords internal
getTimeFromPlan <- function(configurationPlanField, fieldName) {
  timeIsNumeric <- is.numeric(configurationPlanField[[fieldName]])
  if (!timeIsNumeric) {
    return(NULL)
  }
  timeValue <- ospsuite::toBaseUnit(
    quantityOrDimension = ospDimensions$Time,
    values = configurationPlanField[[fieldName]],
    unit = configurationPlanField$TimeUnit
  )
  return(timeValue)
}
