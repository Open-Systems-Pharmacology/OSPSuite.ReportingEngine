captions <- list(
  absorption = function(compound) {
    paste0("Absorption of ", compound)
  },
  massBalance = list(
    timeProfile = function(compoundNames) {
      paste(
        "Amount of drug vs time within the different compartments for",
        paste(compoundNames, collapse = ", ")
      )
    },
    cumulativeTimeProfile = function(compoundNames) {
      paste(
        "Cumulated amount of drug vs time within the different compartments for",
        paste(compoundNames, collapse = ", ")
      )
    },
    normalizedTimeProfile = function(compoundNames) {
      paste(
        "Amount of drug vs time within the different compartments normalized to applicated drugmass for",
        paste(compoundNames, collapse = ", ")
      )
    },
    normalizedCumulativeTimeProfile = function(compoundNames) {
      paste(
        "Cumulated amount of drug vs time within the different compartments normalized to applicated drugmass for",
        paste(compoundNames, collapse = ", ")
      )
    },
    pieChart = function(time, timeUnit, compoundNames) {
      paste0(
        "Fraction of drug within the different compartments at ", time, timeUnit, " for ",
        paste(compoundNames, collapse = ", ")
        )
    }
  ),
  demography = list(
    parameterSection = function(sectionId, parameterName) {
      paste("##", parameterName, "distributions", anchor(sectionId))
    },
    xParameterSection = function(sectionId, parameterName) {
      paste("## ", parameterName, "-dependence ", anchor(sectionId), sep = "")
    },
    yParameterSection = function(sectionId, parameterName) {
      paste("### Dependence of", parameterName, anchor(sectionId))
    },
    populationSection = function(sectionId, simulationSetName, descriptor, level = 4) {
      tagLevel <- paste0(rep("#", level), collapse = "")
      sectionTitle <- paste(tagLevel, "For", reportSimulationSet(simulationSetName, descriptor))
      return(paste(sectionTitle, anchor(sectionId)))
    },
    histogramLegend = function(data, observed = FALSE) {
      if (observed) {
        return(paste0("Observed data (n=", nrow(data), ")"))
      }
      return(paste0("Simulated virtual population (n=", nrow(data), ")"))
    },
    histogram = function(parameterName, simulationSetName, descriptor, dataSource = "") {
      paste0(
        "Distribution of ", parameterName, " for ",
        reportSimulationSet(simulationSetName, descriptor),
        dataSource
      )
    },
    rangePlotLegend = function(simulationSetName, n, dataType = "Simulated", parameterClass = "numeric") {
      paste0(
        dataType, " ",
        switch(parameterClass,
          "character" = "data for",
          paste(AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range, "for")
        ),
        " ", simulationSetName, " (n=", n, ")"
      )
    },
    rangePlot = function(xParameterName, yParameterName, simulationSetName, descriptor, referenceSetName = NULL, plotScale = "linear", parameterClass = "numeric", dataSource = "") {
      referenceSetText <- ifNotNull(referenceSetName, paste0(" in comparison to ", referenceSetName), "")
      return(paste0(
        xParameterName, "-dependence of ", yParameterName, " for ", reportSimulationSet(simulationSetName, descriptor),
        referenceSetText,
        getPlotScaleCaption("Profiles", plotScale),
        switch(parameterClass,
          "character" = getBoxplotDescriptor(),
          ""
        ),
        dataSource
      ))
    }
  ),
  plotGoF = list(
    timeProfile = function(simulationSetName, descriptor, dataSource = "", plotScale = "linear") {
      return(paste0(
        "Time profiles for ", reportSimulationSet(simulationSetName, descriptor),
        dataSource, getPlotScaleCaption("Time profiles", plotScale)
      ))
    },
    obsVsPred = function(simulationSetName, descriptor, dataSource = "", plotScale = "linear", yCaption = NULL, pathName = NULL) {
      return(paste0(
        "Predicted ", getStatisticsCaption(yCaption), "vs observed for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor),
        dataSource, getPlotScaleCaption("Predictions and observations", plotScale)
      ))
    },
    resVsPred = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, yCaption = NULL, pathName = NULL) {
      return(paste0(
        plotScale, " residuals vs predicted ", getStatisticsCaption(yCaption), "values for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor),
        dataSource
      ))
    },
    resVsTime = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals vs time values for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor),
        dataSource
      ))
    },
    resHisto = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals distribution (stacked) for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor),
        dataSource
      ))
    },
    resQQPlot = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor),
        " as quantile-quantile plot", dataSource
      ))
    },
    meanLegend = function(simulationSetName, descriptor, pathName) {
      return(paste0("Simulated ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"))
    },
    populationLegend = function(simulationSetName, descriptor, statistics, pathName) {
      paste0(
        "Simulated ", statistics$yCaption, " and ", statistics$rangeCaption,
        " for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    },
    resLegend = function(simulationSetName, descriptor, pathName) {
      return(paste0(pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"))
    },
    observedLegend = function(simulationSetName, descriptor, pathName) {
      paste0(
        "Observed data for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    },
    lloqLegend = function(simulationSetName, descriptor, pathName) {
      paste0(
        "BLQ data for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    }
  ),
  plotPKParameters = list(
    mean = function(simulationSetName, descriptor) {
      paste0("PK parameters for ", reportSimulationSet(simulationSetName, descriptor))
    },
    outputSection = function(pathName, pathID) {
      paste("## PK Parameters of", pathName, anchor(pathID))
    },
    parameterSection = function(parameterName, parameterID) {
      paste("###", parameterName, anchor(parameterID))
    },
    boxplot = function(parameterName, pathName, simulationSetName, descriptor, plotScale = "linear") {
      paste(
        parameterName, "of", pathName, "for", reportSimulationSet(simulationSetName, descriptor),
        getBoxplotDescriptor(), "in", plotScale, "scale."
      )
    },
    summaryTable = function(parameterName, pathName, simulationSetName, descriptor, displayUnit) {
      return(paste0("Population statistics summarizing ", parameterName, " of ", pathName, " for ", reportSimulationSet(simulationSetName, descriptor), reportUnit(displayUnit)))
    },
    ratioTable = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName) {
      paste(
        parameterName, "ratio population summary statistics of",
        pathName, "for", reportSimulationSet(simulationSetName, descriptor),
        "in comparison to", referenceSetName
      )
    },
    relativeChangeTable = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName) {
      paste(
        "Ratio of population summary statistics of",
        parameterName, "of", pathName, " for ", reportSimulationSet(simulationSetName, descriptor),
        "in comparison to", referenceSetName
      )
    },
    ratioPlot = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName, plotScale = "linear") {
      paste(
        captions$plotPKParameters$ratioTable(parameterName, pathName, simulationSetName, descriptor, referenceSetName),
        getBoxplotDescriptor(), "in", plotScale, "scale."
      )
    },
    relativeChangePlot = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName, plotScale = "linear") {
      paste(
        captions$plotPKParameters$relativeChangeTable(parameterName, pathName, simulationSetName, descriptor, referenceSetName),
        getBoxplotDescriptor(ratio = TRUE), "in", plotScale, "scale."
      )
    },
    rangePlot = function(xParameterName, yParameterName, simulationSetName, descriptor, referenceSetName = NULL, plotScale = "linear") {
      referenceSetText <- ifNotNull(referenceSetName, paste0(" in comparison to ", referenceSetName), "")
      return(paste0(
        xParameterName, "-dependence of ", yParameterName, " for ", reportSimulationSet(simulationSetName, descriptor),
        referenceSetText, ". Profiles are plotted in a ", plotScale, " scale."
      ))
    }
  ),
  plotSensitivity = list(
    mean = function(parameterName, pathName) {
      paste0("Most sensitive parameters for ", parameterName, " of ", pathName, ".")
    },
    population = function(parameterName, pathName, quantiles, simulationSetName, descriptor) {
      quantileText <- paste0(quantiles, collapse = ", ")
      return(paste0(
        "Most sensitive parameters for ", parameterName, " of ", pathName,
        " for individuals at percentiles ", quantileText, " for ",
        reportSimulationSet(simulationSetName, descriptor), "."
      ))
    }
  )
)

getDataSourceCaption <- function(structureSet) {
  # If no observed data, end the sentence of caption
  if (isEmpty(structureSet$simulationSet$dataSource)) {
    return(". ")
  }
  dataSourceCaption <- structureSet$simulationSet$dataSource$getCaption(structureSet$workflowFolder)
  return(paste0(". ", dataSourceCaption, "."))
}

getGoodnessOfFitCaptions <- function(structureSet, plotType, plotScale = "linear", settings = NULL) {
  dataSource <- getDataSourceCaption(structureSet)
  simulationSetName <- structureSet$simulationSet$simulationSetName
  setDescriptor <- structureSet$simulationSetDescriptor
  yCaption <- NULL
  if (isOfType(structureSet$simulationSet, "PopulationSimulationSet") &
    isIncluded(plotType, c("obsVsPred", "resVsPred"))) {
    gofStatistics <- settings$getStatistics()
    yCaption <- gofStatistics$yCaption
    plotCaption <- captions$plotGoF[[plotType]](simulationSetName, setDescriptor, dataSource, plotScale, yCaption)
    return(plotCaption)
  }
  plotCaption <- captions$plotGoF[[plotType]](simulationSetName, setDescriptor, dataSource, plotScale)
  return(plotCaption)
}

# Description needs to adapt to number of sets and descriptor
reportSimulationSet <- function(simulationSetNames, descriptor) {
  # For multiple sets, concatenate with ", " and add "s" to descriptor
  if (length(simulationSetNames) > 1) {
    simulationSetNames <- paste0(simulationSetNames, collapse = ", ")
    if (descriptor != "") {
      descriptor <- paste0(descriptor, "s")
    }
  }
  if (descriptor == "") {
    return(simulationSetNames)
  }
  return(paste0(descriptor, " ", simulationSetNames))
}

reportUnit <- function(displayUnit) {
  if (isOfLength(displayUnit, 0)) {
    return("")
  }
  if (displayUnit == "") {
    return("")
  }
  return(paste0(" reported in [", displayUnit, "]"))
}

# For the reported tables, first column is simulation set names
# Consequently, the descriptor should be used
# However, expression "" is not accepted and needs to be replaced by "Population"
translateDescriptor <- function(descriptor) {
  if (descriptor == "") {
    descriptor <- "Population"
  }
  return(descriptor)
}

formatPKParameterHeader <- function(data, descriptor) {
  names(data) <- c(
    translateDescriptor(descriptor),
    reEnv$defaultPKParametersHeader
  )
  return(data)
}

getTimeRangeCaption <- function(timeRangeName, reference, simulationSetName) {
  if (isIncluded(timeRangeName, ApplicationRanges$total)) {
    return(paste(
      "### For total simulation time range",
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "total"))
    ))
  }
  if (isIncluded(timeRangeName, ApplicationRanges$firstApplication)) {
    return(paste(
      "### For first application range",
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "first"))
    ))
  }
  if (isIncluded(timeRangeName, ApplicationRanges$lastApplication)) {
    return(paste(
      "### For last application range",
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "last"))
    ))
  }
}

getBoxplotDescriptor <- function(percentiles = c(5, 25, 50, 75, 95), ratio = FALSE) {
  paste(
    "shown as box-whisker plot, which indicates", ifelse(ratio, "ratios of", ""), "the",
    paste0(percentiles, "<sup>th</sup>", collapse = ", "), "percentiles"
  )
}

getStatisticsCaption <- function(statistics) {
  if (isEmpty(statistics)) {
    return("")
  }
  return(paste0("(", statistics, ") "))
}

getPlotScaleCaption <- function(plotName, plotScale) {
  return(paste0(". ", plotName, " are plotted in a ", plotScale, " scale."))
}

getResidualsAcrossCaption <- function(pathName) {
  if (isEmpty(pathName)) {
    return("")
  }
  return(paste0(pathName, " across "))
}

getDDIPlotCaption <- function(title, subPlotCaption, pkParameter, plotTypeCaption) {
  longTitle <- ifNotNull(
    condition = subPlotCaption,
    outputIfNotNull = paste0(title, ". ", subPlotCaption, "."),
    outputIfNull = paste0(title, ". ")
  )
  caption <- paste(longTitle, plotTypeCaption, pkParameter, "Ratio.")
  return(caption)
}
