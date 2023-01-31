#' @title repeatableSample
#' @description Perform repeatable random sampling
#' @param x Array to be sampled
#' @param size Number of samples
#' @param n Number of repetitions
#' @param seed Random Seed
#' @return A list of `n` arrays of length `size` sampled from `x`
#' @keywords internal
repeatableSampling <- function(x,
                               size,
                               n = getDefaultMCRepetitions(),
                               seed = getDefaultMCRandomSeed()) {
  # .Random.seed is created when 
  # calling a random number generator for the first time 
  # The next line aims at ensuring that a .Random.seed object exists
  createRandom <- runif(1)
  # Use pre-defined seed to get repeatable results
  oldSeed <- .Random.seed
  on.exit({
    .Random.seed <<- oldSeed
  })
  set.seed(seed)
  # Return a matrix of sampled PK parameters
  if (isOfType(x, "data.frame")) {
    return(lapply(1:n, function(repetition) {
      x[sample(x = 1:nrow(x), size = size, replace = FALSE), ]
    }))
  }
  return(lapply(1:n, function(repetition) {
    sample(x = x, size = size, replace = FALSE)
  }))
}

#' @title ratioBoxplot
#' @description Plot box-whiskers of ratios as is
#' @param data data.frame of the ratios
#' @param plotConfiguration PlotConfiguration R6 class object
#' @return ggplot object
#' @export
#' @import tlf
#' @import ggplot2
ratioBoxplot <- function(data,
                         plotConfiguration = NULL) {
  # TODO: create new molecule plot for this
  ratioPlot <- tlf::initializePlot(plotConfiguration)
  aestheticValues <- tlf:::.getAestheticValuesFromConfiguration(
    n = 1,
    position = 0,
    plotConfigurationProperty = ratioPlot$plotConfiguration$ribbons,
    propertyNames = c("size", "alpha", "fill")
  )

  ratioPlot <- ratioPlot +
    ggplot2::geom_boxplot(
      data = data,
      mapping = ggplot2::aes_string(
        x = "simulationSetName",
        ymin = "ymin",
        lower = "lower",
        middle = "middle",
        upper = "upper",
        ymax = "ymax"
      ),
      stat = "identity",
      fill = aestheticValues$fill,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    )
  ratioPlot <- tlf:::.updateAxes(ratioPlot)
  return(ratioPlot)
}

#' @title pkParameterTableAsRelativeChange
#' @description Get Measure Table for Parallel Population Workflow
#' @param pkParametersTable PK Parameter Measure summarizing key statistics for each population
#' @param referenceSimulationSetName Reference
#' @keywords internal
pkParameterTableAsRelativeChange <- function(pkParametersTable,
                                             referenceSimulationSetName) {
  simulationSetNames <- pkParametersTable$Population
  pkRatiosTable <- pkParametersTable[simulationSetNames != referenceSimulationSetName, ]
  referencePkParametersTable <- pkParametersTable[rep(referenceSimulationSetName, length(pkRatiosTable$Population)), ]
  # Columns 1 and 2 are Population and size
  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]
  return(pkRatiosTable)
}

#' @title getPKParameterRatioPKTable
#' @description Get Data for Measure Table and Plots of PK Ratios
#' @param data A data.frame of PK Parameter values for across Populations
#' @param dataMapping A `BoxWhiskerDataMapping` object
#' @param structureSets `SimulationStructure` R6 class object
#' @param referenceSimulationSetName Name of reference simulation set
#' @param settings A list of task settings
#' @keywords internal
getPKParameterRatioPKTable <- function(data,
                                       dataMapping,
                                       structureSets,
                                       referenceSimulationSetName,
                                       settings = NULL) {
  populationSets <- isSamePopulationInSets(structureSets)
  # Pair each structure set with reference
  # If reference population = comp population, individual ratios
  # Else Monte Carlo Simulation
  referenceData <- data[data$simulationSetName %in% referenceSimulationSetName, ]
  ratioPKTable <- data.frame()
  for (set in populationSets) {
    comparisonData <- data[data$simulationSetName %in% set$simulationSetName, ]
    if (set$isSamePopulation) {
      logDebug(paste0(
        "Simulation Set '", set$simulationSetName,
        "' was identified with same population as reference '",
        referenceSimulationSetName, "'. ",
        "Ratio comparison analyzed statistics of individual PK ratios."
      ))
      ratioData <- comparisonData
      ratioData$Value <- comparisonData$Value / referenceData$Value
      ratioPKMeasure <- getPKParameterMeasure(ratioData, dataMapping)
      ratioPKTable <- rbind.data.frame(ratioPKTable, ratioPKMeasure)
      next
    }
    logDebug(paste0(
      "Simulation Set '", set$simulationSetName,
      "' was identified with population different from reference '",
      referenceSimulationSetName, "'. ",
      "Ratio comparison analyzed statistics from Monte Carlo Sampling"
    ))
    ratioPKMeasure <- getPKParameterRatioMeasureFromMCSampling(
      comparisonData,
      referenceData,
      dataMapping,
      settings
    )
    ratioPKTable <- rbind.data.frame(ratioPKTable, ratioPKMeasure)
  }
  return(ratioPKTable)
}

#' @title getPKParameterRatioMeasureFromMCSampling
#' @description Get PK Parameter Ratio Measure From Monte Carlo Sampling
#' @param comparisonData A data.frame of PK Parameter values for Population to compare
#' @param referenceData A data.frame of PK Parameter values for reference Population
#' @param dataMapping A `BoxWhiskerDataMapping` object
#' @param settings A list of task settings
#' @return A data.frame
#' @keywords internal
getPKParameterRatioMeasureFromMCSampling <- function(comparisonData,
                                                     referenceData,
                                                     dataMapping,
                                                     settings = NULL) {
  # Sample from largest population if size is different
  # Create a list of Sampled PK Parameters for each MC repetition and calculate their Ratio
  if (nrow(comparisonData) < nrow(referenceData)) {
    allSamplesReferenceData <- repeatableSampling(
      # Keep columns at minimum to prevent slowing down computation
      x = referenceData[, c("simulationSetName", "Value")],
      size = nrow(comparisonData),
      n = settings$mcRepetitions %||% getDefaultMCRepetitions(),
      seed = settings$mcRandomSeed %||% getDefaultMCRandomSeed()
    )
    allSamplesRatioData <- lapply(
      allSamplesReferenceData,
      function(sampleReferenceData) {
        ratioData <- comparisonData[, c("simulationSetName", "Value")]
        ratioData$Value <- comparisonData$Value / sampleReferenceData$Value
        return(ratioData)
      }
    )
  } else {
    # Keep columns at minimum to prevent slowing down computation
    allSamplesComparisonData <- repeatableSampling(
      x = comparisonData[, c("simulationSetName", "Value")],
      size = nrow(referenceData),
      n = settings$mcRepetitions %||% getDefaultMCRepetitions(),
      seed = settings$mcRandomSeed %||% getDefaultMCRandomSeed()
    )
    allSamplesRatioData <- lapply(
      allSamplesComparisonData,
      function(sampleComparisonData) {
        ratioData <- sampleComparisonData
        ratioData$Value <- sampleComparisonData$Value / referenceData$Value
        return(ratioData)
      }
    )
  }

  # Get tables of ratio summary statistics as list
  allSamplesRatioMeasure <- lapply(
    allSamplesRatioData,
    function(data) {
      getPKParameterMeasure(dataMapping$checkMapData(data), dataMapping)
    }
  )
  # Transform list to table
  allSamplesRatioMeasure <- do.call("rbind", allSamplesRatioMeasure)
  # Get median statistics over all MC repetitions as a data.frame
  medianPKRatioStatistics <- aggregate(
    allSamplesRatioMeasure[, 2:ncol(allSamplesRatioMeasure)],
    by = list(Population = allSamplesRatioMeasure$Population),
    FUN = median
  )
  # analyticalSolution <- getRatioMeasureAnalyticalSolution(referenceData, comparisonData)
  logDebug(messages$monteCarloChecking(
    displayRatioMeasureAnalyticalSolution(referenceData, comparisonData),
    medianPKRatioStatistics,
    n = settings$mcRepetitions %||% getDefaultMCRepetitions(),
    seed = settings$mcRandomSeed %||% getDefaultMCRandomSeed()
  ))
  return(medianPKRatioStatistics)
}

#' @title isSamePopulationInSets
#' @description Check wither simulation sets use same population as reference set
#' @param structureSets A `SimulationStructure`
#' @return list
isSamePopulationInSets <- function(structureSets) {
  isReference <- sapply(structureSets, function(set) {
    set$simulationSet$referencePopulation
  })
  referenceSet <- structureSets[isReference]
  populationInSets <- lapply(
    structureSets[!isReference],
    function(set) {
      if (set$simulationSet$referencePopulation) {
        return()
      }
      # Same population is identified as
      # 1- same population file
      # 2- same study design file (if defined)
      isSamePopulation <- all(
        set$simulationSet$populationFile %in% referenceSet$simulationSet$populationFile,
        any(
          set$simulationSet$populationFile %in% referenceSet$simulationSet$populationFile,
          all(
            isEmpty(set$simulationSet$studyDesignFile),
            isEmpty(referenceSet$simulationSet$studyDesignFile)
          )
        )
      )
      return(list(
        simulationSetName = set$simulationSet$simulationSetName,
        isSamePopulation = isSamePopulation
      ))
    }
  )
  return(populationInSets)
}

#' @title getRatioMeasureAnalyticalSolution
#' @description Get Ratio Measure Analytical Solution
#' @param y Array of PK parameter numeric values
#' @param x Array of PK parameter numeric values
#' @return A list including geoMean, geoSD and geoCV
#' @keywords internal
getRatioMeasureAnalyticalSolution <- function(x, y) {
  list(
    geoMean = exp(mean(log(y)) - mean(log(x))),
    geoSD = exp(sqrt((sd(log(y)))^2 + (sd(log(x)))^2)),
    geoCV = sqrt(exp((sd(log(y)))^2 + (sd(log(x)))^2) - 1)
  )
}

#' @title displayRatioMeasureAnalyticalSolution
#' @description Display Ratio Measure Analytical Solution message
#' @param comparisonData A data.frame of PK Parameter values for Population to compare
#' @param referenceData A data.frame of PK Parameter values for reference Population
#' @return A character
#' @keywords internal
displayRatioMeasureAnalyticalSolution <- function(referenceData, comparisonData) {
  analyticalSolution <- getRatioMeasureAnalyticalSolution(
    referenceData$Value,
    comparisonData$Value
  )
  checkingMessage <- c(
    paste0(
      "Analysis of PK Ratios between '",
      unique(comparisonData$simulationSetName), "' (n=", nrow(comparisonData), ")",
      " against reference '",
      unique(referenceData$simulationSetName), "' (n=", nrow(referenceData), ")"
    ),
    paste0(
      "Analytical solution for ", paste0(names(analyticalSolution), collapse = ", "),
      " resulted in ", paste0(unlist(analyticalSolution), collapse = ", "),
      " respectively"
    )
  )
  return(checkingMessage)
}
