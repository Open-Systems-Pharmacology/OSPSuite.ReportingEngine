#' @title calculatePKAnalysesRatio
#' @description
#' Calculate and save statistics of ratios of PK parameters for each simulation set but reference
#' If simulation sets use the same population, calculate individual ratios and
#' get summary statistics from these individual ratios.
#' If simulation sets use different populations, perform Monte Carlo Sampling,
#' calculate individual ratios for each repetition of the Monte Carlo Sampling,
#' get summary statistics from these individual ratios for each repetition, and
#' get the median of the summary statistics as best approximation of the ratio summary statistics.
#' @keywords internal
calculatePKAnalysesRatio <- function(structureSets, settings) {
  isReference <- sapply(structureSets, function(set) {
    set$simulationSet$referencePopulation
  })
  referenceSet <- structureSets[isReference][[1]]

  for (set in structureSets[!isReference]) {
    isSamePopulation <- checkIsSamePopulation(set$simulationSet, referenceSet$simulationSet)
    logInfo(messages$ratioIdentifiedPopulations(
      simulationSetName = set$simulationSet$simulationSetName,
      referenceSimulationSetName = referenceSet$simulationSet$simulationSetName,
      isSamePopulation = isSamePopulation
    ))

    if (isSamePopulation) {
      pkRatioSummary <- getPKRatioSummaryForSamePopulation(
        structureSet = set,
        referenceSet = referenceSet
      )
      write.csv(
        pkRatioSummary,
        file = set$pkRatioResultsFileNames,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
      next
    }
    pkRatioSummaries <- getPKRatioSummaryForDifferentPopulations(
      structureSet = set,
      referenceSet = referenceSet,
      settings = settings
    )
    # Save Monte Carlo solution
    write.csv(
      pkRatioSummaries$monteCarlo,
      file = set$pkRatioResultsFileNames,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    # Save analytical solution in case debugging is required
    write.csv(
      pkRatioSummaries$analyticalSolution,
      file = gsub(set$pkRatioResultsFileNames, pattern = ".csv", replacement = "AnalyticalSolutions.csv"),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
  }

  return(invisible())
}

#' @title getPKRatioSummaryForDifferentPopulations
#' @description
#' Calculate and save summary statistics of ratios of PK parameters if populations are identical
#' @param structureSet A `SimulationStructure` object of the population to compare
#' @param referenceSet A `SimulationStructure` object of the reference population
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
getPKRatioSummaryForDifferentPopulations <- function(structureSet, referenceSet, settings) {
  # Use arrange to ensure Ids, QuantityPath and Parameter are consistent between PK parameters and output paths
  pkData <- loadPKAnalysesFromStructureSet(structureSet = structureSet, to = "tibble") %>%
    select(IndividualId, QuantityPath, Parameter, Value) %>%
    arrange(IndividualId, QuantityPath, Parameter)
  referencePKData <- loadPKAnalysesFromStructureSet(structureSet = referenceSet, to = "tibble") %>%
    select(IndividualId, QuantityPath, Parameter, Value) %>%
    arrange(IndividualId, QuantityPath, Parameter)

  # Check that both PK data to be compared are included in reference PK data
  validateIsIncluded(unique(pkData$QuantityPath), unique(referencePKData$QuantityPath))
  validateIsIncluded(unique(pkData$Parameter), unique(referencePKData$Parameter))

  pkRatioSummary <- getPKRatioSummaryFromMCSampling(
    pkData = pkData,
    referencePKData = referencePKData,
    simulationSetName = structureSet$simulationSet$simulationSetName,
    settings = settings
  )

  pkRatioSummaryAnalyticalSolution <- getPKRatioSummaryFromAnalyticalSolution(
    pkData = pkData,
    referencePKData = referencePKData,
    simulationSetName = structureSet$simulationSet$simulationSetName
  )

  return(list(
    monteCarlo = pkRatioSummary,
    analyticalSolution = pkRatioSummaryAnalyticalSolution
  ))
}

#' @title getPKRatioSummaryForSamePopulation
#' @description
#' Calculate and save summary statistics of ratios of PK parameters if populations are identical
#' @param structureSet A `SimulationStructure` object of the population to compare
#' @param referenceSet A `SimulationStructure` object of the reference population
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
getPKRatioSummaryForSamePopulation <- function(structureSet, referenceSet) {
  pkData <- loadPKAnalysesFromStructureSet(structureSet = structureSet, to = "tibble") %>%
    select(IndividualId, QuantityPath, Parameter, Value) %>%
    arrange(IndividualId, QuantityPath, Parameter)
  referencePKData <- loadPKAnalysesFromStructureSet(structureSet = referenceSet, to = "tibble") %>%
    select(IndividualId, QuantityPath, Parameter, Value) %>%
    arrange(IndividualId, QuantityPath, Parameter)
  # Check that both PK data to be compared are included in reference PK data
  validateIsIncluded(unique(pkData$QuantityPath), unique(referencePKData$QuantityPath))
  validateIsIncluded(unique(pkData$Parameter), unique(referencePKData$Parameter))
  # Check that same individuals are present in both data sets and use intersection
  checkSamePopulationIds(
    setIds = pkData$IndividualId,
    referenceSetIds = referencePKData$IndividualId,
    setName = structureSet$simulationSet$simulationSetName,
    referenceSetName = referenceSet$simulationSet$simulationSetName
  )
  ids <- intersect(pkData$IndividualId, referencePKData$IndividualId)
  pkData <- pkData %>% filter(IndividualId %in% ids)
  referencePKData <- referencePKData %>% filter(IndividualId %in% ids)

  # Pivot table to get fast computation of ratios and their statistics
  quantityPaths <- unique(pkData$QuantityPath)
  parameters <- unique(pkData$Parameter)

  pkRatioSummaryGroups <- data.frame(
    SimulationSetName = structureSet$simulationSet$simulationSetName,
    QuantityPath = rep(quantityPaths, each = length(parameters)),
    Parameter = rep(parameters, length(quantityPaths))
  )

  pkData <- pkData %>%
    tidyr::pivot_wider(
      names_from = c(QuantityPath, Parameter),
      values_from = Value
    )
  referencePKData <- referencePKData %>%
    tidyr::pivot_wider(
      names_from = c(QuantityPath, Parameter),
      values_from = Value
    )

  pkRatioSummary <- getPKRatioSummaryStatistics(
    pkData = as.matrix(pkData),
    # In case reference data had more paths and parameters, restrict to those in pkData
    referencePKData = as.matrix(referencePKData[, names(referencePKData) %in% names(pkData)])
  )
  # Combine stats to their parameters, paths and simulation set name
  pkRatioSummary <- bind_cols(
    pkRatioSummaryGroups,
    # First row of summary statistics is individual ID
    # which needs to be removed to match both data.frame rows
    tail(summaryStatisticsToDataFrame(pkRatioSummary), -1)
  )

  return(pkRatioSummary)
}

#' @title getPKRatioSummaryStatistics
#' @description
#' Calculate and save summary statistics of ratios of PK parameters
#' Note that this function computes on matrix objects to be faster
#' than on data.frame when Monte Carlo simulation is performed
#' @param pkData A matrix of PK Parameter values for Population to compare
#' @param referencePKData A matrix of PK Parameter values for reference Population
#' @return A matrix of the PK Parameter ratios summary statistics
#' @keywords internal
#' @importFrom stats quantile sd
getPKRatioSummaryStatistics <- function(pkData, referencePKData) {
  # Since data.frame objects were pivoted to get matrix objects
  # Rows contained unique individuals while
  # columns contained all combinations of QuantityPath and Parameter
  # Since the object is a matrix of numeric values, calculation is vectorized
  ratioData <- pkData / referencePKData
  ratioSummary <- rbind(
    # Apply with MARGIN = 2 means that calculation is performed by columns
    N = apply(ratioData, 2, FUN = function(x) {
      sum(!is.na(x))
    }),
    apply(ratioData, 2, FUN = function(x) {
      quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    }),
    Mean = apply(ratioData, 2, FUN = function(x) {
      mean(x, na.rm = TRUE)
    }),
    SD = apply(ratioData, 2, FUN = function(x) {
      sd(x, na.rm = TRUE)
    }),
    GeoMean = apply(ratioData, 2, FUN = function(x) {
      exp(mean(log(x), na.rm = TRUE))
    }),
    GeoSD = apply(ratioData, 2, FUN = function(x) {
      exp(sd(log(x), na.rm = TRUE))
    })
  )
  return(ratioSummary)
}

#' @title summaryStatisticsToDataFrame
#' @description
#' Translate matrix of summary statistics to data.frame with appropriate names
#' @param data A matrix of summary statistics
#' @return A data.frame displaying summary statistics by column
#' @keywords internal
summaryStatisticsToDataFrame <- function(data, monteCarlo = FALSE) {
  # For same population, summary statistics is by row and needs to be transposed
  # For different populations, summary statistics is directly by column (because of sapply usage)
  if (!monteCarlo) {
    data <- t(data)
  }
  data <- as.data.frame(data)
  names(data) <- c("N", paste0("Perc", c(5, 25, 50, 75, 95)), "Mean", "SD", "GeoMean", "GeoSD")
  return(data)
}

#' @title getMonteCarloMedians
#' @description
#' Get all median values from a list of Monte Carlo simulations
#' @param listOfData A list of summary statistics matrices
#' @return A data.frame displaying summary statistics by column
#' @keywords internal
#' @importFrom stats median
getMonteCarloMedians <- function(listOfData) {
  # Translate list of matrix objects into a single matrix object
  # The matrix object has 10 rows of summary statistics,
  # repeated as many times as the number of Monte Carlo simulations
  data <- do.call("rbind", listOfData)
  medianData <- sapply(
    1:10,
    function(statsIndex) {
      selectedRows <- statsIndex + seq(0, nrow(data) - 10, 10)
      # Get median of each column per selected stats
      apply(data[selectedRows, ], 2, median)
    }
  )
  medianData <- summaryStatisticsToDataFrame(
    # First row is summary stat for individual ids and needs to be removed
    tail(medianData, -1),
    monteCarlo = TRUE
  )
  return(medianData)
}

#' @title mcSampling
#' @description Perform repeatable Monte Carlo random sampling
#' for a data.frame obtained from a `PKAnalyses` object
#' @param dataSize Number of rows/unique individuals in `data`
#' @param sampleSize Number of sampled individuals in each Monte Carlo repetition
#' @param n Number of repetitions
#' @param seed Random Seed Number in order to get repeatable results
#' @return A list of `n` elements that include `sampleSize` integers sampled from `1:dataSize`
#' @keywords internal
#' @import dplyr
mcSampling <- function(dataSize, sampleSize, n = getDefaultMCRepetitions(), seed = getDefaultMCRandomSeed()) {
  # .Random.seed is created when
  # calling a random number generator for the first time
  # The next line aims at ensuring that a .Random.seed object exists
  invisible(stats::runif(1))
  # Use pre-defined seed to get repeatable results
  oldSeed <- .Random.seed
  # Using assign based on https://github.com/r-lib/lintr/issues/787 advice
  on.exit({
    assign(".Random.seed", oldSeed, envir = parent.frame())
  })
  set.seed(seed)

  # Return a matrix of sampled PK parameters
  selectedIds <- lapply(1:n, function(repetition) {
    sample(x = 1:dataSize, size = sampleSize, replace = FALSE)
  })
  return(selectedIds)
}

#' @title getPKRatioSummaryFromAnalyticalSolution
#' @description Get PK Ratio Summary from Analytical Solution
#' @param pkData A data.frame of PK Parameter values for the Population to compare
#' @param referencePKData A data.frame of PK Parameter values for reference Population
#' @param simulationSetName Name of simulation set
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
#' @importFrom stats sd
#' @import dplyr
getPKRatioSummaryFromAnalyticalSolution <- function(pkData, referencePKData, simulationSetName) {
  pkSummary <- pkData %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      MeanLog = mean(log(Value), na.rm = TRUE),
      SDLog = sd(log(Value), na.rm = TRUE),
      .by = c("QuantityPath", "Parameter")
    )
  referencePKSummary <- referencePKData %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      MeanLog = mean(log(Value), na.rm = TRUE),
      SDLog = sd(log(Value), na.rm = TRUE),
      .by = c("QuantityPath", "Parameter")
    )
  # Merge data summaries
  ratioSummary <- left_join(
    as.data.frame(pkSummary),
    as.data.frame(referencePKSummary),
    by = c("QuantityPath", "Parameter"),
    suffix = c("X", "Y")
  ) %>%
    mutate(
      SimulationSetName = simulationSetName,
      Mean = MeanX / MeanY,
      SD = sqrt((SDX / MeanY)^2 + (MeanX * SDY / (MeanY * MeanY))^2),
      GeoMean = exp(MeanLogX - MeanLogY),
      GeoSD = exp(sqrt(SDLogX^2 + SDLogY^2))
    ) %>%
    select(SimulationSetName, QuantityPath, Parameter, Mean, SD, GeoMean, GeoSD)

  return(as.data.frame(ratioSummary))
}


#' @title getPKRatioSummaryFromSettings
#' @description Get PK Parameter Ratio Measure From Monte Carlo Sampling
#' @param pkData A matrix of PK Parameter values for Population to compare
#' @param referencePKData A matrix of PK Parameter values for reference Population
#' @param settings A list of task settings
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
getPKRatioSummaryFromSettings <- function(pkData, referencePKData, settings) {
  # Sample from largest population if size is different
  # Create a list of Sampled PK Parameters for each MC repetition and calculate their Ratio
  pkSize <- nrow(pkData)
  referenceSize <- nrow(referencePKData)
  mcRepetitions <- settings$mcRepetitions %||% getDefaultMCRepetitions()
  mcRandomSeed <- settings$mcRandomSeed %||% getDefaultMCRandomSeed()

  widerPopulation <- ifelse(pkSize < referenceSize, "reference", "comparison")

  selectedIds <- mcSampling(
    dataSize = switch(widerPopulation,
      "reference" = referenceSize,
      "comparison" = pkSize
    ),
    sampleSize = switch(widerPopulation,
      "reference" = pkSize,
      "comparison" = referenceSize
    ),
    n = mcRepetitions,
    seed = mcRandomSeed
  )
  useParallel <- all(
    requireNamespace("parallel", quietly = TRUE),
    settings$numberOfCores > 1
  )
  if (useParallel) {
    cl <- parallel::makeCluster(settings$numberOfCores)
    on.exit(parallel::stopCluster(cl))

    # Parallel code requires that some objects are exported and to use dplyr package
    # Re-import and rename function in this specific environment to be exported on clusters
    getParallelSummaryStatistics <- getPKRatioSummaryStatistics
    invisible(parallel::clusterExport(cl, c(
      "selectedIds",
      "pkData",
      "referencePKData",
      "getParallelSummaryStatistics"
    ),
    envir = environment()
    ))
    listOfMCRatioData <- switch(widerPopulation,
      "reference" = parallel::parLapply(
        cl = cl,
        seq_along(selectedIds),
        function(repetition) {
          getParallelSummaryStatistics(
            pkData = pkData,
            referencePKData = referencePKData[selectedIds[[repetition]], ]
          )
        }
      ),
      "comparison" = parallel::parLapply(
        cl = cl,
        seq_along(selectedIds),
        function(repetition) {
          getParallelSummaryStatistics(
            pkData = pkData[selectedIds[[repetition]], ],
            referencePKData = referencePKData
          )
        }
      )
    )
    return(getMonteCarloMedians(listOfMCRatioData))
  }

  if (settings$showProgress) {
    loadingProgress <- txtProgressBar(max = mcRepetitions, style = 3)
    on.exit(close(loadingProgress))
  }

  listOfMCRatioData <- switch(widerPopulation,
    "reference" = lapply(
      seq_along(selectedIds),
      function(repetition) {
        if (settings$showProgress) {
          setTxtProgressBar(loadingProgress, value = repetition)
        }
        # With pivoted matrix objects, there is no need to group_by rows correspond to unique individual ids
        # thus, smaller population can be used as is and wider only needs to be subset by simulated Monte Carlo rows
        getPKRatioSummaryStatistics(
          pkData = pkData,
          referencePKData = referencePKData[selectedIds[[repetition]], ]
        )
      }
    ),
    "comparison" = lapply(
      seq_along(selectedIds),
      function(repetition) {
        if (settings$showProgress) {
          setTxtProgressBar(loadingProgress, value = repetition)
        }
        # data.frame is arranged by increasing values of IndividualId
        getPKRatioSummaryStatistics(
          pkData = pkData[selectedIds[[repetition]], ],
          referencePKData = referencePKData
        )
      }
    )
  )
  return(getMonteCarloMedians(listOfMCRatioData))
}

#' @title getPKRatioSummaryFromMCSampling
#' @description Get PK Parameter Ratio Measure From Monte Carlo Sampling
#' @param pkData A data.frame of PK Parameter values for Population to compare
#' @param referencePKData A data.frame of PK Parameter values for reference Population
#' @param simulationSetName Name of simulation set
#' @param settings A list of task settings
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
getPKRatioSummaryFromMCSampling <- function(pkData, referencePKData, simulationSetName, settings = NULL) {
  t0 <- tic()
  mcRepetitions <- settings$mcRepetitions %||% getDefaultMCRepetitions()
  mcRandomSeed <- settings$mcRandomSeed %||% getDefaultMCRandomSeed()
  logInfo(messages$monteCarlo(simulationSetName, mcRepetitions, mcRandomSeed))

  # Pivot table to get fast computation of ratios and their statistics
  quantityPaths <- unique(pkData$QuantityPath)
  parameters <- unique(pkData$Parameter)

  pkRatioSummaryGroups <- data.frame(
    SimulationSetName = simulationSetName,
    QuantityPath = rep(quantityPaths, each = length(parameters)),
    Parameter = rep(parameters, length(quantityPaths))
  )

  pkData <- pkData %>%
    tidyr::pivot_wider(
      names_from = c(QuantityPath, Parameter),
      values_from = Value
    )
  referencePKData <- referencePKData %>%
    tidyr::pivot_wider(
      names_from = c(QuantityPath, Parameter),
      values_from = Value
    )

  pkRatioSummary <- getPKRatioSummaryFromSettings(
    pkData = as.matrix(pkData),
    referencePKData = as.matrix(referencePKData[, names(referencePKData) %in% names(pkData)]),
    settings = settings
  )
  pkRatioSummary <- bind_cols(pkRatioSummaryGroups, pkRatioSummary)
  logInfo(messages$runCompleted(getElapsedTime(t0), "Monte Carlo Sampling"))

  return(pkRatioSummary)
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
pkParameterTableAsRelativeChange <- function(pkParametersTable, referenceSimulationSetName) {
  simulationSetNames <- pkParametersTable$Population
  pkRatiosTable <- pkParametersTable[simulationSetNames != referenceSimulationSetName, ]
  referencePkParametersTable <- pkParametersTable[rep(referenceSimulationSetName, length(pkRatiosTable$Population)), ]
  # Columns 1 and 2 are Population and size
  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]
  return(pkRatiosTable)
}

#' @title getPKParameterRatioTable
#' @description Get Table of summary statistics of PK parameter ratios across simulation sets
#' @param pkParameter PK Parameter name
#' @param outputPath Quantity Path name
#' @param structureSets `SimulationStructure` R6 class object
#' @keywords internal
#' @import dplyr
getPKParameterRatioTable <- function(pkParameter,
                                     outputPath,
                                     structureSets) {
  isReference <- sapply(structureSets, function(set) {
    set$simulationSet$referencePopulation
  })
  ratioData <- data.frame()
  for (set in structureSets[!isReference]) {
    ratioData <- rbind.data.frame(
      ratioData,
      readObservedDataFile(set$pkRatioResultsFileNames) %>%
        filter(QuantityPath %in% outputPath, Parameter %in% pkParameter) %>%
        select(!c(QuantityPath, Parameter))
    )
  }
  return(ratioData)
}
