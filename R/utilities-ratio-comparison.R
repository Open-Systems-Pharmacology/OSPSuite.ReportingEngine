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
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = structureSet$pkAnalysisResultsFileNames,
    simulation = ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  )
  referencePKAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = referenceSet$pkAnalysisResultsFileNames,
    simulation = ospsuite::loadSimulation(referenceSet$simulationSet$simulationFile)
  )
  # Use arrange to ensure Ids are consistent between PK parameters and output paths
  pkData <- ospsuite::pkAnalysesToTibble(pkAnalyses) %>% arrange(IndividualId)
  referencePKData <- ospsuite::pkAnalysesToTibble(referencePKAnalyses) %>% arrange(IndividualId)

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
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = structureSet$pkAnalysisResultsFileNames,
    simulation = ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  )
  referencePKAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = referenceSet$pkAnalysisResultsFileNames,
    simulation = ospsuite::loadSimulation(referenceSet$simulationSet$simulationFile)
  )
  pkData <- ospsuite::pkAnalysesToTibble(pkAnalyses)
  referencePKData <- ospsuite::pkAnalysesToTibble(referencePKAnalyses)
  # Check that both PK data to be compared are included in reference PK data
  validateIsIncluded(unique(pkData$QuantityPath), unique(referencePKData$QuantityPath))
  validateIsIncluded(unique(pkData$Parameter), unique(referencePKData$Parameter))

  pkRatioSummary <- getPKRatioSummaryStatistics(
    pkData = pkData,
    referencePKData = referencePKData,
    simulationSetName = structureSet$simulationSet$simulationSetName
  )

  return(pkRatioSummary)
}

#' @title getPKRatioSummaryStatistics
#' @description
#' Calculate and save summary statistics of ratios of PK parameters
#' @param pkData A data.frame of PK Parameter values for Population to compare
#' @param referencePKData A data.frame of PK Parameter values for reference Population
#' @param simulationSetName Name of simulation set
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
#' @import dplyr
getPKRatioSummaryStatistics <- function(pkData, referencePKData, simulationSetName) {
  # Use left join to include only comparable data
  # in case reference includes additional parameters or paths
  ratioData <- left_join(
    pkData,
    referencePKData,
    by = c("IndividualId", "QuantityPath", "Parameter"),
    suffix = c("", "Ref")
  ) %>% mutate(Ratio = Value / ValueRef)

  # Data summary is applied to subgroups of QuantityPath and Parameter
  ratioSummary <- ratioData[, c("QuantityPath", "Parameter", "Ratio")] %>%
    summarise(
      N = n(),
      Perc5 = quantile(Ratio, probs = 0.05, na.rm = TRUE),
      Perc25 = quantile(Ratio, probs = 0.25, na.rm = TRUE),
      Perc50 = quantile(Ratio, probs = 0.5, na.rm = TRUE),
      Perc75 = quantile(Ratio, probs = 0.75, na.rm = TRUE),
      Perc95 = quantile(Ratio, probs = 0.95, na.rm = TRUE),
      Mean = mean(Ratio, na.rm = TRUE),
      SD = sd(Ratio, na.rm = TRUE),
      GeoMean = exp(mean(log(Ratio), na.rm = TRUE)),
      GeoSD = exp(sd(log(Ratio), na.rm = TRUE)),
      .by = c("QuantityPath", "Parameter")
    ) %>%
    mutate(SimulationSetName = simulationSetName, .before = everything())

  return(as.data.frame(ratioSummary))
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
  createRandom <- runif(1)
  # Use pre-defined seed to get repeatable results
  oldSeed <- .Random.seed
  on.exit({
    .Random.seed <<- oldSeed
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
#' @param pkData A data.frame of PK Parameter values for Population to compare
#' @param referencePKData A data.frame of PK Parameter values for reference Population
#' @param settings A list of task settings
#' @return A data.frame of the PK Parameter ratios summary statistics
#' @keywords internal
getPKRatioSummaryFromSettings <- function(pkData, referencePKData, settings) {
  # Sample from largest population if size is different
  # Create a list of Sampled PK Parameters for each MC repetition and calculate their Ratio
  pkSize <- length(unique(pkData$IndividualId))
  referenceSize <- length(unique(referencePKData$IndividualId))
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
    invisible(parallel::clusterEvalQ(cl, library(dplyr)))
    invisible(parallel::clusterExport(cl, c(
      "selectedIds",
      "pkSize",
      "referenceSize",
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
            pkData = pkData %>%
              mutate(IndividualId = ave(
                1:pkSize,
                pkData$QuantityPath,
                pkData$Parameter,
                FUN = identity
              )),
            referencePKData = referencePKData %>%
              slice(selectedIds[[repetition]], .by = c("QuantityPath", "Parameter")) %>%
              mutate(IndividualId = ave(1:pkSize, QuantityPath, Parameter, FUN = identity)),
            simulationSetName = repetition
          )
        }
      ),
      "comparison" = parallel::parLapply(
        cl = cl,
        seq_along(selectedIds),
        function(repetition) {
          getParallelSummaryStatistics(
            pkData %>%
              slice(selectedIds[[repetition]], .by = c("QuantityPath", "Parameter")) %>%
              mutate(IndividualId = ave(1:referenceSize, QuantityPath, Parameter, FUN = identity)),
            referencePKData = referencePKData %>%
              mutate(IndividualId = ave(
                1:referenceSize,
                referencePKData$QuantityPath,
                referencePKData$Parameter,
                FUN = identity
              )),
            simulationSetName = repetition
          )
        }
      )
    )
    mcRatioData <- do.call("rbind", listOfMCRatioData)
    return(mcRatioData)
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
        # data.frame is arranged by increasing values of IndividualId
        # since group_by is slow, ave is used instead
        getPKRatioSummaryStatistics(
          pkData = pkData %>%
            mutate(IndividualId = ave(
              1:pkSize,
              pkData$QuantityPath,
              pkData$Parameter,
              FUN = identity
            )),
          referencePKData = referencePKData %>%
            slice(selectedIds[[repetition]], .by = c("QuantityPath", "Parameter")) %>%
            mutate(IndividualId = ave(1:pkSize, QuantityPath, Parameter, FUN = identity)),
          simulationSetName = repetition
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
          pkData %>%
            slice(selectedIds[[repetition]], .by = c("QuantityPath", "Parameter")) %>%
            mutate(IndividualId = ave(1:referenceSize, QuantityPath, Parameter, FUN = identity)),
          referencePKData = referencePKData %>%
            mutate(IndividualId = ave(
              1:referenceSize,
              referencePKData$QuantityPath,
              referencePKData$Parameter,
              FUN = identity
            )),
          simulationSetName = repetition
        )
      }
    )
  )
  mcRatioData <- do.call("rbind", listOfMCRatioData)
  return(mcRatioData)
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
  mcRatioData <- getPKRatioSummaryFromSettings(
    pkData = pkData,
    referencePKData = referencePKData,
    settings = settings
  )
  logInfo(messages$runCompleted(getElapsedTime(t0), "Monte Carlo Sampling"))

  # Get median statistics over all MC repetitions as a data.frame
  mcRatioSummary <- mcRatioData %>%
    group_by(QuantityPath, Parameter) %>%
    summarise_all(median, .group = "drop_last") %>%
    mutate(SimulationSetName = simulationSetName, .before = everything())

  return(mcRatioSummary)
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
