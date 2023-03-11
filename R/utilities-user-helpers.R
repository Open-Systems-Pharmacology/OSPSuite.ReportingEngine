#' @title getPKParametersInOutput
#' @param output Output object
#' @return Names of pkParameters in `output`
#' @export
#' @family workflow helpers
#' @examples
#' myOutput <- Output$new(
#'   path = "a",
#'   pkParameters = c("C_max", "AUC_tEnd")
#' )
#' getPKParametersInOutput(myOutput)
#'
getPKParametersInOutput <- function(output) {
  validateIsOfType(output, "Output")
  pkParameters <- sapply(output$pkParameters, function(pkParameterInfo) {
    pkParameterInfo$pkParameter
  })
  if (isEmpty(pkParameters)) {
    return(NA)
  }
  return(pkParameters)
}

#' @title getPKParameterGroupsInOutput
#' @param output Output object
#' @return Names of pkParameter groups in `output`
#' @export
#' @family workflow helpers
#' @examples
#' myOutput <- Output$new(
#'   path = "a",
#'   pkParameters = c("C_max", "AUC_tEnd")
#' )
#' getPKParametersInOutput(myOutput)
#'
#' # If a group is defined
#' myGroupOutput <- Output$new(
#'   path = "a",
#'   pkParameters = c(
#'     C_max = PkParameterInfo$new("C_max", group = 1),
#'     AUC_tEnd = PkParameterInfo$new("AUC_tEnd", group = 2)
#'   )
#' )
#' getPKParametersInOutput(myGroupOutput)
#'
getPKParameterGroupsInOutput <- function(output) {
  validateIsOfType(output, "Output")
  pkParameters <- sapply(output$pkParameters, function(pkParameterInfo) {
    pkParameterInfo$group
  })
  if (isEmpty(pkParameters)) {
    return(NA)
  }
  return(pkParameters)
}

#' @title getSimulationParameterDisplayPaths
#' @param parameterPaths Paths of a parameter in simulation
#' @param simulation A `Simulation` object from `ospsuite`
#' @param dictionary A data.frame mapping `parameterPaths` to `parameterDisplayPaths`
#' @return An array of `parameterDisplayPaths` matching the `parameterPaths`
#' @export
#' @family workflow helpers
getSimulationParameterDisplayPaths <- function(parameterPaths, simulation, dictionary) {
  parameterDisplayPaths <- ospsuite::getParameterDisplayPaths(parameterPaths, simulation)
  for (parameterIndex in seq_along(parameterPaths)) {
    # Get the index of first found parameter in dictionary if defined
    dictionaryIndex <- head(which(parameterPaths[parameterIndex] %in% dictionary$parameter), 1)
    if (!isEmpty(dictionaryIndex)) {
      # User should already get a warning/error from ospsuite if a parameter path is not defined
      # as.character enforces character is used instead of levels
      parameterDisplayPaths[parameterIndex] <- as.character(dictionary$displayPath[dictionaryIndex])
    }
  }
  return(parameterDisplayPaths)
}

#' @title getPopulationPKData
#' @description Get population PK data merging demography and PK data of a population
#' @param population A `Population` object from the `{ospsuite}` package
#' @param simulation A `Simulation` object from the `{ospsuite}` package
#' @return A data.frame
#' @export
#' @family workflow helpers
getPopulationPKData <- function(population, simulation) {
  populationTable <- ospsuite::populationToDataFrame(population)
  allParameters <- ospsuite::getAllParametersMatching(population$allParameterPaths, simulation)
  for (parameter in allParameters) {
    populationTable[, parameter$path] <- ospsuite::toDisplayUnit(parameter, populationTable[, parameter$path])
  }
  return(populationTable)
}

#' @title getPopulationPKMetaData
#' @description Get population PK meta data merging demography and PK meta data of a population
#' @param population A `Population` object from the `{ospsuite}` package
#' @param simulation A `Simulation` object from the `{ospsuite}` package
#' @param parameterDisplayPaths A data.frame mapping parameter paths to their display paths
#' @return Array of list for each demography and PK parameter that includes `dimension`, `unit` and `class`
#' @export
#' @family workflow helpers
getPopulationPKMetaData <- function(population, simulation, parameterDisplayPaths) {
  metaData <- list()
  allParameters <- ospsuite::getAllParametersMatching(population$allParameterPaths, simulation)
  for (covariate in population$allCovariateNames) {
    metaData[[covariate]] <- list(
      dimension = covariate,
      unit = "",
      class = class(population$getCovariateValues(covariate))
    )
  }
  for (parameter in allParameters) {
    metaData[[parameter$path]] <- list(
      dimension = getSimulationParameterDisplayPaths(parameter$path, simulation, parameterDisplayPaths),
      unit = parameter$displayUnit,
      class = class(population$getParameterValues(parameter$path))
    )
  }
  return(metaData)
}
