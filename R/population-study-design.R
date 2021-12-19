#' @title addStudyParameters
#' @param population `Population` object
#' @param simulation `Simulation` object
#' @param studyDesignFile file name of study design table
#' @export
#' @import ospsuite
addStudyParameters <- function(population, simulation, studyDesignFile) {
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsOfType(simulation, "Simulation")
  ospsuite.utils::validateIsString(studyDesignFile)

  studyDesign <- loadStudyDesign(studyDesignFile, population, simulation)

  initialTargetValues <- rep(NA, population$count)
  populationData <- ospsuite::populationAsDataFrame(population)

  for (target in studyDesign$targets) {
    parameterPath <- target$name

    updatedTargetValues <- updateTargetValues(initialTargetValues, target$values, studyDesign$source, populationData)

    population$setParameterValues(parameterPath, updatedTargetValues)
  }
}

#' @title loadStudyDesign
#' @description Load a StudyDesign object from a file containing a study design table
#' In this table, Line 1 is path,  Line 2 is unit, Line 3 is type and subsequent lines are values.
#' The cells for type can include "SOURCE" OR "TARGET". "SOURCE" type can also include subtype "MIN", "MAX" or "EQUALS".
#' In the current version, unit is expected to be intern units of simulation.
#' @param studyDesignFile file name of study design table
#' @param population `Population` object
#' @param simulation `Simulation` object
#' @return A `StudyDesign` object`
#' @export
loadStudyDesign <- function(studyDesignFile, population, simulation) {
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsOfType(simulation, "Simulation")
  ospsuite.utils::validateIsString(studyDesignFile)

  designData <- read.csv(studyDesignFile, header = FALSE, stringsAsFactors = FALSE)

  studyDesign <- StudyDesign$new(designData, population, simulation)

  return(studyDesign)
}

# Defines study design format
studyDesignPathLine <- 1
studyDesignUnitLine <- 2
studyDesignTypeLine <- 3

#' @title updateTargetValues
#' @param values initial vector to update
#' @param targetValues vector of values to be assigned using `sourceExpressions`
#' @param sourceExpressions study design expressions to be evaluated
#' @param data population data as data.frame
#' @import ospsuite
#' @keywords internal
updateTargetValues <- function(values, targetValues, sourceExpressions, data) {
  validateIsSameLength(targetValues, sourceExpressions)
  ospsuite.utils::validateIsOfType(data, "data.frame")
  ospsuite.utils::validateIsOfLength(values, nrow(data))

  for (sourceIndex in seq_along(sourceExpressions)) {
    sourceFilter <- eval(sourceExpressions[sourceIndex])

    values[sourceFilter] <- targetValues[sourceIndex]
  }
  return(values)
}

#' @title StudyDesign
#' @description StudyDesign
#' @field source expressions used on source data
#' @field targets list of targets of expressions and associated values
#' @keywords internal
StudyDesign <- R6::R6Class(
  "StudyDesign",
  cloneable = FALSE,
  public = list(
    source = NULL,
    targets = NULL,

    #' @description Create a new `StudyDesign` object.
    #' @param data data.frame read from study design file
    #' @param population `Population` object
    #' @param simulation `Simulation` object
    #' @return `StudyDesign` class object
    initialize = function(data, population, simulation) {
      ospsuite.utils::validateIsOfType(population, "Population")
      ospsuite.utils::validateIsOfType(simulation, "Simulation")
      self$targets <- mapStudyDesignTargets(data, population, simulation)
      self$source <- mapStudyDesignSources(data, population, simulation)

      for (target in self$targets) {
        validateIsSameLength(target$values, self$source)
      }
    },

    #' @description Print study design features
    #' @return data.frame
    print = function() {
      studyDesign <- data.frame(source = as.character(self$source))
      for (target in self$targets) {
        studyDesign <- cbind.data.frame(studyDesign, target$print())
      }
      print(studyDesign)
    }
  )
)

#' @title StudyDesignTarget
#' @description StudyDesignTarget
#' @field name path name of study design target
#' @field values values assigned to study design target
#' @keywords internal
StudyDesignTarget <- R6::R6Class(
  "StudyDesign",
  cloneable = FALSE,
  public = list(
    name = NULL,
    values = NULL,

    #' @description Create a new `StudyDesign` object.
    #' @param name path name of study design target
    #' @param values values assigned to study design target.
    #' `values` must be the same length as source condition expressions
    #' @return `StudyDesignTarget` class object
    initialize = function(name, values) {
      ospsuite.utils::validateIsString(name)
      ospsuite.utils::validateIsNumeric(values)

      self$name <- name
      self$values <- values
    },

    #' @description Print study design target features
    #' @return data.frame
    print = function() {
      target <- data.frame(self$values)
      names(target) <- self$name
      return(target)
    }
  )
)

#' @title mapStudyDesignSources
#' @param data data.frame read from a study design file
#' @param population `Population` object
#' @param simulation `Simulation` object
#' @return vector of expressions assigning target values
#' Must be the same length as target values
#' @import utils
#' @keywords internal
mapStudyDesignSources <- function(data, population, simulation) {
  sourceFilter <- grepl("SOURCE", data[studyDesignTypeLine, ])
  validateIsPositive(sum(sourceFilter))
  # Enforce data.frame with drop = FALSE
  sourceData <- data[, sourceFilter, drop = FALSE]
  sourceExpressions <- NULL

  for (columnIndex in seq(1, ncol(sourceData))) {
    path <- sourceData[studyDesignPathLine, columnIndex]
    values <- as.numeric(utils::tail(sourceData[, columnIndex], -studyDesignTypeLine))

    sourceType <- sourceData[studyDesignTypeLine, columnIndex]
    expressionType <- sourceTypeToExpressionType(sourceType)

    # Covariates are part of population but are not included in simulations
    # For all other paths, they will be checked using getQuantity and converted to base unit
    if (!path %in% population$allCovariateNames) {
      pathQuantity <- ospsuite::getQuantity(path, simulation)
      unit <- sourceData[studyDesignUnitLine, columnIndex]
      values <- ospsuite::toBaseUnit(pathQuantity, values, unit, simulation$molWeightFor(path))
    }

    sourceExpressionsByColumn <- paste0("data[,'", path, "']", expressionType, values)
    sourceExpressionsByColumn[values %in% NA] <- "TRUE"

    ospsuite.utils::ifNotNull(
      sourceExpressions,
      sourceExpressions <- paste(sourceExpressions, sourceExpressionsByColumn, sep = " & "),
      sourceExpressions <- sourceExpressionsByColumn
    )
  }
  return(parse(text = sourceExpressions))
}

sourceTypeToExpressionType <- function(sourceType) {
  ospsuite.utils::validateIsString(sourceType)
  expressionType <- NULL
  if (grepl("MIN", sourceType)) {
    expressionType <- " >= "
  }
  if (grepl("MAX", sourceType)) {
    expressionType <- " < "
  }
  if (grepl("EQUALS", sourceType)) {
    expressionType <- " == "
  }
  if (is.null(expressionType)) {
    ospsuite.utils::validateIsIncluded(sourceType, c("MIN", "MAX", "EQUALS"))
  }
  return(expressionType)
}

#' @title mapStudyDesignTargets
#' @param data data.frame read from a study design file
#' @param population `Population` object
#' @param simulation `Simulation` object
#' @return list of `StudyDesignTarget` objects
#' @import utils
#' @keywords internal
mapStudyDesignTargets <- function(data, population, simulation) {
  targetFilter <- grepl("TARGET", data[studyDesignTypeLine, ])
  validateIsPositive(sum(targetFilter))
  # Enforce data.frame with drop = FALSE
  targetData <- data[, targetFilter, drop = FALSE]
  target <- vector(mode = "list", length = ncol(targetData))

  for (columnIndex in seq(1, ncol(targetData))) {
    path <- targetData[studyDesignPathLine, columnIndex]
    values <- as.numeric(utils::tail(targetData[, columnIndex], -studyDesignTypeLine))

    # Covariates are part of population but are not included in simulations
    # For all other paths, they will be checked using getQuantity and converted to base unit
    if (!path %in% population$allCovariateNames) {
      pathQuantity <- ospsuite::getQuantity(path, simulation)
      unit <- targetData[studyDesignUnitLine, columnIndex]
      values <- ospsuite::toBaseUnit(pathQuantity, values, unit, simulation$molWeightFor(path))
    }
    target[[columnIndex]] <- StudyDesignTarget$new(path, values)
  }
  return(target)
}
