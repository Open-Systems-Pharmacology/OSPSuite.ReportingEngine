#' @title addStudyParameters
#' @param population `Population` object
#' @param studyDesignFile file name of study design table
#' @export
#' @import ospsuite
addStudyParameters <- function(population, studyDesignFile){
  validateIsOfType(population, "Population")
  
  studyDesign <- loadStudyDesign(studyDesignFile)
  
  initialTargetValues <- rep(NA, population$count)
  populationData <- ospsuite::populationAsDataFrame(population)
  
  for(target in studyDesign$targets){
    parameterPath <- target$name
    
    updatedTargetValues <- updateTargetValues(initialTargetValues, target$values, studyDesign$source, populationData)
    
    population$setParameterValues(parameterPath, updatedTargetValues)
  }
}

#' @title updateTargetValues
#' @param values initial vector to update
#' @param targetValues vector of values to be assigned using `sourceExpressions`
#' @param sourceExpressions study design expressions to be evaluated
#' @param data population data as data.frame
#' @export
#' @import ospsuite
updateTargetValues <- function(values, targetValues, sourceExpressions, data){
  validateIsSameLength(targetValues, sourceExpressions)
  validateIsOfType(data, "data.frame")
  validateIsOfLength(values, nrow(data))
  
  for(sourceIndex in seq_along(sourceExpressions)){
    sourceFilter <- eval(sourceExpressions[sourceIndex])
    
    values[sourceFilter] <- targetValues[sourceIndex]
    }
  
  return(values)
}

#' @title loadStudyDesign
#' @description Load a StudyDesign object from a file containing a study design table
#' In this table, Line 1 is path,  Line 2 is unit, Line 3 is type and subsequent lines are values.
#' The cells for type can include "SOURCE" OR "TARGET". "SOURCE" type can also include subtype "MIN", "MAX" or "EQUALS".
#' In the current version, unit is expected to be intern units of simulation.
#' @param studyDesignFile file name of study design table
#' @return A `StudyDesign` object`
#' @export
loadStudyDesign <- function(studyDesignFile) {
  validateIsString(studyDesignFile)
  designData <- read.csv(studyDesignFile, header = FALSE, stringsAsFactors = FALSE)

  studyDesign <- StudyDesign$new(designData)

  return(studyDesign)
}

# Defines study design format
studyDesignPathLine <- 1
studyDesignUnitLine <- 2
studyDesignTypeLine <- 3

#' @title StudyDesign
#' @description StudyDesign
#' @field source expressions used on source data
#' @field targets list of targets of expressions and associated values
#' @export
StudyDesign <- R6::R6Class(
  "StudyDesign",
  cloneable = FALSE,
  public = list(
    source = NULL,
    targets = NULL,

    #' @description Create a new `StudyDesign` object.
    #' @param data data.frame read from study design file
    #' @return `StudyDesign` class object
    initialize = function(data) {
      self$targets <- mapStudyDesignTargets(data)

      self$source <- mapStudyDesignSources(data)

      for (target in self$targets) {
        validateIsSameLength(target$values, self$source)
      }
    },
    
    #' @description Print study design features
    #' @return data.frame
    print = function() {
      studyDesign <- data.frame(source = as.character(self$source))
      for (target in self$targets){
        studyDesign <- cbind.data.frame(studyDesign,
                                        target$print())
      }
      print(studyDesign)
    }
  )
)

#' @title StudyDesignTarget
#' @description StudyDesignTarget
#' @field name path name of study design target
#' @field unit path unit of study design target
#' @field values values assigned to study design target.
#' @export
StudyDesignTarget <- R6::R6Class(
  "StudyDesign",
  cloneable = FALSE,
  public = list(
    name = NULL,
    unit = NULL,
    values = NULL,

    #' @description Create a new `StudyDesign` object.
    #' @param name path name of study design target
    #' @param unit path unit of study design target
    #' @param values values assigned to study design target.
    #' `values` must be the same length as source condition expressions
    #' @return `StudyDesignTarget` class object
    initialize = function(name, unit, values) {
      validateIsString(c(name, unit))
      validateIsNumeric(values)

      self$name <- name
      self$unit <- unit
      self$values <- values
    },

    #' @description Print study design target features
    #' @return data.frame
    print = function() {
      target <- data.frame(self$values)
      names(target) <- self$name
      if (!self$unit %in% "") {
        names(target) <- paste0(self$name, " [", self$unit, "]")
      }
      return(target)
    }
  )
)

#' @title mapStudyDesignSources
#' @param data data.frame read from a study design file
#' @return vector of expressions assigning target values
#' Must be the same length as target values
#' @import utils
mapStudyDesignSources <- function(data) {
  sourceFilter <- grepl("SOURCE", data[studyDesignTypeLine, ])
  validateIsPositive(sum(sourceFilter))
  sourceData <- data[, sourceFilter]

  sourceExpressions <- NULL
  for (columnIndex in seq(1, ncol(sourceData))) {
    path <- sourceData[studyDesignPathLine, columnIndex]
    unit <- sourceData[studyDesignUnitLine, columnIndex]
    sourceType <- sourceData[studyDesignTypeLine, columnIndex]

    values <- utils::tail(sourceData[, columnIndex], -studyDesignTypeLine)

    expressionType <- sourceTypeToExpressionType(sourceType)

    sourceExpressionsByColumn <- paste0("data[,'", path, "']", expressionType, values)

    sourceExpressionsByColumn[values %in% ""] <- "TRUE"

    ifnotnull(
      sourceExpressions,
      sourceExpressions <- paste(sourceExpressions, sourceExpressionsByColumn, sep = " & "),
      sourceExpressions <- sourceExpressionsByColumn
    )
  }

  return(parse(text = sourceExpressions))
}

sourceTypeToExpressionType <- function(sourceType) {
  validateIsString(sourceType)
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
    validateIsIncluded(sourceType, c("MIN", "MAX", "EQUALS"))
  }
  return(expressionType)
}

#' @title mapStudyDesignTargets
#' @param data data.frame read from a study design file
#' @return list of `StudyDesignTarget` objects
#' @import utils
mapStudyDesignTargets <- function(data) {
  targetFilter <- grepl("TARGET", data[studyDesignTypeLine, ])
  validateIsPositive(sum(targetFilter))
  targetData <- data[, targetFilter]

  target <- vector(mode = "list", length = ncol(targetData))
  for (columnIndex in seq(1, ncol(targetData))) {
    targetValues <- as.numeric(utils::tail(targetData[, columnIndex], -studyDesignTypeLine))

    target[[columnIndex]] <- StudyDesignTarget$new(
      name = targetData[studyDesignPathLine, columnIndex],
      unit = targetData[studyDesignUnitLine, columnIndex],
      values = targetValues
    )
  }

  return(target)
}
