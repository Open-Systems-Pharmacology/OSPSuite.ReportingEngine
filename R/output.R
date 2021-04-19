#' @title ResidualScales
#' @description List of available scales for calculation of residuals
#' @export
#' @import ospsuite
ResidualScales <- ospsuite::enum(c("Linear", "Logarithmic"))

#' @title DataSelectionKeys
#' @description List of available short keys for observed data selection
#' @export
DataSelectionKeys <- list(
  NONE = "NONE",
  ALL = "ALL"
)

#' @title Output
#' @description R6 class representing workflow output
#' @field path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field displayName display name for `path`
#' @field displayUnit display unit for `path`
#' @field dataSelection character or expression used to filter the observed data
#' @field dataDisplayName display name of the observed data
#' @field pkParameters R6 class `PkParameterInfo` objects
#' @field residualScale Scale for calculation of residuals as included in enum `ResidualScales`
#' @export
#' @import ospsuite
Output <- R6::R6Class(
  "Output",
  cloneable = FALSE,
  public = list(
    path = NULL,
    displayName = NULL,
    displayUnit = NULL,
    dataSelection = NULL,
    dataDisplayName = NULL,
    pkParameters = NULL,
    residualScale = NULL,

    #' @description
    #' Create a new `Output` object.
    #' @param path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param displayName display name for `path`
    #' @param displayUnit display unit for `path`
    #' @param dataSelection characters or expression to filter the observed data
    #' @param dataDisplayName display name of the observed data
    #' @param pkParameters R6 class `PkParameterInfo` objects or their names
    #' @param residualScale Scale for calculation of residuals as included in enum `ResidualScales`
    #' @return A new `Output` object
    initialize = function(path,
                              displayName = NULL,
                              displayUnit = NULL,
                              dataSelection = DataSelectionKeys$NONE,
                              dataDisplayName = NULL,
                              pkParameters = NULL,
                              residualScale = ResidualScales$Logarithmic) {
      validateIsString(path)
      validateIsOfLength(path, 1)
      validateIsString(c(displayName, displayUnit, dataDisplayName), nullAllowed = TRUE)
      validateIsIncluded(residualScale, ResidualScales)
      ifnotnull(displayName, validateIsOfLength(displayName, 1))
      ifnotnull(displayUnit, validateIsOfLength(displayUnit, 1))
      ifnotnull(dataDisplayName, validateIsOfLength(dataDisplayName, 1))
      validateIsOfType(dataSelection, c("character", "expression"), nullAllowed = TRUE)
      ifnotnull(dataSelection, validateIsOfLength(dataSelection, 1))
      validateIsOfType(c(pkParameters), c("character", "PkParameterInfo"), nullAllowed = TRUE)

      self$path <- path
      self$displayName <- displayName %||% path
      self$displayUnit <- displayUnit
      self$residualScale <- residualScale

      # If filter is null, assumes that user won't get any observed data
      self$dataSelection <- dataSelection
      # Ensure that dataFilter is of type expression
      if (isOfType(self$dataSelection, "character")) {
        if (self$dataSelection %in% DataSelectionKeys$ALL) {
          self$dataSelection <- "TRUE"
        }
        self$dataSelection <- parse(text = self$dataSelection)
        if (dataSelection %in% DataSelectionKeys$NONE) {
          self$dataSelection <- NULL
        }
      }

      self$dataDisplayName <- dataDisplayName %||% self$displayName

      self$pkParameters <- c(pkParameters)

      if (isOfType(self$pkParameters, "character")) {
        self$pkParameters <- sapply(self$pkParameters, function(pkParameter) {
          PkParameterInfo$new(pkParameter)
        })
      }
    }
  )
)
