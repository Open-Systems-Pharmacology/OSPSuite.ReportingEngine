#' @title ResidualScales
#' @description List of available scales for calculation of residuals
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists available Residual Scales
#' ResidualScales
#'
ResidualScales <- enum(c("Linear", "Logarithmic"))

#' @title DataSelectionKeys
#' @description List of available short keys for observed data selection
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists available Data Selection Keys
#' DataSelectionKeys
#'
DataSelectionKeys <- enum(c("NONE", "ALL"))

#' @title Output
#' @description R6 class representing workflow output
#' @field path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field displayName display name for `path`
#' @field displayUnit display unit for `path`
#' @field dataSelection character or expression used to select a subset of observed data
#' @field dataUnit Unit of the observed data
#' @field dataDisplayName display name of the observed data
#' @field pkParameters R6 class `PkParameterInfo` objects
#' @field residualScale Scale for calculation of residuals as included in enum `ResidualScales`
#' @export
#' @import ospsuite
#' @import ospsuite.utils
Output <- R6::R6Class(
  "Output",
  public = list(
    path = NULL,
    displayName = NULL,
    displayUnit = NULL,
    dataSelection = NULL,
    dataDisplayName = NULL,
    dataUnit = NULL,
    pkParameters = NULL,
    residualScale = NULL,

    #' @description
    #' Create a new `Output` object.
    #' @param path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param displayName display name for `path`
    #' @param displayUnit display unit for `path`
    #' @param dataSelection characters or expression to select subset the observed data
    #' By default, no data is selected.
    #' When using a character array, selections are concatenated with the `&` sign
    #' @param dataUnit Unit of the observed data
    #' @param dataDisplayName display name of the observed data
    #' @param pkParameters R6 class `PkParameterInfo` objects or their names
    #' @param residualScale Scale for calculation of residuals as included in enum `ResidualScales`
    #' @return A new `Output` object
    initialize = function(path,
                          displayName = NULL,
                          displayUnit = NULL,
                          dataSelection = DataSelectionKeys$NONE,
                          dataUnit = NULL,
                          dataDisplayName = NULL,
                          pkParameters = NULL,
                          residualScale = ResidualScales$Logarithmic) {
      validateIsString(path)
      validateIsOfLength(path, 1)
      validateIsString(c(displayName, dataUnit, displayUnit, dataDisplayName), nullAllowed = TRUE)
      validateIsIncluded(residualScale, ResidualScales)
      ifNotNull(displayName, validateIsOfLength(displayName, 1))
      ifNotNull(displayUnit, validateIsOfLength(displayUnit, 1))
      ifNotNull(dataUnit, validateIsOfLength(dataUnit, 1))
      ifNotNull(dataDisplayName, validateIsOfLength(dataDisplayName, 1))
      validateIsOfType(dataSelection, c("character", "expression"), nullAllowed = TRUE)
      validateIsOfType(c(pkParameters), c("character", "PkParameterInfo"), nullAllowed = TRUE)

      self$path <- path
      self$displayName <- displayName %||% path
      self$displayUnit <- displayUnit
      self$dataUnit <- dataUnit
      self$dataDisplayName <- dataDisplayName %||% self$displayName
      self$residualScale <- residualScale

      # If data selection is expression, use it as is
      self$dataSelection <- dataSelection
      # Ensure that dataFilter is of type expression
      if (isOfType(self$dataSelection, "character")) {
        # When concatenating, ALL won't be understood by dplyr
        # Needs to be replaced by true to select all data
        dataSelection[dataSelection %in% DataSelectionKeys$ALL] <- TRUE
        # Concatenate selections using &
        self$dataSelection <- paste(dataSelection, collapse = " & ")
        # If any selection include None, do not select anything
        if (isIncluded(DataSelectionKeys$NONE, dataSelection)) {
          self$dataSelection <- FALSE
        }
      }

      self$pkParameters <- c(pkParameters)
      if (isOfType(self$pkParameters, "character")) {
        self$pkParameters <- sapply(self$pkParameters, function(pkParameter) {
          PkParameterInfo$new(pkParameter)
        })
      }
    }
  )
)
