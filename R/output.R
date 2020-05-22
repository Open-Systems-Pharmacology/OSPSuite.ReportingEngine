#' @title Output
#' @description R6 class representing workflow output
#' @field path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field displayName display name for `path`
#' @field displayUnit display unit for `path`
#' @field dataFilter character or expression used to filter the observed data
#' @field dataDisplayName display name of the observed data
#' @field pkParameters R6 class `PkParameterInfo` objects
#' @export
#' @import ospsuite
Output <- R6::R6Class(
  "Output",
  cloneable = FALSE,
  public = list(
    path = NULL,
    displayName = NULL,
    displayUnit = NULL,
    dataFilter = NULL,
    dataDisplayName = NULL,
    pkParameters = NULL,

    #' @description
    #' Create a new `Output` object.
    #' @param path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param displayName display name for `path`
    #' @param displayUnit display unit for `path`
    #' @param dataFilter characters or expression to filter the observed data
    #' @param dataDisplayName display name of the observed data
    #' @param pkParameters R6 class `PkParameterInfo` objects or their names
    #' @return A new `Output` object
    initialize = function(path,
                          displayName = NULL,
                          displayUnit = NULL,
                          dataFilter = NULL,
                          dataDisplayName = NULL,
                          pkParameters = NULL) {
      validateIsString(path)
      validateIsOfLength(path, 1)
      validateIsString(c(displayName, displayUnit, dataDisplayName), nullAllowed = TRUE)
      ifnotnull(displayName, validateIsOfLength(displayName, 1))
      ifnotnull(displayUnit, validateIsOfLength(displayUnit, 1))
      ifnotnull(dataDisplayName, validateIsOfLength(dataDisplayName, 1))
      validateIsOfType(dataFilter, c("character", "expression"), nullAllowed = TRUE)
      ifnotnull(dataFilter, validateIsOfLength(dataFilter, 1))
      validateIsOfType(c(pkParameters), c("character", "PkParameterInfo"), nullAllowed = TRUE)

      self$path <- path
      self$displayName <- displayName %||% path
      self$displayUnit <- displayUnit

      # If filter is null, assumes that user won't get any observed data
      self$dataFilter <- dataFilter
      # Ensure that dataFilter is of type expression
      if (isOfType(self$dataFilter, "character")) {
        self$dataFilter <- parse(text = self$dataFilter)
      }

      self$dataDisplayName <- dataDisplayName %||% paste0(self$displayName, " observed data")

      self$pkParameters <- c(pkParameters)

      if (isOfType(self$pkParameters, "character")) {
        self$pkParameters <- sapply(self$pkParameters, function(pkParameter) {
          PkParameterInfo$new(pkParameter)
        })
      }
    }
  )
)
