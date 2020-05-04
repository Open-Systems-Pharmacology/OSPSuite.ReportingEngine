#' @title Output
#' @description R6 class representing workflow output
#' @field path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field displayName display name for `path`
#' @field displayUnit display unit for `path`
#' @field dataFilter character or expression used to filter the observed data
#' @field dataDisplayName display name of the observed data
#' @export
Output <- R6::R6Class(
  "Output",
  public = list(
    path = NULL,
    displayName = NULL,
    displayUnit = NULL,
    dataFilter = NULL,
    dataDisplayName = NULL,

    #' @description
    #' Create a new `Output` object.
    #' @param path path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param displayName display name for `path`
    #' @param displayUnit display unit for `path`
    #' @param dataFilter characters or expression to filter the observed data
    #' @param dataDisplayName display name of the observed data
    #' @return A new `Output` object
    initialize = function(path,
                              displayName = NULL,
                              displayUnit = NULL,
                              dataFilter = NULL,
                              dataDisplayName = NULL) {
      validateIsString(path)
      validateIsString(c(displayName, displayUnit, dataDisplayName), nullAllowed = TRUE)
      validateIsOfType(dataFilter, c("character", "expression"), nullAllowed = TRUE)

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
    }
  )
)
