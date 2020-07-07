#' @title PkParameterInfo
#' @description R6 class representing workflow selected PK parameter
#' @field pkParameter PK Parameter name for the simulation (e.g. `AUC_inf`)
#' @field displayName display name for `pkParameter`
#' @field displayUnit display unit for `pkParameter`
#' @export
#' @import ospsuite
PkParameterInfo <- R6::R6Class(
  "PkParameterInfo",
  cloneable = FALSE,
  public = list(
    pkParameter = NULL,
    displayName = NULL,
    displayUnit = NULL,

    #' @description
    #' Create a new `PkParameterInfo` object.
    #' @param pkParameter PK Parameter name for the simulation (e.g. `AUC_inf`)
    #' Tips: use ospsuite::allPKParameterNames() to get a list of available PK parameters
    #' @param displayName display name for `pkParameter`
    #' @param displayUnit display unit for `pkParameter`
    #' @return A new `PkParameterInfo` object
    initialize = function(pkParameter,
                              displayName = NULL,
                              displayUnit = NULL) {
      validateIsString(pkParameter)
      validateIsOfLength(pkParameter, 1)
      validateIsString(c(displayName, displayUnit), nullAllowed = TRUE)
      validateIsIncluded(pkParameter, ospsuite::allPKParameterNames())
      ifnotnull(displayName, validateIsOfLength(displayName, 1))
      ifnotnull(displayUnit, validateIsOfLength(displayUnit, 1))
      # TO DO: add validateIsUnit method
      # validateIsUnit(displayUnit, nullAllowed = TRUE)

      defaultPKParameterProperties <- ospsuite::pkParameterByName(name = pkParameter)

      self$pkParameter <- pkParameter
      self$displayName <- displayName %||% defaultPKParameterProperties$displayName
      self$displayUnit <- displayUnit %||% defaultPKParameterProperties$displayUnit
    }
  )
)
