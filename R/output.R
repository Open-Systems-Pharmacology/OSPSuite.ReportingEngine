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
#' @field groupID Grouping Identifier.
#' Outputs with same identifier and unit are plotted together
#' @field color Displayed line/point color of the Output in plots
#' @field fill Displayed range color of the Output in plots
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
    groupID = NULL,
    color = NULL,
    fill = NULL,
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
    #' @param groupID Grouping Identifier.
    #' Outputs with same identifier and unit are plotted together
    #' @param color Displayed line/point color of the `Output` in plots
    #' @param fill Displayed range color of the `Output` in plots
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
                          groupID = NULL,
                          color = NULL,
                          fill = NULL,
                          dataSelection = DataSelectionKeys$NONE,
                          dataUnit = NULL,
                          dataDisplayName = NULL,
                          pkParameters = NULL,
                          residualScale = ResidualScales$Logarithmic) {
      validateIsString(path)
      validateIsOfLength(path, 1)
      # Check optional inputs
      optionalStringInputs <- c("displayName", "displayUnit", "color", "fill", "dataUnit", "dataDisplayName")
      eval(parse(text = paste0("validateIsString(", optionalStringInputs, ", nullAllowed = TRUE)")))
      eval(parse(text = paste0("ifNotNull(", optionalStringInputs, ", validateIsOfLength(", optionalStringInputs, ", 1))")))

      validateIsIncluded(residualScale, ResidualScales)
      validateIsOfType(c(pkParameters), c("character", "PkParameterInfo"), nullAllowed = TRUE)

      self$path <- path
      self$displayName <- displayName %||% path
      self$displayUnit <- displayUnit
      self$dataUnit <- dataUnit
      self$dataDisplayName <- dataDisplayName %||% self$displayName
      self$residualScale <- residualScale

      self$groupID <- groupID

      self$color <- color %||% newOutputColor()
      self$fill <- fill %||% newOutputColor()

      self$dataSelection <- translateDataSelection(dataSelection)

      self$pkParameters <- c(pkParameters)
      if (isOfType(self$pkParameters, "character")) {
        # Keep only unique PK parameter names
        self$pkParameters <- sapply(
          unique(self$pkParameters),
          function(pkParameter) {
            PkParameterInfo$new(pkParameter)
          }
        )
      }
      # Throw warning if PK Parameter display names are not the same
      checkNoDuplicate(
        values = sapply(
          self$pkParameters,
          function(pkParameter) {
            pkParameter$displayName
          }
        ),
        variableName = paste0("PK Parameters display names for Output '", self$displayName, "'"),
        nullAllowed = TRUE
      )
    }
  )
)
