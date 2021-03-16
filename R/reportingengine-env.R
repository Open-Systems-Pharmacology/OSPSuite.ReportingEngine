# Environment that holds various global variables and settings for ospsuite.reportingengine package,
# It is not exported and should not be directly manipulated by other packages.
reEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
reEnv$packageName <- "ospsuite.reportingengine"

# Default format values for numerics
reEnv$formatNumericsDigits <- 5
reEnv$formatNumericsSmall <- 2
reEnv$formatNumericsScientific <- NA


#' @title setDefaultNumericFormat
#' @description Set default format for numeric values output in reports
#' @param digits Number of significant digits
#' @param nsmall Number of decimal digits
#' (Note: `nsmall` needs to be consistent with `digits` for being correctly used)
#' @param scientific Logical defining if numeric format uses a scientific expression
#' @export
setDefaultNumericFormat <- function(digits = NULL,
                                    nsmall = NULL,
                                    scientific = NULL){
  validateIsInteger(digits, nullAllowed = TRUE)
  validateIsInteger(nsmall, nullAllowed = TRUE)
  validateIsLogical(scientific, nullAllowed = TRUE)
  
  reEnv$formatNumericsDigits <- digits %||% reEnv$formatNumericsDigits
  reEnv$formatNumericsSmall <- nsmall %||% reEnv$formatNumericsSmall
  reEnv$formatNumericsScientific <- scientific %||% reEnv$formatNumericsScientific
  return(invisible())
}

# Default theme
reEnv$theme <- tlf::loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf"))

#' @title setDefaultTheme
#' @param theme `Theme` object from `tlf` package
#' @export
setDefaultTheme <- function(theme){
  validateIsOfType(theme,  "Theme")
  reEnv$theme <- theme
  tlf::useTheme(reEnv$theme)
}

#' @title setDefaultThemeFromJson
#' @param jsonFile path to json file where theme properties are stored
#' @export
setDefaultThemeFromJson <- function(jsonFile){
  validateIsString(jsonFile)
  validateIsFileExtension(jsonFile, "json")
  newTheme <- tlf::loadThemeFromJson(jsonFile)
  setDefaultTheme(newTheme)
}