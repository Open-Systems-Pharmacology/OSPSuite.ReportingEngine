#' @title QualificationVersionInfo
#' @description Object defining Qualification Version Information to be displayed on title page
#' @field qualificationPlanRelease Qualification Plan Release Version (string, e.g. `"1.1"`)
#' @field osp OSP version (string, e.g. `"10.0"`)
#' @field qualificationFramework Qualification Framework version (string, e.g. `"3.0"`)
#' @export
#' @import ospsuite.utils
#' @family qualification worfklow
QualificationVersionInfo <- R6::R6Class(
  "QualificationVersionInfo",
  public = list(
    qualificationPlanRelease = NULL,
    osp = NULL,
    qualificationFramework = NULL,

    #' @description
    #' Create a new `QualificationVersionInfo` object.
    #' @param qualificationPlanRelease Qualification Plan Release Version (string, e.g. `"1.1"`)
    #' @param osp OSP version (string, e.g. `"10.0"`)
    #' @param qualificationFramework Qualification Framework version (string, e.g. `"3.0"`)
    #' @return A new `QualificationVersionInfo` object
    initialize = function(qualificationPlanRelease = NULL,
                              osp = NULL,
                              qualificationFramework = NULL) {
      validateIsString(qualificationPlanRelease, nullAllowed = TRUE)
      validateIsString(osp, nullAllowed = TRUE)
      validateIsString(qualificationFramework, nullAllowed = TRUE)

      # In gsub, character "." has a special meaning
      # and needs to be escaped in the expression using "\\"
      private$.qualificationPlanReleasePattern <- "x\\.x"
      private$.ospPattern <- "y\\.y"
      private$.qualificationFrameworkPattern <- "z\\.z"

      self$qualificationPlanRelease <- qualificationPlanRelease
      self$osp <- osp
      self$qualificationFramework <- qualificationFramework
    },

    #' @description
    #' Print version information
    #' @return A text with system information
    print = function() {
      infoPrint <- c(
        sprintf("Qualification Plan Release Version: %s", self$qualificationPlanRelease %||% ""),
        sprintf("OSP version: %s", self$osp %||% ""),
        sprintf("Qualification Framework version: %s", self$qualificationFramework %||% "")
      )
      cat(infoPrint, sep = "\n")
      return(invisible(infoPrint))
    },

    #' @description
    #' Update `text` if patterns of version info are found
    #' @param text character string
    #' @return Updated character string
    updateText = function(text) {
      eval(private$.updateExpression(c("qualificationPlanRelease", "osp", "qualificationFramework")))
      return(text)
    },

    #' @description
    #' Get the Qualification Plan Release Version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern**
    #' **Escape characters may be included in the pattern**
    #' @return A character
    getQualificationPlanReleasePattern = function() {
      return(private$.getPattern("qualificationPlanRelease"))
    },

    #' @description
    #' Get the OSP version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern**
    #' **Escape characters may be included in the pattern**
    #' @return A character
    getOSPPattern = function() {
      return(private$.getPattern("osp"))
    },

    #' @description
    #' Get the Qualification Framework version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern**
    #' **Escape characters may be included in the pattern**
    #' @return A character
    getQualificationFrameworkPattern = function() {
      return(private$.getPattern("qualificationFramework"))
    },

    #' @description
    #' Set the Qualification Plan Release Version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern.**
    #' **You may need to include escape characters in `pattern`**
    #' @param pattern characters to be replaced in title page
    setQualificationPlanReleasePattern = function(pattern) {
      validateIsString(pattern)
      private$.qualificationPlanRelease <- pattern
      return(invisible())
    },

    #' @description
    #' Set the OSP version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern.**
    #' **You may need to include escape characters in `pattern`**
    #' @param pattern characters to be replaced in title page
    setOSPPattern = function(pattern) {
      validateIsString(pattern)
      private$.osp <- pattern
      return(invisible())
    },

    #' @description
    #' Set the Qualification Framework version pattern to be replaced in title page
    #' **Caution, the function `gsub` is used to replace the pattern.**
    #' **You may need to include escape characters in `pattern`**
    #' @param pattern characters to be replaced in title page
    setQualificationFrameworkPattern = function(pattern) {
      validateIsString(pattern)
      private$.qualificationFrameworkPattern <- pattern
      return(invisible())
    }
  ),

  private = list(
    .qualificationPlanReleasePattern = NULL,
    .ospPattern = NULL,
    .qualificationFrameworkPattern = NULL,

    .getPattern = function(field) {
      # Return the value of a field to be passed on gsub pattern input
      return(eval(parse(text = paste0("private$.", field, "Pattern"))))
    },

    .updateExpression = function(field) {
      # Return expression to be evaluated that updates text based on version info
      return(parse(text = paste0(
        "if(!isEmpty(self$", field, ")){",
        "text <- gsub(pattern = private$.", field, "Pattern, replacement = self$", field, ", x = text)",
        "}"
      )))
    }
  )
)
