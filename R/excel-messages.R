#' @title ExcelMessaging
#' @description  R6 class for tracking and printing messages for the Excel template
#' @field type "warnings" or "errors" (could potentially include other kind of information)
#' @field messages list of messages included in each script section
#' @importFrom ospsuite.utils %||%
#' @keywords internal
ExcelMessaging <- R6::R6Class(
  "ExcelMessaging",
  cloneable = FALSE,

  public = list(
    type = NULL,
    messages = NULL,

    #' @description Initialize an `ExcelMessaging` object
    #' @param type "warnings" or "errors"
    #' @return A new `ExcelMessaging` object
    initialize = function(type) {
      validateIsIncluded(type, c("errors", "warnings"))
      self$type <- type
      self$messages <- list()
    },

    #' @description display the messages stored in messages
    #' @return Messages content
    displayMessage = function() {
      message <- NULL
      for (sectionIndex in seq_along(self$messages)) {
        if (isOfLength(self$messages[[sectionIndex]], 0)) {
          next
        }
        message <- c(
          message,
          paste0("The following ", self$type, " were identified during the writing of section '", names(self$messages)[sectionIndex], "':"),
          paste("- ", self$messages[[sectionIndex]])
        )
      }
      # Message when no warning founds
      message <- message %||% paste0("No ", self$type, " found during the writing of the script")
      cat(message, sep = "\n")
      return(invisible(message))
    }
  )
)
