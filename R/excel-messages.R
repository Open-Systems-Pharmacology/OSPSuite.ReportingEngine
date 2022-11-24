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
        if (isEmpty(self$messages[[sectionIndex]])) {
          next
        }

        message <- c(
          message,
          self$getMessageHeader(names(self$messages)[sectionIndex]),
          paste("- ", self$messages[[sectionIndex]])
        )
      }
      if (isEmpty(message)) {
        logInfo(self$getNoIssueMessage())
        return(invisible())
      }
      logError(message)
      return(invisible(message))
    },

    #' @description Get message header
    #' @param section name of R script section
    getMessageHeader = function(section) {
      return(paste(
        "Potential", highlight(self$type),
        "identified when writing section ", highlight(section)
      ))
    },

    #' @description Get message displaying no issue identified
    getNoIssueMessage = function() {
      return(paste("No potential", highlight(self$type), "identified when writing R script"))
    }
  )
)
