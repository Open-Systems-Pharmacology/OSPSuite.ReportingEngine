#' @title LogTypes
#' @description List of log types for recording workflow information
#' @export
#' @examples
#' LogTypes$Info
#' LogTypes$Error
#' LogTypes$Debug
LogTypes <- enum(c(
  "Info",
  "Error",
  "Debug"
))


#' @title Logging
#' @description R6 class managing logging
#' @import ospsuite.utils
#' @keywords internal
Logging <- R6::R6Class(
  "Logging",
  cloneable = FALSE,
  public = list(
    #' @field messages Array of messages to log.
    #' Each element of `messages` is a list
    #' that includes `time`, `type` and `text` of message to log.
    messages = list(),
    #' @field folder Folder where log files are saved
    folder = NULL,

    #' @description
    #' Add an `Info` message to log
    #' @param text content of the `Info` message
    #' @param display Logical setting if message is displayed on console
    info = function(text, display = NULL) {
      message <- self$textToMessage(text, logType = LogTypes$Info)
      self$printMessage(message, display)
      self$saveMessage(message)
      self$addMessage(message)
      return(invisible())
    },
    #' @description
    #' Add an `Error` message to log
    #' @param text content of the `Error` message
    #' @param display Logical setting if message is displayed on console
    error = function(text, display = NULL) {
      message <- self$textToMessage(text, logType = LogTypes$Error)
      self$printMessage(message, display)
      self$saveMessage(message)
      self$addMessage(message)
      return(invisible())
    },
    #' @description
    #' Add a `Debug` message to log
    #' @param text content of the `Debug` message
    #' @param display Logical setting if message is displayed on console
    debug = function(text, display = NULL) {
      message <- self$textToMessage(text, logType = LogTypes$Debug)
      self$printMessage(message, display)
      self$saveMessage(message)
      self$addMessage(message)
      return(invisible())
    },

    #' @description
    #' Add meta data to `text` to create an informative message to log
    #' with a flag for its `logType` (`"Info"`, `"Error"`, or `"Debug"`)
    #' @param text content of the message
    #' @param logType type of message (`"Info"`, `"Error"`, or `"Debug"`)
    textToMessage = function(text, logType) {
      list(
        time = getTimeStamp(),
        type = logType,
        text = as.character(text)
      )
    },

    #' @description
    #' Add a message to the list of logged `messages`
    #' @param message content of the message
    addMessage = function(message) {
      message$text <- removeHighlight(message$text)
      self$messages[[length(self$messages) + 1]] <- message
    },

    #' @description
    #' Display logged `messages` as a data.frame
    #' Can be filtered according to `logTypes` (`"Info"`, `"Error"`, and/or `"Debug"`)
    #' @param logTypes types of message (`"Info"`, `"Error"`, or `"Debug"`)
    showMessages = function(logTypes = LogTypes) {
      selectedMessages <- data.frame()
      for (message in self$messages) {
        if (!isIncluded(message$type, logTypes)) {
          next
        }
        selectedMessages <- rbind.data.frame(
          selectedMessages,
          as.data.frame(message)
        )
      }
      return(selectedMessages)
    },

    #' @description
    #' Print the log messages on R/RStudio console
    #' @param message Message to print
    #' @param display Logical setting if message is displayed on console
    printMessage = function(message, display = NULL) {
      if (!(display %||% reEnv$defaultLogPrint[[message$type]])) {
        return(invisible())
      }
      cat(paste0(message$time, "\n"))
      # If installed, use crayon to beautify the message display
      if (requireNamespace("crayon", quietly = TRUE)) {
        # Green font for log Info
        if (isIncluded(message$type, LogTypes$Info)) {
          cat(crayon::green$bold("i Info\t"))
          cat(message$text, sep = "\n\t")
        }
        # Yellow font for log Error
        if (isIncluded(message$type, LogTypes$Error)) {
          cat(crayon::yellow$bold("! Warning\t"))
          cat(message$text, sep = "\n\t")
        }
        # Blue font for log Debug
        if (isIncluded(message$type, LogTypes$Debug)) {
          cat(crayon::blue$bold("> Debug\t"))
          cat(message$text, sep = "\n\t")
        }
        return(invisible())
      }
      # Yellow font for log Error
      if (isIncluded(message$type, LogTypes$Error)) {
        warning(message$text, call. = FALSE, immediate. = TRUE)
        return(invisible())
      }
      cat(message$text, sep = "\n\t")
      return(invisible())
    },

    #' @description
    #' Write a message in its log file
    #' @param message Message to print
    saveMessage = function(message) {
      if (is.null(self$folder)) {
        return(invisible())
      }
      if (!file.exists(self$folder)) {
        dir.create(self$folder, showWarnings = FALSE, recursive = TRUE)
      }
      logFile <- switch(message$type,
        "Info" = file.path(self$folder, private$.logInfo),
        "Error" = file.path(self$folder, private$.logError),
        "Debug" = file.path(self$folder, private$.logDebug)
      )
      fileObject <- file(logFile, encoding = "UTF-8", open = "at")
      write(message$time, file = fileObject, append = TRUE)
      write(removeHighlight(message$text), file = fileObject, append = TRUE, sep = "\n")
      close(fileObject)
      return(invisible())
    },

    #' @description
    #' Write the log messages in a json log file
    #' @param fileName Name of json log file
    writeAsJson = function(fileName) {
      write(
        jsonlite::toJSON(self$messages, auto_unbox = TRUE, pretty = TRUE),
        file = fileName
      )
      return(invisible())
    },

    #' @description
    #' Write the log messages in txt log files
    write = function() {
      for (message in self$messages) {
        self$saveMessage(message)
      }
      return(invisible())
    },

    #' @description
    #' Reset/empty log messages
    #' @param folder Folder where logs are saved
    reset = function(folder = NULL) {
      self$messages <- list()
      self$folder <- folder
      return(invisible())
    }
  ),
  private = list(
    .logInfo = "log-info.txt",
    .logError = "log-error.txt",
    .logDebug = "log-debug.txt"
  )
)
