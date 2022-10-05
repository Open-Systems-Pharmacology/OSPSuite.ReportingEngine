#' @title tic
#' @description Trigger time tracker
#' @return Numeric time in seconds
#' @keywords internal
tic <- function() {
  return(as.numeric(Sys.time()))
}

#' @title getElapsedTime
#' @description Get computer time as a character
#' @param tic Numeric time from chrono trigger
#' @param unit display unit of elapsed time
#' @return Character displaying elapsed time in `unit`
#' @keywords internal
getElapsedTime <- function(tic, unit = "min") {
  toc <- as.numeric(Sys.time())
  # if unknown unit assumes seconds
  elapsedTime <- round(
    (toc - tic) / switch(unit,
      "h" = 3600,
      "min" = 60,
      "s" = 1,
      1
    ),
    1
  )
  return(paste(elapsedTime, unit))
}


#' @title getTimeStamp
#' @description Get computer time as a character
#' @keywords internal
getTimeStamp <- function() {
  return(format(Sys.time(), "%d/%m/%Y - %H:%M:%S"))
}

#' @title resetLogs
#' @description Reset/empty messages of global logging system
#' @param folder Folder where logs are saved
#' @keywords internal
resetLogs <- function(folder) {
  reEnv$log$reset(folder)
  return(invisible())
}

#' @title setLogFolder
#' @description Set folder where logs are saved
#' @param folder Folder where logs are saved
#' @keywords internal
setLogFolder <- function(folder) {
  reEnv$log$folder <- folder
  return(invisible())
}

#' @title saveLogsToJson
#' @description Save workflow logs to a json file
#' @param jsonFile Path of json file saving log messages
#' @export
saveLogsToJson <- function(jsonFile) {
  validateIsFileExtension(jsonFile, "json")
  reEnv$log$writeAsJson(jsonFile)
  return(invisible())
}

#' @title saveLogs
#' @description Save workflow logs to their respective files
#' @param folder Directory into which logs will be saved
#' @export
saveLogs <- function(folder = NULL) {
  if (!is.null(folder)) {
    setLogFolder(folder)
  }
  reEnv$log$write()
  return(invisible())
}

#' @title showLogMessages
#' @description Display log messages as a data.frame
#' @param logTypes Select specific logs in `LogTypes` that will be displayed in the data.frame.
#' @export
showLogMessages <- function(logTypes = LogTypes) {
  validateIsIncluded(logTypes, LogTypes)
  return(reEnv$log$showMessages(logTypes))
}

#' @title logError
#' @description Save error messages into a log error file
#' @param message message to save in log file
#' @param printConsole logical to print error on console
#' @export
logError <- function(message, printConsole = NULL) {
  reEnv$log$error(message, display = printConsole)
  return(invisible())
}

#' @title logErrorThenStop
#' @description Log the error with a message and then stop, displaying same message.
#' @param message message to display and then log
#' @param logFolder **Deprecated** path where logs are saved
#' @keywords internal
logErrorThenStop <- function(message, logFolder = NULL) {
  logError(message, printConsole = FALSE)
  stop(as.character(message), call. = FALSE)
}

#' @title logDebug
#' @description Save intermediate messages into a log debug file
#' @param message message to save in log file
#' @param printConsole logical to print error on console
#' @export
logDebug <- function(message, printConsole = NULL) {
  reEnv$log$debug(message, display = printConsole)
  return(invisible())
}

#' @title logInfo
#' @description Save info messages into a log info file
#' @param message message to save in log file
#' @param printConsole logical to print error on console
#' @export
logInfo <- function(message, printConsole = NULL) {
  reEnv$log$info(message, display = printConsole)
  return(invisible())
}

#' @title logCatch
#' @description Catch errors, log and display meaningfull information
#' @param expr Evaluated code chunks
#' @export
logCatch <- function(expr) {
  tryCatch(withCallingHandlers(
    expr,
    error = function(errorCondition) {
      # Informative trace keeps only calls related to error from all current calls
      # by removing tryCatch, logCatch, withCallingHandlers, simpleError from trace
      calls <- sys.calls()
      errorTrace <- "\n> Error Trace"
      if (requireNamespace("crayon", quietly = TRUE)) {
        errorTrace <- crayon::yellow$bold(errorTrace)
      }
      for (call in calls) {
        textCall <- deparse(call, nlines = 1)

        callNotDisplayed <- any(sapply(
          c("logCatch", "tryCatch", "withCallingHandlers", "simpleError"),
          FUN = function(pattern) {
            grepl(textCall, pattern = pattern, ignore.case = TRUE)
          }
        ))

        if (callNotDisplayed) {
          next
        }
        tabs <- paste0(rep(" ", length(errorTrace)), collapse = "")
        errorTrace <- c(
          errorTrace,
          paste0(tabs, "\u21aa ", textCall)
        )
      }
      errorMessage <- c(
        errorCondition$message,
        errorTrace
      )
      logError(errorMessage)
      stop(errorCondition$message)
    },
    warning = function(warningCondition) {
      # Remove unwanted warning from ggplot
      # In case, include them in log debug
      callNotDisplayed <- any(sapply(
        c("Transformation introduced infinite values",
          "Each group consists of only one observation",
          "rows containing non-finite values",
          "Ignoring unknown parameters"),
        FUN = function(pattern) {
          grepl(warningCondition$message, pattern = pattern)
        }
      ))
      if(callNotDisplayed){
        logDebug(warningCondition$message)
        tryInvokeRestart("muffleWarning")
        return(invisible())
      }
      logError(warningCondition$message)
      tryInvokeRestart("muffleWarning")
    }
  ),
  error = function(errorCondition) {
    stop(errorCondition$message, call. = FALSE)
  }
  )
}
