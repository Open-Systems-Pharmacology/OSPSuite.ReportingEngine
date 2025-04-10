#' @title tic
#' @description Trigger time tracker
#' @return Numeric time in seconds
#' @keywords internal
tic <- function() {
  return(as.numeric(Sys.time()))
}

#' @title getElapsedTime
#' @description Get computer time as a character
#' @param tic Numeric start time
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
resetLogs <- function(folder = NULL) {
  reEnv$log$reset(folder)
  return(invisible())
}

#' @title setLogFolder
#' @description Set folder where logs are saved
#' @param folder Folder where logs are saved
#' @keywords internal
setLogFolder <- function(folder = NULL) {
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
  setLogFolder()
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
#' @description Catch errors, log and display meaningful information
#' @param expr Evaluated code chunks
#' @export
logCatch <- function(expr) {
  tryCatch(
    withCallingHandlers(
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
            c("logCatch", "qualificationCatch", "stop", "tryCatch", "withCallingHandlers", "simpleError", "eval\\(ei, envir\\)"),
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
          c(
            "introduced infinite values",
            "Each group consists of only one observation",
            "rows containing non-finite values",
            "rows containing missing values",
            "Ignoring unknown parameters",
            "was deprecated in ggplot2",
            "(font family)*(not found)*(font database)",
            # warning thrown because of non-ASCII unicode characters
            "mbcsToSbcs"
          ),
          FUN = function(pattern) {
            grepl(warningCondition$message, pattern = pattern)
          }
        ))
        # invokeRestart("muffleWarning") prevents the unwanted  display of the message
        # as an actual warning written in red on the console
        # However, if the restart is not found, this ends up with an error
        # tryInvokeRestart could have been used instead but appeared only on R.version 4.0.0
        if (callNotDisplayed) {
          logDebug(warningCondition$message)
        } else {
          logError(warningCondition$message)
        }
        try({
          invokeRestart("muffleWarning")
        })
        return(invisible())
      },
      message = function(messageCondition) {
        # Remove unwanted messages especially from ggplot
        # In case, include them in log debug
        callNotDisplayed <- any(sapply(
          c("Each group consists of only one observation"),
          FUN = function(pattern) {
            grepl(messageCondition$message, pattern = pattern)
          }
        ))
        if (callNotDisplayed) {
          logDebug(messageCondition$message)
        } else {
          logInfo(messageCondition$message)
        }
        # Allows logCatch to go on after catching a message
        try({
          invokeRestart("muffleMessage")
        })
      }
    ),
    error = function(errorCondition) {
      # Prevent logging new messages in old log files after crash
      setLogFolder()
      stop(errorCondition$message, call. = FALSE)
    }
  )
}

#' @title displayConfigurationPlanPlotInfo
#' @description
#' Display information of a configuration plan `Plot` field
#' @param configurationPlanField A list extracted from a configuration plan field
#' @return Array of name properties and their value
#' @export
#' @examples
#'
#' # Example of list with properties
#' plotField <- list(
#'   PlotNumber = 5,
#'   Simulation = "Name of Simulation",
#'   Project = "Name of Project",
#'   Options = list(width = 10, height = 10, units = "cm")
#' )
#'
#' # Log messages are usually displayed through `cat()`, `error()` or `warning()`
#' cat(displayConfigurationPlanPlotInfo(plotField))
#'
displayConfigurationPlanPlotInfo <- function(configurationPlanField) {
  message <- unlist(lapply(
    names(configurationPlanField),
    FUN = function(fieldElement) {
      # Lists can include a lot of options
      # which would make a very long displayed message
      # If necessary, this can be changed to get a max number of characters
      if (isOfType(configurationPlanField[[fieldElement]], "list")) {
        return(paste0(highlight(fieldElement), ": list(...)"))
      }
      fieldValues <- paste(configurationPlanField[[fieldElement]], collapse = ", ")
      return(paste0(highlight(fieldElement), ": ", fieldValues))
    }
  ))
  infoTitle <- "Configuration Plan Information:"
  if (requireNamespace("crayon", quietly = TRUE)) {
    infoTitle <- crayon::yellow$bold(infoTitle)
  }
  message <- paste(c("", infoTitle, message, ""), collapse = "\n")
  return(message)
}


#' @title qualificationCatch
#' @description
#' Add configuration plan plot information when catching a warning/error
#' @param expr Evaluated code chunks
#' @param configurationPlanField A list extracted from a configuration plan field
#' @keywords internal
qualificationCatch <- function(expr, configurationPlanField = NULL) {
  withCallingHandlers(
    expr,
    error = function(errorCondition) {
      plotInfo <- displayConfigurationPlanPlotInfo(configurationPlanField)
      stop(c(errorCondition$message, plotInfo), call. = FALSE)
    },
    warning = function(warningCondition) {
      plotInfo <- displayConfigurationPlanPlotInfo(configurationPlanField)
      warning(c(warningCondition$message, plotInfo), call. = FALSE)
      try({
        invokeRestart("muffleWarning")
      })
    }
  )
  return(invisible())
}
