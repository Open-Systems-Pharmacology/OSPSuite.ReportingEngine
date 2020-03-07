#' @title logError
#' @description Save error messages into a log error file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-error.txt`
#' @param printConsole logical to print error on console
#' @export
logError <- function(message,
                     file = "log-error.txt",
                     printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  write(c(timeStamp, message),
    file = file, append = TRUE, sep = "\n"
  )
  return()
}

#' @title logDebug
#' @description Save intermediate messages into a log debug file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-debug.txt`
#' @param printConsole logical to print error on console
#' @export
logDebug <- function(message,
                     file = "log-debug.txt",
                     printConsole = FALSE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  write(c(timeStamp, message),
    file = file, append = TRUE, sep = "\n"
  )
  return()
}

#' @title logInfo
#' @description Save info messages into a log info file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-info.txt`
#' @param printConsole logical to print error on console
#' @export
logInfo <- function(message,
                    file = "log-info.txt",
                    printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  write(c(timeStamp, message),
    file = file, append = TRUE, sep = "\n"
  )
  return()
}

#' @title logWorkflow
#' @description Save messages into log files
#' @param message message to save in log files
#' @param pathFolder folder where the logs are saved
#' Default is `getwd()`
#' @param logTypes types of logs where message is saved
#' Use enum `LogTypes` to get all the logTypes.
#' Default is c("Info", "Debug")
#' @return
#' @export
logWorkflow <- function(message,
                        pathFolder = getwd(),
                        logTypes = c(LogTypes$Info, LogTypes$Debug)) {
  if (LogTypes$Info %in% logTypes) {
    logInfo(message,
      file = file.path(pathFolder, defaultFileNames$logInfoFile())
    )
  }
  if (LogTypes$Debug %in% logTypes) {
    logDebug(message,
      file = file.path(pathFolder, defaultFileNames$logDebugFile())
    )
  }
  if (LogTypes$Error %in% logTypes) {
    logError(message,
      file = file.path(pathFolder, defaultFileNames$logErrorFile())
    )
  }
  return()
}

#' @export
LogTypes <- enum(c(
  "Info",
  "Error",
  "Debug"
))
