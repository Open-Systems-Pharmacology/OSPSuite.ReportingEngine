#' @title logError
#' @description Save error messages into a log error file
#' @param message message to save in log file
#' @param file log file name.
#' Default is set in function logFileName
#' @param printConsole logical to print error on console
#' @export
logError <- function(message,
                     file = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logErrorFile()),
                     printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S"))
  if (printConsole) {
    warning(paste0(timeStamp, message))
  }
  write(paste0(timeStamp, message),
    file = file, append = TRUE, sep = " "
  )
  return(invisible())
}

#' @title logDebug
#' @description Save intermediate messages into a log debug file
#' @param message message to save in log file
#' @param file log file name.
#' Default is set in function logFileName
#' @param printConsole logical to print error on console
#' @export
logDebug <- function(message,
                     file = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logDebugFile()),
                     printConsole = FALSE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S"))
  if (printConsole) {
    print(paste0(timeStamp, message))
  }
  write(paste0(timeStamp, message),
    file = file, append = TRUE, sep = " "
  )
  return(invisible())
}

#' @title logInfo
#' @description Save info messages into a log info file
#' @param message message to save in log file
#' @param file log file name.
#' Default is set in function logFileName
#' @param printConsole logical to print error on console
#' @export
logInfo <- function(message,
                    file = file.path(defaultFileNames$workflowFolderPath(), defaultFileNames$logInfoFile()),
                    printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S"))
  if (printConsole) {
    print(paste0(timeStamp, message))
  }
  write(paste0(timeStamp, message),
    file = file, append = TRUE, sep = " "
  )
  return(invisible())
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
  return(invisible())
}

#' @title LogTypes
#' @description List of log types for recording workflow information
#' @export
LogTypes <- enum(c(
  "Info",
  "Error",
  "Debug"
))
