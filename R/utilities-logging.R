#' @title logError
#' @description Save error messages into a log error file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-error.txt`
#' @param printConsole logical to print error on console
#' @return
#' @export
logError <- function(message,
                     file = "log-error.txt",
                     printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  return(message)
}

#' @title logDebug
#' @description Save intermediate messages into a log debug file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-debug.txt`
#' @param printConsole logical to print error on console
#' @return
#' @export
logDebug <- function(message,
                     file = "log-debug.txt",
                     printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  return(message)
}

#' @title logInfo
#' @description Save info messages into a log info file
#' @param message message to save in log file
#' @param file log file name.
#' Default is `log-info.txt`
#' @param printConsole logical to print error on console
#' @return
#' @export
logInfo <- function(message,
                    file = "log-info.txt",
                    printConsole = TRUE) {
  timeStamp <- sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M"))
  if (printConsole) {
    print(timeStamp)
    print(message)
  }
  return(message)
}
