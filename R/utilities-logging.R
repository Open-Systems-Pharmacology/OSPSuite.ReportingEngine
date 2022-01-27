#' @title LogLevels
#' @description Helper enum listing all available log levels
#' @import ospsuite.utils
#' @export
#' @examples
#' LogLevels$Info
#' LogLevels$Warning
#' LogLevels$Error
#' LogLevels$Debug
LogLevels <- enum(c(
  "Info",
  "Warning",
  "Error",
  "Debug"
))

#' @title getLogPaths
#' @description Get paths of log files from requested log level
#' @param logLevel Name of log level. Use helper enum `LogLevels` for finding appropriate log level.
#' @param logFolder Folder in which the log files are saved
#' @return An array of file paths
#' @keywords internal
getLogPaths <- function(logLevel, logFolder){
  filePaths <- file.path(
    logFolder,
    switch(
      logLevel,
      "Info" = c("log-info.txt", "log-debug.txt"),
      "Warning" = c("log-error.txt", "log-debug.txt"),
      "Error" = c("log-info.txt", "log-error.txt", "log-debug.txt"),
      "Debug" = "log-debug.txt"
    )
  )
  return(filePaths)
}

#' @title timeStamp
#' @description Print time stamp for logs
#' @return A character string
#' @keywords internal
timeStamp <- function(){
  sprintf("%s : ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S"))
}

#' @title logMessage
#' @description Report a logging message
#' @param message message to report
#' @param logFolder Directory in which logs are saved.
#' If `NULL` the message is only reported on console
#' @import ospsuite.utils
#' @export
logMessage <- function(message, 
                       logLevel = LogLevels$Info,
                       logFolder = NULL){
  validateIsString(message)
  validateIsIncluded(logLevel, LogLevels)
  timedMessage <- paste0(timeStamp(), message)
  if(is.null(logFolder)){
    switch(
      logLevel,
      "Info" = cat(timedMessage),
      "Warning" = warning(timedMessage, call. = FALSE),
      "Error" = stop(timedMessage, call. = FALSE)
      )
    return(invisible())
  }
  logFiles <- getLogPaths(logLevel, logFolder)
  for(logFile in logFiles){
    # Using the function file, the log is created if unexisting
    fileObject <- file(logFile, encoding = "UTF-8")
    write(timedMessage, file = fileObject, sep = "\n")
    close(fileObject)
  }
  # Call function itself to print the message within the console
  logMessage(message, logLevel)
  return(invisible())
}
