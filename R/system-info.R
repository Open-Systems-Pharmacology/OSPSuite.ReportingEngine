# System Information for Reporting Engine

#' @title isValidatedSystem
#' @description Check if environment variable is defined and as value `'prodrun'`
#' @return logical assessing if system is validated
#' @keywords internal
isValidatedSystem <- function() {
  return(Sys.getenv("VALIDATEDR_ENVIRONMENT") %in% "prodrun")
}

#' @title ReportingEngineInfo
#' @description R6 class representing system information for Reporting Engine
#' @field Date Date at which the class is initialized
#' @keywords internal
ReportingEngineInfo <- R6::R6Class(
  "ReportingEngineInfo",
  public = list(
    Date = NULL,

    #' @description
    #' Create a new `ReportingEngineInfo` object.
    #' @return A new `ReportingEngineInfo` object
    initialize = function() {
      info <- getReportingEngineInfo()
      self$Date <- info$Date
      private$.computerName <- info$computerName
      private$.userName <- info$userName
      private$.login <- info$login
      private$.tlfVersion <- info$tlfVersion
      private$.ospsuiteVersion <- info$ospsuiteVersion
      private$.reportingengineVersion <- info$reportingengineVersion
      private$.Rversion <- info$Rversion
      private$.isValidatedSystem <- isValidatedSystem()
    },

    #' @description
    #' Print system information
    #' @return A text with system information
    print = function() {
      systemValidated <- " NOT"
      if (private$.isValidatedSystem) {
        systemValidated <- ""
      }
      infoPrint <- c(
        sprintf("Reporting Engine Information:"),
        sprintf("Date: %s", as.character(self$Date)),
        sprintf("User Information:"),
        sprintf("Computer Name: %s", private$.computerName),
        sprintf("User: %s", private$.userName),
        sprintf("Login: %s", private$.login),
        sprintf("System is%s validated", systemValidated),
        sprintf("System versions:"),
        sprintf("R version: %s", private$.Rversion),
        sprintf("OSP Suite Package version: %s", as.character(private$.ospsuiteVersion)),
        sprintf("OSP Reporting Engine version: %s", as.character(private$.reportingengineVersion)),
        sprintf("tlf version: %s", as.character(private$.tlfVersion))
      )
      invisible(self)
      return(infoPrint)
    },

    #' @description
    #' Is the system validated
    #' @return A logical reporting if the system is included in the validated systems
    isValidated = function() {
      return(private$.isValidatedSystem)
    }
  ),
  private = list(
    .computerName = NULL,
    .userName = NULL,
    .login = NULL,
    # Packages versions
    .tlfVersion = NULL,
    .ospsuiteVersion = NULL,
    .reportingengineVersion = NULL,
    .Rversion = NULL,
    .isValidatedSystem = FALSE
  )
)

#' @title getReportingEngineInfo
#' @description Get relevant information regarding the Reporting Engine
#' @return list of user information on reporting engine
#' @export
getReportingEngineInfo <- function() {
  info <- list()
  info$Date <- format(Sys.time(), "%d/%m/%Y - %H:%M:%S")
  systemInfo <- Sys.info()
  info$computerName <- systemInfo[["nodename"]]
  info$userName <- systemInfo[["user"]]
  info$login <- systemInfo[["login"]]

  # Packages versions
  info$tlfVersion <- utils::packageVersion(pkg = "tlf")
  info$ospsuiteVersion <- utils::packageVersion(pkg = "ospsuite")
  info$reportingengineVersion <- utils::packageVersion(pkg = "ospsuite.reportingengine")

  # R version
  session <- sessionInfo()
  info$Rversion <- session$R.version$version.string

  return(info)
}
