# System Information for Reporting Engine

#' @title ReportingEngineInfo
#' @docType class
#' @description  R6 class providing relevant information regarding the Reporting Engine
#' @field Date (public)
#' @field computerName (private) name of computer
#' @field userName (private) name of user
#' @field login (private)
#' @field tlfVersion (private) version of tlf package
#' @field ospsuiteVersion (private) version of osp suite package
#' @field Rversion (private) version of R
#' @field isValidatedSystem (private) logical for computers that are evaluatued validated for Reporting Engine
#' @section Methods:
#' \describe{
#' \item{new()}{Initialize ReportingEngineInfo}
#' \item{print()}{Print Reporting Engine Inforation}
#' }
#' @export
#' @format NULL
ReportingEngineInfo <- R6::R6Class(
  "ReportingEngineInfo",
  public = list(
    Date = NULL,
    initialize = function() {
      info <- getReportingEngineInfo()
      self$Date <- info$Date
      private$computerName <- info$computerName
      private$userName <- info$userName
      private$login <- info$login
      private$tlfVersion <- info$tlfVersion
      private$ospsuiteVersion <- info$ospsuiteVersion
      private$Rversion <- info$Rversion
      # TO DO: define validated list of systems that will use Reporting Engine
      validatedSystems <- info$computerName
      if (private$computerName %in% validatedSystems) {
        private$isValidatedSystem <- TRUE
      }
    },
    print = function() {
      systemValidated <- "NOT"
      if (private$isValidatedSystem) {
        systemValidated <- ""
      }
      infoPrint <- c(sprintf("\nReporting Engine Information:\n"),
                     sprintf("Date: %s \n", as.character(self$Date)),
                     sprintf("\nUser Information: \n"),
                     sprintf("Computer Name: %s \n", private$computerName),
                     sprintf("User: %s \n", private$userName),
                     sprintf("Login: %s \n", private$login),
                     sprintf("System is %s validated \n", systemValidated),
                     sprintf("\nSystem versions: \n"),
                     sprintf("R version: %s \n", private$Rversion),
                     sprintf("OSP Suite Package version: %s \n", as.character(private$ospsuiteVersion)),
                     sprintf("tlf version: %s \n", as.character(private$tlfVersion))
      )
      invisible(self)
      return(infoPrint)
    }
  ),
  private = list(
    computerName = NULL,
    userName = NULL,
    login = NULL,
    # Packages versions
    tlfVersion = NULL,
    ospsuiteVersion = NULL,
    # reportingengineVersion = NULL,
    Rversion = NULL,
    isValidatedSystem = FALSE
  )
)

#' @title getReportingEngineInfo
#' @description Get relevant information regarding the Reporting Engine
#' @return list of user information on reporting engine
#' @export
getReportingEngineInfo <- function() {
  info <- list()
  info$Date <- Sys.time()
  systemInfo <- Sys.info()
  info$computerName <- systemInfo[["nodename"]]
  info$userName <- systemInfo[["user"]]
  info$login <- systemInfo[["login"]]

  # Packages versions
  info$tlfVersion <- utils::packageVersion(pkg = "tlf")
  info$ospsuiteVersion <- utils::packageVersion(pkg = "ospsuite")
  # info$reportingengineVersion <- utils::packageVersion(pkg = "ospsuite.reportingengine")

  # R version
  session <- sessionInfo()
  info$Rversion <- session$R.version$version.string

  return(info)
}
