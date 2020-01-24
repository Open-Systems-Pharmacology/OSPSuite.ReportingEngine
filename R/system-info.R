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
      cat("Reporting Engine Information: \n")
      cat("Date: ", as.character(self$Date), "\n", sep = "")
      cat("\nUser Information: \n")
      cat("Computer Name: ", private$computerName, "\n", sep = "")
      cat("User: ", private$userName, "\n", sep = "")
      cat("Login: ", private$login, "\n", sep = "")
      cat("System is ")
      if (!private$isValidatedSystem) {
        cat("NOT")
      }
      cat("validated \n", sep = "")
      cat("\nSystem versions: \n")
      cat("R version: ", private$Rversion, "\n", sep = "")
      cat("OSP Suite Package version: ", as.character(private$ospsuiteVersion), "\n", sep = "")
      cat("tlf version: ", as.character(private$tlfVersion), "\n", sep = "")

      invisible(self)
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
