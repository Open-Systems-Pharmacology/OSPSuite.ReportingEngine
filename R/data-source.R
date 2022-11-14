#' @title DataSource
#' @description R6 class managing observed data in simulation sets
#' @field dataFile file name of the observed dataset
#' @field metaDataFile file name of the observed data dictionary
#' @field caption displayed legend of the data in the report
#' @field descriptor displayed data source descriptor in the report
#' @export
DataSource <- R6::R6Class(
  "DataSource",
  public = list(
    dataFile = NULL,
    metaDataFile = NULL,
    caption = NULL,
    descriptor = "Data source:",

    #' @description
    #' Create a new `DataSource` object.
    #' @param dataFile file name of the observed dataset
    #' @param metaDataFile file name of the observed data dictionary
    #' @param caption displayed legend of the data in the report
    #' @return A new `DataSource` object
    initialize = function(dataFile,
                          metaDataFile,
                          caption = NULL) {
      validateIsString(dataFile)
      validateIsString(metaDataFile)
      validateIsString(caption, nullAllowed = TRUE)

      self$dataFile <- dataFile
      self$metaDataFile <- metaDataFile
      self$caption <- caption
    },

    #' @description
    #' Get the report caption that informs about the data source
    #' @param workflowFolder Path of workflow folder
    #' @return A character string
    getCaption = function(workflowFolder = NULL) {
      if (!isEmpty(self$caption)) {
        return(paste(self$descriptor, self$caption))
      }
      if (isEmpty(workflowFolder)) {
        return(paste(self$descriptor, self$dataFile))
      }
      return(paste(self$descriptor, self$getRelativeDataPath(workflowFolder)))
    },

    #' @description
    #' Get the relative data path compared to workflow folder
    #' @param workflowFolder Path of workflow folder
    #' @return A character string
    getRelativeDataPath = function(workflowFolder) {
      # Use strplit combined with normalizePath to get a vector of path elements
      workflowPathElements <- unlist(strsplit(normalizePath(workflowFolder, winslash = "/", mustWork = FALSE), "/"))
      dataPathElements <- unlist(strsplit(normalizePath(self$dataFile, winslash = "/", mustWork = FALSE), "/"))
      # Then, remove elements of workflowFolder found in dataFile
      workflowPathSize <- length(workflowPathElements)
      dataPathSize <- length(dataPathElements)
      isCommon <- c(
        sapply(
          1:min(dataPathSize, workflowPathSize),
          FUN = function(index) {
            workflowPathElements[index] == dataPathElements[index]
          }
        ),
        rep(FALSE, max(0, dataPathSize - workflowPathSize))
      )
      return(paste0(dataPathElements[!isCommon], collapse = "/"))
    }
  )
)
