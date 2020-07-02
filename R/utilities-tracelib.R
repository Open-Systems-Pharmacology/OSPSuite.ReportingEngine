tracelibCheck <- function() {
  return(suppressWarnings(expr = require("tracelib", quietly = TRUE)))
}

re.tStartMetadataCapture <- function(...) {
  print("tStartMetadataCapture")
  if (tracelibCheck()) {
    #return(tracelib::tStartMetadataCapture(..., offset = 2))
  }
}

re.tEndMetadataCapture <- function(..., actionToken) {
  print("tEndMetadataCapture")
  if (tracelibCheck()) {
    #tracelib::tEndMetadataCapture(..., actionToken = actionToken)
  }
}

re.tStoreFileMetadata <- function(...) {
  print("tStoreFileMetadata")
  if (tracelibCheck()) {
    #tracelib::tStoreFileMetadata(...)
  }
}

re.tStartAction <- function(...) {
  print("tStartAction")
  if (tracelibCheck()) {
    #return(tracelib::tStartAction(..., offset = 1))
  }
}

re.tEndAction <- function(..., actionToken) {
  print("tEndAction")
  if (tracelibCheck()) {
    #tracelib::tEndAction(..., actionToken = actionToken)
  }
}
