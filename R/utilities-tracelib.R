tracelibCheck <- function() {
  return(suppressWarnings(expr = require("tracelib", quietly = TRUE)))
}

re.tStartMetadataCapture <- function(...) {
  if (tracelibCheck()) {
    return(tracelib::tStartMetadataCapture(..., offset = 2))
  }
}

re.tEndMetadataCapture <- function(..., actionToken) {
  if (tracelibCheck()) {
    tracelib::tEndMetadataCapture(..., actionToken = actionToken)
  }
}

re.tStoreFileMetadata <- function(...) {
  if (tracelibCheck()) {
    tracelib::tStoreFileMetadata(...)
  }
}

re.tStartAction <- function(...) {
  if (tracelibCheck()) {
    return(tracelib::tStartAction(..., offset = 1))
  }
}

re.tEndAction <- function(..., actionToken) {
  if (tracelibCheck()) {
    tracelib::tEndAction(..., actionToken = actionToken)
  }
}
