re.tStartMetadataCapture <- function(...) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    return(tracelib::tStartMetadataCapture(..., offset = 2))
  }
}

re.tEndMetadataCapture <- function(..., actionToken) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    tracelib::tEndMetadataCapture(..., actionToken = actionToken)
  }
}

re.tStoreFileMetadata <- function(...) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    tracelib::tStoreFileMetadata(...)
  }
}

re.tStartAction <- function(...) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    return(tracelib::tStartAction(..., offset = 1))
  }
}

re.tEndAction <- function(..., actionToken) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    tracelib::tEndAction(..., actionToken = actionToken)
  }
}
