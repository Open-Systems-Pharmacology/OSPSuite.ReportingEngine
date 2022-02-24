re.tStartMetadataCapture <- function(...) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    return(tracelib::tStartMetadataCapture(..., offset = 2))
  }
}

re.tEndMetadataCapture <- function(...) {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    tracelib::tEndMetadataCapture(...)
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

re.tEndAction <- function() {
  if (requireNamespace("tracelib", quietly = TRUE)) {
    tracelib::tEndAction()
  }
}
