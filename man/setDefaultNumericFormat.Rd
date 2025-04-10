% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-reportingengine-env.R
\name{setDefaultNumericFormat}
\alias{setDefaultNumericFormat}
\title{setDefaultNumericFormat}
\usage{
setDefaultNumericFormat(digits = NULL, scientific = NULL)
}
\arguments{
\item{digits}{Number of significant digits}

\item{scientific}{Logical defining if numeric format uses a scientific expression}
}
\description{
Set default format for numeric values output in reports
}
\examples{
setDefaultNumericFormat(digits = 2, scientific = TRUE)
}
