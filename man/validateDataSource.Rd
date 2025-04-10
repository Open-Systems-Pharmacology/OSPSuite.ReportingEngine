% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error-checks.R
\name{validateDataSource}
\alias{validateDataSource}
\title{Check the consistency between observed data and its dictionary.
Units for \code{dv }and \code{time} need to be defined at least once in either
the observed dataset, its dictionary or outputs
In case of multiple definitions, warnings will thrown and the following priorities will be applied:
\enumerate{
\item Use units from outputs
\item Use units from observed dataset
\item Use units from dictionary
}}
\usage{
validateDataSource(dataSource, outputs, nullAllowed = TRUE)
}
\arguments{
\item{dataSource}{A \code{DataSource} object}

\item{outputs}{list or array of \code{Output} objects}

\item{nullAllowed}{Logical defining if \code{NULL} input is allowed}
}
\description{
Check the consistency between observed data and its dictionary.
Units for \code{dv }and \code{time} need to be defined at least once in either
the observed dataset, its dictionary or outputs
In case of multiple definitions, warnings will thrown and the following priorities will be applied:
\enumerate{
\item Use units from outputs
\item Use units from observed dataset
\item Use units from dictionary
}
}
\keyword{internal}
