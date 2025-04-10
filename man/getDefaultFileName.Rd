% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filenames-nomenclature.R
\name{getDefaultFileName}
\alias{getDefaultFileName}
\title{getDefaultFileName}
\usage{
getDefaultFileName(..., suffix, extension = "csv", sep = "-")
}
\arguments{
\item{...}{names}

\item{suffix}{Suffix to be added at the end of the file name}

\item{extension}{file format. Default is \code{csv}}

\item{sep}{separation between names and suffix. Default is \code{-}}
}
\value{
default filename adding default suffix and file extension
}
\description{
Add suffix and extension to create a default file name
for population files, results files and PK-analysis files.
}
\keyword{internal}
