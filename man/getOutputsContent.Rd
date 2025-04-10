% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-excel-input.R
\name{getOutputsContent}
\alias{getOutputsContent}
\title{getOutputsContent}
\usage{
getOutputsContent(excelFile, outputsTable, simulationOutputs)
}
\arguments{
\item{excelFile}{name of the Excel file from which the R script is created}

\item{outputsTable}{Data.frame read from the Excel sheet "Outputs"}

\item{simulationOutputs}{Names of Output objects used by simulation sets}
}
\value{
Character vector defining the \code{Output} object
}
\description{
Creates a character vector to be written in a workflow .R script defining \code{Output} object.
}
\keyword{internal}
