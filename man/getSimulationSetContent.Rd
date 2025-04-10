% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-excel-input.R
\name{getSimulationSetContent}
\alias{getSimulationSetContent}
\title{getSimulationSetContent}
\usage{
getSimulationSetContent(excelFile, simulationTable, workflowMode)
}
\arguments{
\item{excelFile}{name of the Excel file from which the R script is created}

\item{simulationTable}{Data.frame read from the Excel sheet "SimulationSets}

\item{workflowMode}{Either \code{PopulationWorkflow} or \code{MeanModelWorkflow}}
}
\value{
Character vector defining the \code{SimulationSet} objects
}
\description{
Creates a character vector to be written in a workflow .R script defining \code{SimulationSet} objects.
}
\keyword{internal}
