% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-goodness-of-fit.R
\name{getResidualsPlotResults}
\alias{getResidualsPlotResults}
\title{getResidualsPlotResults}
\usage{
getResidualsPlotResults(
  timeRange,
  residualsData,
  metaDataFrame,
  structureSet,
  settings = NULL
)
}
\arguments{
\item{timeRange}{array of time values defining range of simulated data}

\item{residualsData}{data.frame of residuals data}

\item{metaDataFrame}{metaData represented as a data.frame}

\item{structureSet}{A \code{SimulationStructure} object}

\item{settings}{Optional settings for the plots. In particular, includes reference data for population time profile.}
}
\value{
List of \code{plots}, their \code{captions} and \code{data} to export
}
\description{
Get plots and their captions for residuals
}
\keyword{internal}
