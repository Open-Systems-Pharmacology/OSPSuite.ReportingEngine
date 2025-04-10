% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qualification-gof.R
\name{getQualificationGOFPlot}
\alias{getQualificationGOFPlot}
\title{getQualificationGOFPlot}
\usage{
getQualificationGOFPlot(
  plotType,
  data,
  metaData,
  axesProperties,
  plotProperties
)
}
\arguments{
\item{plotType}{Name of PK Parameter as defined by users}

\item{data}{data.frame with PK Ratios}

\item{metaData}{metaData with units and dimension for labeling the table header}

\item{axesProperties}{list of axes properties obtained from \code{getAxesProperties}}

\item{plotProperties}{list of plot properties defined in field \code{Plot} of GOFMerged configuration plan}
}
\value{
A ggplot object
}
\description{
Get goodness of fit plot
}
\keyword{internal}
