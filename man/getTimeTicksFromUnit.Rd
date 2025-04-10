% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-plots.R
\name{getTimeTicksFromUnit}
\alias{getTimeTicksFromUnit}
\title{getTimeTicksFromUnit}
\usage{
getTimeTicksFromUnit(unit, timeValues = NULL, maxTicks = 10)
}
\arguments{
\item{unit}{A time unit as defined in \code{ospsuite::ospUnits$Time}}

\item{timeValues}{Numeric values used by the data}

\item{maxTicks}{Maximum number of ticks allowed}
}
\value{
List of \code{ticks} and their \code{ticklabels}
}
\description{
Defines auto time ticks from time unit and time values
}
\keyword{internal}
