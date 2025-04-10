% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-plots.R
\name{autoAxesLimits}
\alias{autoAxesLimits}
\title{autoAxesLimits}
\usage{
autoAxesLimits(x, scale = tlf::Scaling$lin)
}
\arguments{
\item{x}{Values from which the limits are calculated}

\item{scale}{Name of the scale of the axis
Use helper enum \code{Scaling} from \code{tlf} package to find scales.}
}
\value{
A list of units for goodness of fit results
}
\description{
Defines auto axis limits
}
\keyword{internal}
