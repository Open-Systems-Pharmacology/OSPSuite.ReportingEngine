% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-qualification.R
\name{extractNameAndUnit}
\alias{extractNameAndUnit}
\title{extractNameAndUnit}
\usage{
extractNameAndUnit(text)
}
\arguments{
\item{text}{Character from which name and unit are extracted}
}
\value{
A named list, with fields \code{name} and \code{unit}.
}
\description{
Returns a named list with two entries (name, unit) corresponding to the name and unit
extracted out of the \code{text} provided as parameter
}
\examples{
\dontrun{
res <- extractNameAndUnit("Value [mg]")
res$name
# > "Value"
res$unit
# > "mg"

res <- extractNameAndUnit("Value")
res$name
# > "Value"
res$unit
# > ""
}
}
\keyword{internal}
