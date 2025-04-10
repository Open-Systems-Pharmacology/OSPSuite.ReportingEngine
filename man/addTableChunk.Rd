% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-writing-report.R
\name{addTableChunk}
\alias{addTableChunk}
\title{addTableChunk}
\usage{
addTableChunk(
  fileName,
  tableFileRelativePath,
  tableFileRootDirectory,
  digits = NULL,
  scientific = NULL,
  na = "-"
)
}
\arguments{
\item{fileName}{name of .md file}

\item{tableFileRelativePath}{path to table relative to working directory}

\item{tableFileRootDirectory}{working directory}

\item{digits}{number of decimal digits in displayed numbers}

\item{scientific}{logical defining if displayed numbers use scientific writing}

\item{na}{character string replacing \code{NA} values in table}
}
\description{
Add a table in a .md document
}
\seealso{
Other reporting: 
\code{\link{addFigureChunk}()},
\code{\link{addTextChunk}()},
\code{\link{anchor}()},
\code{\link{copyReport}()},
\code{\link{getAnchorName}()},
\code{\link{getFigurePathsFromReport}()},
\code{\link{hasAnchor}()},
\code{\link{mergeMarkdownFiles}()},
\code{\link{renderReport}()},
\code{\link{renderWordReport}()},
\code{\link{resetReport}()}
}
\concept{reporting}
