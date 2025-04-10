% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-writing-report.R
\name{resetReport}
\alias{resetReport}
\title{resetReport}
\usage{
resetReport(fileName)
}
\arguments{
\item{fileName}{name of .md file to reset}
}
\description{
Initialize a report, warning if a previous version already exists
}
\examples{
\dontrun{
resetReport("report.md")
}

}
\seealso{
Other reporting: 
\code{\link{addFigureChunk}()},
\code{\link{addTableChunk}()},
\code{\link{addTextChunk}()},
\code{\link{anchor}()},
\code{\link{copyReport}()},
\code{\link{getAnchorName}()},
\code{\link{getFigurePathsFromReport}()},
\code{\link{hasAnchor}()},
\code{\link{mergeMarkdownFiles}()},
\code{\link{renderReport}()},
\code{\link{renderWordReport}()}
}
\concept{reporting}
