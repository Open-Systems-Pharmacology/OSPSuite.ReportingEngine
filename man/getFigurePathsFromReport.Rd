% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-writing-report.R
\name{getFigurePathsFromReport}
\alias{getFigurePathsFromReport}
\title{getFigurePathsFromReport}
\usage{
getFigurePathsFromReport(fileName)
}
\arguments{
\item{fileName}{name of .md file to}
}
\value{
array of file paths corresponding to figures linked in reports
}
\description{
Get file paths from a report figure links
}
\examples{
\dontrun{
# Check the figure paths of your report named "report.md"
getFigurePathsFromReport("report.md")
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
\code{\link{hasAnchor}()},
\code{\link{mergeMarkdownFiles}()},
\code{\link{renderReport}()},
\code{\link{renderWordReport}()},
\code{\link{resetReport}()}
}
\concept{reporting}
