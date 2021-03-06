% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateExpand.R
\name{dateExpand}
\alias{dateExpand}
\title{Expands Durations Into Separate Dates.}
\usage{
dateExpand(df, startVar, endVar, eventVar)
}
\arguments{
\item{df}{data frame to transform}
}
\description{
Takes a data set with date-start and date-end, expands rows with duration
(eg. different start and end) into separate rows
allocates events randomly throughout constitutive dates.
Called in getTimeline
}
\examples{
expanded <- dateExpand(gedData) 
}
\keyword{dates}
\keyword{durations}
\keyword{ged}
