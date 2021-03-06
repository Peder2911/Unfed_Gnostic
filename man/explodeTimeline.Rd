% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explodeTimeline.R
\name{explodeTimeline}
\alias{explodeTimeline}
\title{Explodes a timeline of events-per-day into individual events}
\usage{
explodeTimeline(tl, countVar, dropCountVar = TRUE)
}
\arguments{
\item{tl}{timeline}
}
\description{
Explodes a timeline summary into individual events 
where each data point accounts for a single occurrence of the event count.
Useful for vizualising with geom_freqpoly and other stat_bin based geoms
}
\examples{
ged <- ged\%>\%
   getTimeline('1999-01-01','2000-01-01')
   explodeTimeline('deaths')
}
\keyword{dates}
