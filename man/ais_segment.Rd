% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_ais_segment.R
\name{ais_segment}
\alias{ais_segment}
\title{Create navigation tracks from AIS data.}
\usage{
ais_segment(df, time_interval = (300/60), distance = (50 * 1.61 * 1000))
}
\arguments{
\item{time_interval}{maximum time interval allowed between points, in hours}

\item{distance}{maximum distance allowed between points, in meters}

\item{ais}{csv of AIS positional data}
}
\description{
A function to create navigation tracks from AIS data based on maximum distance and maximum time interval between observations.
}
\details{
This function takes basic \code{.csv} files containing AIS observation data and creates line segments divided by individual boats (\code{MMSI} field) and by a user defined maximum time internal and distance between observations.
}
\examples{
# Example 1:
ais <- ais_segment(df)
}
\keyword{AIS}
\keyword{navigation}
